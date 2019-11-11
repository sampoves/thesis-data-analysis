# -*- coding: utf-8 -*-
"""
Created on Fri May 10 00:07:36 2019

@author: Sampo Vesanen

Sampo Vesanen Thesis survey datacrunch
"Parking of private cars and spatial accessibility in Helsinki Capital Region"

TODO:
    - Geographic analyses (Can't get geoplot to work)
    - Detect outliers

Important questions etc
    - Respondent specific reports (probably not useful)
    - fix crs mismatches
    - See how long user took to first visit survey and answer to the survey
    - Have same IPs sent records for same areas more than once?
"""

import os
import pandas as pd
import geopandas as gpd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.pyplot import figure
from datetime import timedelta
from shapely.geometry import Polygon, MultiPolygon, Point, LinearRing
import scipy.stats as stats
from scipy.stats import levene
from statsmodels.formula.api import ols 
import statsmodels.api as sm
#import mapclassify #scheme="fisher_jenks" needs mapclassify


# Working directories
wd = r"C:\Sampon\Maantiede\Master of the Universe\python"
datawd = r"C:\Sampon\Maantiede\Master of the Universe\leaflet_survey_results"

# Survey data file paths
records_data = os.path.join(datawd, "records.csv")
visitors_data = os.path.join(datawd, "visitors.csv")

os.chdir(wd)

# Import functions. Has to be done after inserting os.chdir()
from thesis_data_analysis_funcs import *



###################
### IMPORT DATA ###
###################

# Survey data. Lambda x to preserve leading zeros
records = pd.read_csv(records_data, converters={'zipcode': lambda x: str(x)})
visitors = pd.read_csv(visitors_data)

# Shapefiles
grid = gpd.read_file("MetropAccess_YKR_grid_EurefFIN.shp", encoding='utf-8')
resarea = gpd.read_file("paavo\pno_dissolve.shp", encoding='utf-8')

# import urban zones shapefile and select only relevant area for this study
ykr_vyoh = gpd.read_file(
        r"C:\Sampon\Maantiede\Master of the Universe\yhdyskuntarakenteenvyoh17\YKRVyohykkeet2017.shp", 
        encoding='utf-8')
ykr_vyoh = gpd.overlay(ykr_vyoh, 
                       gpd.GeoDataFrame(geometry=resarea.buffer(500)), 
                       how="intersection")

# From Urban Atlas 2012 preserve only forests. Select only relevant area for
# this study
forest = gpd.read_file(r"FI001L3_HELSINKI\ua2012_research_area.shp", 
                       encoding="utf-8")
forest = forest[forest["ITEM2012"] == "Forests"]



###########################
### PREPARE SOURCE DATA ###
###########################

# Remove column "Unnamed: 0"
records = records.drop(columns=["Unnamed: 0"])
visitors = visitors.drop(columns=["Unnamed: 0"])

# Timestamps to datetime 
records["timestamp"] = convertToDatetime(records, "timestamp")
visitors["ts_first"] = convertToDatetimeVar2(visitors, "ts_first")
visitors["ts_latest"] = convertToDatetimeVar2(visitors, "ts_latest")

# visitors, records: remove author's visits and three records inputted by
# the author. I have visited the survey from university, work and mobile
# phone and these will be impossible to trace down.
records = records.drop([0, 5, 6])

# I was contacted on 24.5.2019 22.41 that a respondent had filled three 
# zipcodes erroneously: Kallio, Käpylä and one other (probably Pukinmäki-Savela).
# The following removes those records.
records = records.drop([1277, 1278, 1279])

# author's home computer visits
visitors = visitors.iloc[1:] 

 # 82.102.24.252, a confirmed VPN visit by author
visitors = visitors.drop([3753])



##########################
### PROCESS SHAPEFILES ###
##########################

# Process Helsinki Capital Region geodataframe from PAAVO data
muns = ["091", "049", "092", "235"]
postal = gpd.read_file(r"paavo\2019\pno_tilasto_2019.shp", encoding="utf-8")
postal = postal[postal.kunta.isin(muns)]
postal = postal.reset_index().drop(columns=["index", "euref_x", "euref_y"])

# In the traditional travel time matrix all parking took 0.42 minutes. The
# team used the bounding box below to define an area in center of Helsinki
# to note an area where people walk a longer time to their cars
walkingBbox = LinearRing([(387678.024778, 6675360.99039), 
                          (387891.53396, 6670403.35286),
                          (383453.380944, 6670212.21613), 
                          (383239.871737, 6675169.85373),
                          (387678.024778, 6675360.99039)])


### Remove islands unreachable by car

# Preserve two largest Polygons from these postal code areas
preserveTwoLargest = ["00830", "00570", "00340", "00200", "00890"]

# Preserve largest includes island postal code areas
preserveLargest = ["00210", "00860", "00840", "00850", "00870", "00590"]

# Suvisaaristo needs special attention. Define Polygon
suvisaar = Polygon([(371829.79632436, 6668010.26942102), 
                    (372652.78908073, 6667612.58164613), 
                    (371343.73348838, 6666204.10411005), 
                    (372923.43770531, 6665381.11135368), 
                    (374635.70451388, 6667452.40184791), 
                    (372078.35118367, 6668800.12152949), 
                    (371829.79632436, 6668010.26942102)])

# Get mainland
mainland = max(postal.unary_union, key=lambda a: a.area)

# Process islands unreachable by car
for idx, geom in enumerate(postal.geometry):
    
    # "in" utilises operator
    if postal.loc[idx, "posti_alue"] in preserveTwoLargest:
        thisGeom = postal.loc[idx]
        geomList = [(idx, P.area) for idx, P in enumerate(thisGeom.geometry)]
        geomList = geomList[:2] # two largest
        geomList = [idx for idx, area in geomList] # preserve ids
        # get two largest Polygons in MultiPolygon in current thisGeom
        thisGeom = MultiPolygon(
                [geom for idx, geom in enumerate(thisGeom.geometry) if idx in geomList])
        mainland = mainland.union(thisGeom)
    
    # Islands
    if postal.loc[idx, "posti_alue"] in preserveLargest:
        if geom.geom_type == "MultiPolygon":
            largest = max(geom, key=lambda a: a.area)
        else:
            largest = geom
        mainland = mainland.union(largest)
    
    # Special case Tapiola, preserve entire MultiPolygon
    if postal.loc[idx, "posti_alue"] == "02100":
        thisGeom = postal.loc[idx, "geometry"]
        mainland = mainland.union(thisGeom)
        
    # Special case Suvisaaristo
    if postal.loc[idx, "posti_alue"] == "02380":
        match = postal.loc[idx, "geometry"]
        preserve = MultiPolygon(
                [geom for geom in match if geom.intersects(suvisaar)])
        mainland = mainland.union(preserve)
    
    # Special case Kuusisaari-Lehtisaari, preserve 3 largest Polygons
    if postal.loc[idx, "posti_alue"] == "00340":
        thisGeom = postal.loc[idx]
        geomList = [(idx, P.area) for idx, P in enumerate(thisGeom.geometry)]
        geomList = geomList[:3] # three largest
        geomList = [idx for idx, area in geomList] # preserve ids
        thisGeom = MultiPolygon(
                [geom for idx, geom in enumerate(thisGeom.geometry) if idx in geomList])
        mainland = mainland.union(thisGeom)

# Replace postal geometries with geometries without islands
# NB! postal.at[idx, "geometry"] = thisGeom seems to raise ValueError in
# GeoPandas 0.6.0. Complains that "Must have equal len keys and value when 
# setting with an iterable"
for idx, geom in enumerate(postal.geometry):
    if geom.geom_type == "MultiPolygon":
        thisGeom = MultiPolygon(
                [pol for idx, pol in enumerate(geom) if pol.intersects(mainland.buffer(-10))])
        if thisGeom.is_empty == False:
            postal.at[idx, "geometry"] = thisGeom
            



################################
### GIVE GRID CELLS ZIPCODES ###
################################

# newest test, RTREE ENABLED
# SEEMS TO WORK SOMEHOW, BUT WHY 16000 ROWS?! MORE THAN GRID 13231
# GOT IT; BECAUSE THE BORDERS OF ZIPCODES ARE AMBIGUOUS, this code does not
# test for to which zipcode it should belong to, probably records two zipcodes
# for one ykr-id 
from rtree import index

# Save crs to be used here
crs = grid.crs

# Get mainland envelope, then symmetric diffence to produce exteriors MultiPolygon
exteriors = mainland.envelope.buffer(1000)
exteriors = gpd.overlay(gpd.GeoDataFrame(geometry=[exteriors]), 
                        gpd.GeoDataFrame(geometry=[mainland]), 
                        how="symmetric_difference")
exteriors.crs = crs

# List to collect pairs of intersecting features
fc_intersect = []

# Instantiate index class
idx = index.Index()
for i, featA in grid.iterrows():
    idx.insert(i, featA["geometry"].bounds)

# This runs for 8 minutes! 20 000 results. Wrong. Investigate
for i2, featB in postal.iterrows():
    
    print("\nCurrent zipcode is {0}, {1}".format(
            featB.loc["posti_alue"], featB.loc["nimi"]))

    # Test for potential intersection with each feature of the other feature 
    # collection
    for intersect_maybe in idx.intersection(featB["geometry"].bounds):
        # Confirm intersection

        if featB["geometry"].intersects(grid.loc[intersect_maybe]["geometry"]):
            
            print("-- Intersection found in grid cell {0}, YKRID {1}".format(
                    intersect_maybe,
                    grid.loc[intersect_maybe]["YKR_ID"]))
                
            # create mainland without the current zipcode. Use it to test if
            # there are neighboring zipcodes in current grid cell
            reduced_mainland = gpd.overlay(gpd.GeoDataFrame(geometry=[mainland]),
                                           gpd.GeoDataFrame(geometry=[featB.geometry]),
                                           how="symmetric_difference")
            
            # Test current grid cell for presence of area which is not research 
            # area (sea, neighboring municipalities)
            if grid.loc[intersect_maybe]["geometry"].intersects(
                    exteriors.geometry[0]):
                
                print("---- grid cell intersects with exterior")
                
                # this code is needed because there are pieces of coast line
                # there one cell is shared by two zipcodes

                
                # thisGridCell is the entire current grid cell which
                # intersects current zipcode area. It is inevitably shaped like
                # a square and may contain exterior areas
                thisGridCell = grid.loc[[intersect_maybe]]
                
                # thisCellPiece is the current piece of grid in the current
                # zipcode area. Does not include exteriors of other zip code
                # areas
                thisCellPiece = gpd.overlay(
                        thisGridCell,
                        gpd.GeoDataFrame(geometry=[featB.geometry]),
                        how="intersection")
                
                # cellMinusExt is the entire current grid cell with 
                # all possible zipcode areas but does not include exterior 
                # areas. Use these to test if current piece of grid cell in 
                # current zipcode area is the largest feature
                cellMinusExt = gpd.overlay(thisGridCell, exteriors, 
                                           how="difference")
                
                # If the area of cellPieceArea is smaller than half of the
                # entire gridCellArea, do not accept that as result. Move on.
                cellMinusExtArea = 0 if cellMinusExt.empty == True else cellMinusExt.area.values[0]
                cellPieceArea = 0 if thisCellPiece.empty == True else thisCellPiece.area.values[0]
                
               
                if cellPieceArea / cellMinusExtArea > 0.5:
                    fc_intersect.append(
                            [grid.loc[intersect_maybe].YKR_ID, featB.posti_alue])
                    next
                
                # conclusion. Next cycle
                else:
                    print("------ grid cell ")
                    next
              
            # Test for presence of other zipcode areas in current grid cell
            if grid.loc[intersect_maybe]["geometry"].intersects(
                    reduced_mainland.geometry[0]):
                
                print("---- grid cell intersects with another zipcode area")
                
                # thisGridCell is the entire current grid cell which
                # intersects current zipcode area. It is inevitably shaped like
                # a square and may contain exterior areas
                thisGridCell = grid.loc[[intersect_maybe]]
                
                # thisCellPiece is the current piece of grid in the current
                # zipcode area. Does not include exteriors of other zip code
                # areas
                thisCellPiece = gpd.overlay(
                        thisGridCell,
                        gpd.GeoDataFrame(geometry=[featB.geometry]),
                        how="intersection")
                
                # If the area of cellPieceArea is smaller than half of the
                # entire gridCellArea, do not accept that as result. Move on.
                gridCellArea = 0 if thisGridCell.empty == True else thisGridCell.area.values[0]
                cellPieceArea = 0 if thisCellPiece.empty == True else thisCellPiece.area.values[0]
                
                if cellPieceArea / gridCellArea > 0.5:
                    fc_intersect.append(
                            [grid.loc[intersect_maybe].YKR_ID, featB.posti_alue])
                    next
                
                #conclusion
                else:
                    next
                
            print("---- Grid cell completely within zipcode area")
                
            # Default course of action, grid cell completely within zipcode area
            fc_intersect.append(
                    [grid.loc[intersect_maybe].YKR_ID, featB.posti_alue])

# 20000 results, why
#test
testdf = pd.DataFrame.from_records(fc_intersect)
testdf.columns = ["ykr", "count"]
grouped = testdf.groupby(["ykr"]).agg({"ykr": "count"})






# teesttt
plot_polygon([mainland, thisGridCell, cellAllZips, cellMinusExt])
plot_polygon([cellAllZips, thisCellPiece, cellMinusExt])


# current piece of this zipcode
postal.loc[143, "geometry"].intersects(grid.loc[13105]["geometry"])
pelle = gpd.overlay(postal.loc[[143]], grid.loc[[13105]], 
                    how="intersection")

# the entire piece of land in this grid cell, can be of many zipcodes
maapala = gpd.overlay(grid.loc[[13105]], exteriors, how="difference")
plot_polygon([maapala, pelle])








# TEST 2000

from shapely.geometry import shape

# convert to lists of shapely geometries
zipcodes = [shape(poly) for poly in postal.geometry]
gridcells = [shape(poly) for poly in grid.geometry]

# intersections with references to the original polygon layer
for i, poly in enumerate(zipcodes):
    for cell in gridcells:
        if poly.intersects(cell):
            print("pf")
            #print poly_layer[i]


# MEGA SJOIN
# sjoin could work

# all intersections, still problems with partitions
delCol = postal.columns[2:106]
intersections = gpd.sjoin(grid, postal.drop(columns=delCol), how="inner", op="intersects")
grouped = intersections.groupby(["YKR_ID"]).agg({"YKR_ID": "count"})

# test more
# exterior border
ext_ints = gpd.sjoin(grid,
                     gpd.GeoDataFrame(crs=crs, geometry=[mainland.boundary]),
                     how="inner", op="intersects")

# all borders of zipcode areas, including exterior borders
border_ints = gpd.sjoin(grid, 
                        gpd.GeoDataFrame(geometry=postal.boundary.geometry),
                        how="inner", op="intersects")

# both is all zipcode borders without exterior borders
both = gpd.overlay(border_ints, ext_ints, how="difference")



# NEWTEST 03030303030
# https://gis.stackexchange.com/a/227433/122733
for i, ziparea in postal.iterrows():
    
    thisZip = gpd.GeoDataFrame(crs=crs, geometry=[ziparea.geometry])
    these = gpd.sjoin(grid, thisZip, how="inner", op="intersects")
    
    # test if current ziparea intersects areas outside research area
    if ziparea.geometry.intersects(exteriors.geometry[0]):
        print("klfds")
        
        
        testt = gpd.sjoin(grid, thisZip, how="inner", op="intersects")

    
## newer test 20042334    

# test postal area boundaries with grid cells. test exterior boundaries and
# zipcode boundaries. maybe keep track which ykrids have been checked out

#hidas
ple = gpd.GeoDataFrame(crs=crs, geometry=[postal.boundary.unary_union])
testf = gpd.sjoin(grid, ple, how="inner", op="intersects")
    
    












# THIS WORKS! is far as I have testeddd
### TAAS UUS TEEST
from rtree import index

# kirjoita muistiin, että mikä ykr-koodi on saanu minkäkin aluesumman
# col (total area of zipcode) <-- tässä kaikki maa, tärkeä reunapolygoneille
# col (area of current iteration in current zipcode) <-- käy sit vaikka monta
# kertaa kaikki läpi, jotta löytyy suurin prosentuaalinen osuus cellille

# try with sjoin
# exterior border
#crs = grid.crs
#exteriorInter = gpd.sjoin(grid,
#                          gpd.GeoDataFrame(crs=crs, geometry=[mainland.boundary]),
#                          how="inner", op="intersects")

# all borders of zipcode areas, including exterior borders
#borderInter = gpd.sjoin(grid, 
#                        gpd.GeoDataFrame(geometry=postal.boundary.geometry),
#                        how="inner", op="intersects")

# InteriorborderInter is all zipcode borders without exterior borders
#interiorBorderInter = gpd.overlay(borderInter, exteriorInter, how="difference")

#plot_polygon([both])
#plot_polygon([borderInter, exteriorInter])
#plot_polygon([exteriorInter])


# Instantiate index class
grid_index = index.Index()
for idx, cell in grid.iterrows():
    grid_index.insert(idx, cell["geometry"].bounds)

#int_index = index.Index()
#for idx, cell in interiorBorderInter.iterrows():
#    int_index.insert(idx, cell["geometry"].bounds)


# initiate listing for grid cells
grid_areas = gpd.GeoDataFrame(columns=["ykr", "zipcode", "largest_area", 
                                       "total_area"])

# kind of works?! 12 000 results in maybe 6 minutes
i = 0
for postal_idx, postalarea in postal.iterrows():
    print("now processing {0}".format(postalarea.nimi))
    
    # find candidates for intersection
    for intersection_maybe in grid_index.intersection(postalarea["geometry"].bounds):  
            
        # Confirmation of intersection
        if postalarea.geometry.intersects(grid.loc[intersection_maybe]["geometry"]):
            
            # with interiorBorderInter we know that the grid cell piece is
            # not a full square
            thisCellPiece = gpd.overlay(
                    gpd.GeoDataFrame(geometry=[grid.loc[intersection_maybe]["geometry"]]),
                    gpd.GeoDataFrame(geometry=[postalarea.geometry]),
                    how="intersection")
            
            # current grid cell area
            cellarea = thisCellPiece.area[0]
            
            # test if cell ykr-id already exists in grid_areas
            grid_ykr = grid.loc[intersection_maybe].YKR_ID
            if grid_areas[grid_areas["ykr"].isin([grid_ykr])].empty == True:
                
                # this YKR_ID does not yet exist
                grid_areas.loc[i, "ykr"] = grid.loc[intersection_maybe].YKR_ID
                grid_areas.loc[i, "zipcode"] = postalarea.posti_alue
                grid_areas.loc[i, "largest_area"] = cellarea
                grid_areas.loc[i, "total_area"] = cellarea
                i += 1
                
            #if grid_areas.YKR_ID == grid.loc[intersection_maybe].YKR_ID:
            else:   
                # this is the location of the ykr_id
                found_idx = grid_areas.index[grid_areas.ykr == grid.loc[intersection_maybe].YKR_ID][0]
                
                # in this case the ykr id has been iterated over in some previous
                # iteration of a postal area. Only add to largest_area if larger
                # than before and add to total_area
                if cellarea > grid_areas.loc[found_idx, "largest_area"]:
                    grid_areas.loc[found_idx, "largest_area"] = cellarea
                    grid_areas.loc[found_idx, "zipcode"] = postalarea.posti_alue
                    
                grid_areas.loc[found_idx, "total_area"] += cellarea


grid_areas.rename(columns={"ykr": "YKR_ID"}, inplace=True)
grid[~grid["YKR_ID"].isin(grid_areas["YKR_ID"].tolist())]

# here we see that ykr ids which were missing from grid_areas are ones which
# are completely outside PAAVO zipcode delineation
plot_polygon([mainland, grid[~grid["YKR_ID"].isin(grid_areas["YKR_ID"].tolist())]])
plot_polygon([grid, grid[~grid["YKR_ID"].isin(grid_areas["YKR_ID"].tolist())]])


       
     
            







# List to collect pairs of intersecting features
fc_intersect = []

# Instantiate index class
grid_index = index.Index()
for idx, featA in grid.iterrows():
    grid_index.insert(idx, featA["geometry"].bounds)



for idx, postalarea in postal.iterrows():
    
    # Here rtree index only iterates through grid cells which are located 
    # inside the bounds of current postalarea. We get a list of potential
    # intersections in this area
    for intersect_maybe in grid_index.intersection(postalarea["geometry"].bounds):        
        
        # Confirm intersection
        if postalarea["geometry"].intersects(grid.loc[intersect_maybe]["geometry"]):
            
            if round(grid.loc[intersect_maybe]["geometry"].area) == 62500:
                
                # append data that this cell is guaranteed full cell.
                # NOTE, all full grid cells
                fc_intersect.append(
                        [grid.loc[intersect_maybe].YKR_ID, 
                         postalarea.posti_alue])
                
                # found full, delete from index
                grid_index.delete(intersect_maybe, 
                                  grid.loc[intersect_maybe]["geometry"].bounds)
                
    break


testdf = pd.DataFrame.from_records(fc_intersect)
grid[grid["YKR_ID"].isin(testdf[0])].plot() #yes in testdf
grid[~grid["YKR_ID"].isin(testdf[0])].plot() #not in testdf







############################
### RESPONDENT BEHAVIOUR ###
############################

# Create groupby aggregations of the records.
visitor_grp = records.groupby(["ip"]).agg({
        "id": "count", 
        "timestamp": lambda x: x.tolist(),
        "zipcode": lambda x: x.tolist(),
        "likert": lambda x: x.tolist(),
        "parkspot": lambda x: x.tolist(),
        "parktime": lambda x: x.tolist(),
        "walktime": lambda x: x.tolist(),
        "timeofday": lambda x: x.tolist()})

    
        
#################################
### DETECTION OF ILLEGAL DATA ###
#################################

# Detect if a user has answered a same area multiple times. Please note that 
# this only locates duplicates but does nothing about it.
for visitor in records.ip.unique():
    theseRecords = records.loc[records['ip'] == visitor]
    
    if(theseRecords["zipcode"].is_unique) == False:
        print("\nDuplicates detected for ip code {0}".format(
                theseRecords.ip.unique()[0]))
        dupl = theseRecords[theseRecords.zipcode.duplicated(keep=False)]
        dupl = dupl.groupby(["zipcode"]).agg(
                {"id": lambda x: x.tolist(),
                 "timestamp": lambda x: x.tolist(), 
                 "ip": "first",
                 "zipcode": "first", 
                 "likert": lambda x: identicaltest(x), # defined in funcs.py
                 "parkspot": lambda x: identicaltest(x),
                 "parktime": lambda x: identicaltest(x), 
                 "walktime": lambda x: identicaltest(x), 
                 "timeofday": lambda x: identicaltest(x)})
    
        # Produce report
        for idx, row in dupl.drop(["ip"], axis=1).iterrows():
            print(row.to_string(), "\n") # suppress showing of dtype



# Remove illegal answers. At this time any value equal or above 60 minutes is 
# deemed illegal. Value invalid starts at 6 as at this point I have removed 3 
# of my own records and 3 others which were reported to me as false. See above.
delList = []
illegal = []
invalid = 6

for idx, (park_value, walk_value, ip_value) in enumerate(
        zip(records["parktime"], records["walktime"], records["ip"])):
    
    if (park_value >= 60 or walk_value >= 60):
        print("Illegal walktime or parktime detected:", idx, park_value, 
              walk_value)
        delList.append(idx)
        illegal.append(ip_value)
        invalid += 1
 
# Illegal contains all IPs which have values equal or above 60 as answers. 
# illegal_df shows all records made by these IP addresses
illegal = list(set(illegal))
illegal_df = records[records["ip"].isin(illegal)]

# Drop the records which have values equal or above 60 as answer. Drop the IP 
# addresses from visitors as well.
records = records.drop(records.index[delList])
records = records.reset_index()
visitors = visitors[~visitors["ip"].isin(illegal)]
visitors = visitors.reset_index()



#################################
### ADD DATA TO GEODATAFRAMES ###
#################################

# Reclassify ykr_vyoh. Reclassification is created as in the SYKE website
#https://www.ymparisto.fi/fi-FI/Elinymparisto_ja_kaavoitus/Yhdyskuntarakenne/Tietoa_yhdyskuntarakenteesta/Yhdyskuntarakenteen_vyohykkeet
ykr_vyoh["vyohselite"] = ykr_vyoh["vyohselite"].replace(
        {"keskustan reunavyöhyke/intensiivinen joukkoliikenne": 
            "keskustan reunavyöhyke", 
         "keskustan reunavyöhyke/joukkoliikenne": 
             "keskustan reunavyöhyke",
         "alakeskuksen jalankulkuvyöhyke/intensiivinen joukkoliikenne": 
             "alakeskuksen jalankulkuvyöhyke",
         "alakeskuksen jalankulkuvyöhyke/joukkoliikenne": 
             "alakeskuksen jalankulkuvyöhyke"})

# add ykr_vyoh columns
postal["ykr_kesk_jalan"] = 0
postal["ykr_kesk_reuna"] = 0
postal["ykr_int_joukko"] = 0
postal["ykr_joukkoliik"] = 0
postal["ykr_alakesk_jalan"] = 0
postal["ykr_autovyoh"] = 0
postal["ykr_novalue"] = 0

# add Urban Atlas 2012 forest amount
postal["ua_forest"] = 0

# Add column "answer count", "parktime mean", and "walktime mean"
postal["answer_count"] = 0
postal["parktime_mean"] = 0
postal["walktime_mean"] = 0

# Calculate answer count for each zipcode area
for zipcode in records.zipcode:
    for idx, postal_zip in enumerate(postal.posti_alue):
        if postal_zip == zipcode:
            postal.loc[idx, "answer_count"] += 1

# Calculate parktime mean to postal.parktime_mean
for idx, postal_zip in enumerate(postal.posti_alue):
    this_mean = records.loc[
            records['zipcode'] == postal_zip]["parktime"].mean()
    postal.loc[idx, "parktime_mean"] = round(this_mean, 2)

# Calculate walktime mean to postal.walktime_mean
for idx, postal_zip in enumerate(postal.posti_alue):
    this_mean = records.loc[
            records['zipcode'] == postal_zip]["walktime"].mean()
    postal.loc[idx, "walktime_mean"] = round(this_mean, 2)

# Prepare annotation of plot features
postal['coords'] = postal['geometry'].apply(lambda x: x.representative_point().coords[:])
postal['coords'] = [coords[0] for coords in postal['coords']]

# population density
#postal["pop_dens"] = postal.apply(
#        lambda row: round(row["he_vakiy"]/(row["pinta_ala"]/1000000), 2), axis=1)



###################################
### PREPARE AND SHOW STATISTICS ###
###################################
    
statistics = Stats(records, postal, visitors, invalid)
statistics.calculateStats()



#####################################################################
### Set percentage of urban zones and forest in each zipcode area ###
#####################################################################

# Idea is to overlay yhdyskuntarakenteen vyöhykkeet on dataframe postal and see 
# the percentage how much of each zone is included in the zipcode in question. 
# Together with checking out how much forest is there in research area we could 
# find some interesting results.

# YKR zones

# Dictionary key to help allocation of values
dictKey = {"keskustan jalankulkuvyöhyke": "ykr_kesk_jalan", 
           "keskustan reunavyöhyke": "ykr_kesk_reuna", 
           "intensiivinen joukkoliikennevyöhyke": "ykr_int_joukko",
           "joukkoliikennevyöhyke": "ykr_joukkoliik", 
           "alakeskuksen jalankulkuvyöhyke": "ykr_alakesk_jalan", 
           "autovyöhyke": "ykr_autovyoh"}

# geom_grouper prevents multiple instances of same zones in the dict
geom_grouper = lambda x: x.unary_union

for row in postal.iterrows():
    
    # row is tuple, change that to have better readability
    row = row[1]
    
    # Intersect current Polygon with urban zones shapefile
    thisPol = gpd.GeoDataFrame(geometry=[row.geometry])
    thisIntersect = gpd.overlay(thisPol, ykr_vyoh, how="intersection")
    
    # Groupby ensures each zone appears only once
    group = thisIntersect.groupby("vyohselite", as_index=False)["geometry"].agg(
            {"geometry": geom_grouper})
    
    # This calculates Polygon areas and informs us how indices have to be ordered
    # to get sorted values
    area_order = list(
            group["geometry"].map(lambda p: p.area).sort_values(ascending=False).index)
    group = group.reindex(area_order)
    group = group.reset_index()
    
    # Insert this dict into all zipcode rows
    thisDict = [{
        thisRow[1].vyohselite: 
            round(thisRow[1].geometry.area / thisPol.unary_union.area, 3)
        } for thisRow in group.iterrows()]
    
    # Print report
    #print("{0} {1} \n{2}\n".format(row.posti_alue, row.nimi, thisDict))
    
    # iterate thisDict, list of dictionaries to insert values in postal
    for item in thisDict:
        for k, v in item.items():
            
            # Add data to postal. In .loc square brackets select correct row of 
            # placement then use dictKey dictionary to tell which column to 
            # append in. Finally append current value v
            postal.loc[postal["posti_alue"] == row.posti_alue, dictKey[k]] = v
    
# Calculate ykr_novalue
postal["ykr_novalue"] = postal.apply(
        lambda row: 1 - (row.ykr_kesk_jalan + row.ykr_kesk_reuna +
        row.ykr_int_joukko + row.ykr_joukkoliik + 
        row.ykr_alakesk_jalan + row.ykr_autovyoh), axis=1)

# Make sure rounding does not cause trouble in the calculation above
postal["ykr_novalue"].loc[postal["ykr_novalue"] < 0] = 0

# Urban Atlas 2012 forest

# Reproject the geometries by replacing the values with projected ones
forest["geometry"] = forest["geometry"].to_crs(epsg=3067)

# Iterate over postal areas and then intersect with forest layer
for row in postal.iterrows():
    row = row[1]
    thisPol = gpd.GeoDataFrame(geometry=[row.geometry])
    thisIntersect = gpd.overlay(thisPol, forest, how="intersection")
    if thisIntersect.empty == False:
        forestpercent = thisIntersect.unary_union.area / thisPol.area
        
        # print report
        #print("zipcode {0} {1} is {2} % forest".format(
        #        row.posti_alue, row.nimi, 
        #        round(forestpercent[0], 3) * 100))
        
        # append forest data to column "ua_forest"
        postal.loc[postal["posti_alue"] == row.posti_alue, "ua_forest"] = round(
                forestpercent[0], 3)
    #else:
    #    # print forestless report
    #    print("zipcode {0} {1} is forestless".format(row.posti_alue,
    #          row.nimi))


#########################################
# BRING YKR ZONES AND FOREST TO RECORDS #
#########################################

# See generalised value of ykr_vyoh and forest in records for further
# statistical analysis

# Bring data
# zipcode and posti_alue different names. Clumsily rename and name back
# posti_alue to zipcode to prevent breaking of code
postal = postal.rename(columns={"posti_alue": "zipcode"})
records = pd.merge(records, postal[["zipcode","ua_forest"]], on="zipcode")
postal = postal.rename(columns={"zipcode": "posti_alue"})
    
# Calculate jenks breaks for ua_forest. Use breaks to reclassify values
# in records. We will use code created by GitHub user Drewda. This is now
# commented because calculation time takes about 20 seconds
#breaks = getJenksBreaks(records["ua_forest"].tolist(), 5)

# See the breaks on plot
#plt.figure(figsize = (10, 8))
#hist = plt.hist(records["ua_forest"], bins=20, align="left", color="g")
#for b in breaks:
#    plt.vlines(b, ymin=0, ymax = max(hist[0]))

# Reclassify using this clumsy nested np.where(). Use values calculated in
# getJenksBreaks()
records["ua_forest"] = np.where(
        records["ua_forest"] < 0.062,
        "Scarce forest", 
        (np.where(records["ua_forest"] < 0.167,
                  "Some forest", 
                  (np.where(records["ua_forest"] < 0.288, 
                            "Moderate forest", 
                            (np.where(records["ua_forest"] < 0.495, 
                                      "Mostly forest", 
                                      (np.where(records["ua_forest"] < 1, 
                                                "Predominantly forest",
                                                "novalue"))))))))).tolist()

# Use largest percentage for a class in each zipcode!
ykrColumns = ["ykr_alakesk_jalan", "ykr_autovyoh", "ykr_int_joukko", 
        "ykr_joukkoliik", "ykr_kesk_jalan", "ykr_kesk_reuna", "ykr_novalue"]

# Create dataframe with postal codes and reclassified ykr zones. idxmax will
# fetch us the names of the columns where largest value of each row resides
largestYkr = postal[["posti_alue"]].join([postal[ykrColumns].idxmax(axis=1)])
largestYkr = largestYkr.rename(columns={"posti_alue": "zipcode",
                                        0: "ykr_zone"})
    
# Merge will spread values of column ykr_zone over all occurrences of specific
# zipcodes
records = pd.merge(records, largestYkr, on="zipcode")

# Rename ykr zone records to human readable versions
for idx, row in enumerate(records.iterrows()):
    
    for realname, abbr in dictKey.items():
        
        if row[1].ykr_zone == abbr:
            records.loc[idx, "ykr_zone"] = realname
        
        elif row[1].ykr_zone == "ykr_novalue":
            records.loc[idx, "ykr_zone"] = "novalue"
        
    

################################
### Divisions by postal code ###
################################

# Some postal code areas contain localities which are in conflict with the
# subdivisions. For example, zipcode Lippajärvi-Järvenperä 02940 contains
# localities Högnäs (belongs to Vanha-Espoo), Lippajärvi (belongs to Suur-
# Leppävaara). In these cases I will use my own deliberation for
# classification. The main problem areas are just south and north of Kauniainen
# (for example Lippajärvi-Järvenperä and Sepänkylä-Kuurinniitty) and just 
# south of Helsinki-Vantaa airport.

# Helsinki
# Piirijako
# https://www.avoindata.fi/data/fi/dataset/helsinki-alueittain/resource/9e197c6a-1882-4ad9-a50b-9dc7c49cb75a

# Southern subdivision
hkiSouth = ["00250", # Taka-Töölö 
            "00260", # Keski-Töölö
            "00100", # Helsinki keskusta - Etu-Töölö
            "00180", # Kamppi-Ruoholahti
            "00200", # Lauttasaari
            "00210", # Vattuniemi
            "00220", # Jätkäsaari
            "00120", # Punavuori
            "00130", # Kaartinkaupunki
            "00140", # Kaivopuisto-Ullanlinna
            "00160", # Katajanokka
            "00150", # Eira-Hernesaari
            "00190", # Suomenlinna
            "00170"] # Kruununhaka
#postal[postal.posti_alue.isin(hkiSouth)].plot()

# Western subdivision
hkiWest = ["00290", # Meilahden sairaala-alue
           "00270", # Pohjois-Meilahti
           "00280", # Ruskeasuo
           "00330", # Munkkiniemi
           "00350", # Munkkivuori-Niemenmäki
           "00310", # Kivihaka
           "00320", # Etelä-Haaga
           "00340", # Kuusisaari-Lehtisaari
           "00400", # Pohjois-Haaga
           "00380", # Pitäjänmäen teollisuusalue
           "00360", # Pajamäki
           "00370", # Reimarla
           "00440", # Lassila
           "00390", # Konala
           "00410", # Malminkartano 
           "00420", # Kannelmäki
           "00430", # Maununneva
           "00300"] # Pikku Huopalahti
#postal[postal.posti_alue.isin(hkiWest)].plot()

# Central (Keskinen) subdivision
hkiCentral = ["00230", # Ilmala 
             "00240", # Länsi-Pasila 
             "00610", # Käpylä 
             "00600", # Koskela 
             "00560", # Toukola-Vanhakaupunki 
             "00550", # Vallila 
             "00520", # Itä-Pasila
             "00510", # Etu-Vallila-Alppila
             "00500", # Sörnäinen
             "00530", # Kallio
             "00540", # Kalasatama
             "00580"] # Verkkosaari
#postal[postal.posti_alue.isin(hkiCentral)].plot()

# Norther subdivision
hkiNorth = ["00690", # Tuomarinkylä-Torpparinmäki 
            "00670", # Paloheinä
            "00660", # Länsi-Pakila
            "00680", # Itä-Pakila
            "00630", # Maunula-Suursuo
            "00620", # Metsälä-Etelä-Oulunkylä
            "00650", # Veräjämäki
            "00640"] # Oulunkylä-Patola
#postal[postal.posti_alue.isin(hkiNorth)].plot()

# Northeastern subdivision
hkiNortheast = ["00740", # Siltamäki
                "00750", # Puistola
                "00760", # Suurmetsä
                "00780", # Tapaninvainio
                "00730", # Tapanila
                "00770", # Jakomäki-Alppikylä
                "00720", # Pukinmäki-Savela
                "00710", # Pihlajamäki
                "00790", # Viikki
                "00700"] # Malmi
#postal[postal.posti_alue.isin(hkiNortheast)].plot()

# Southeastern subdivision
hkiSoutheast = ["00800", # Länsi-Herttoniemi 
                "00880", # Roihupellon teollisuusalue
                "00820", # Roihuvuori
                "00810", # Herttoniemi
                "00830", # Tammisalo                
                "00570", # Kulosaari
                "00590", # Kaitalahti
                "00870", # Etelä-Laajasalo
                "00850", # Jollas
                "00860", # Santahamina                
                "00840"] # Laajasalo
#postal[postal.posti_alue.isin(hkiSoutheast)].plot()

# Eastern subdivision
hkiEast = ["00940", # Kontula
           "00970", # Mellunmäki
           "00920", # Myllypuro
           "00950", # Vartioharju 
           "00960", # Pohjois-Vuosaari 
           "00980", # Etelä-Vuosaari
           "00990", # Aurinkolahti 
           "00900", # Puotinharju 
           "00910", # Puotila            
           "00930"] # Itäkeskus-Marjaniemi
#postal[postal.posti_alue.isin(hkiEast)].plot()

# Östersundom subdivision
hkiOster = ["00890"] # Östersundom
#postal[postal.posti_alue.isin(hkiOster)].plot()


# All of the centers in Helsinki
#postal[postal.posti_alue.isin(hkiSouth + hkiWest + hkiCentral + hkiNorth +
#                              hkiNortheast + hkiSoutheast + hkiEast +
#                              hkiOster)].plot()


# Espoo
#https://www.espoo.fi/fi-fi/Espoon_kaupunki/Tietoa_Espoosta/Tilastot_ja_tutkimukset/Aluejakokartat
# See Excel sheet for exact place names
# Suur-, tilasto- ja pienalueiden nimet 1.1.2014
    
#Suur-Leppävaara subdivision
# Kanta-Leppävaara, Kilo-Karakallio, Laaksolahti, Viherlaakso-Lippajärvi,
# Sepänkylä
espLeppavaara = ["02620", # Karakallio
                 "02610", # Kilo
                 "02600", # Etelä-Leppävaara 
                 "02650", # Pohjois-Leppävaara 
                 "02660", # Lintuvaara
                 "02710", # Viherlaakso
                 "02720", # Lähderanta
                 "02730", # Jupperi
                 "02630", # Nihtisilta
                 "02680"] # Uusmäki
#postal[postal.posti_alue.isin(espLeppavaara)].plot()

#Suur-Tapiola subdivision
# Kanta-Tapiola, Otaniemi, Haukilahti-Westend, Mankkaa, Laajalahti
espTapiola = ["02170", # Haukilahti
              "02140", # Laajalahti
              "02180", # Mankkaa
              "02200", # Niittykumpu
              "02150", # Otaniemi
              "02100", # Tapiola
              "02130", # Pohjois-Tapiola
              "02110", # Otsolahti
              "02120", # Länsikorkee-Suvikumpu
              "02160"] # Westend
#postal[postal.posti_alue.isin(espTapiola)].plot()

#Suur-Matinkylä subdivision
# Matinkylä, Olari, Henttaa-Suurpelto
espMatinkyla = ["02250", # Henttaa
                "02240", # Friisilä
                "02210", # Olari
                "02290", # Puolarmetsän sairaala, own placement
                "02230"] # Matinkylä
#postal[postal.posti_alue.isin(espMatinkyla)].plot()

#Suur-Espoonlahti subdivision
# Kanta-Espoonlahti, Saunalahti, Nöykkiö-Latokaski, Kaitaa, Suvisaaristo
espEspoonlahti = ["02320", # Espoonlahti
                  "02360", # Soukka
                  "02300", # Nöykkiönpuro
                  "02330", # Saunalahti-Kattilalaakso
                  "02340", # Latokaski
                  "02270", # Finnoo-Eestinmalmi
                  "02260", # Kaitaa
                  "02280", # Malminmäki-Eestinlaakso
                  "02380"] # Suvisaaristo
#postal[postal.posti_alue.isin(espEspoonlahti)].plot()

#Suur-Kauklahti subdivision
# Kanta-Kauklahti, Kurttila-Vanttila
espKauklahti = ["02780"] # Kauklahti and all others
#postal[postal.posti_alue.isin(espKauklahti)].plot()

#Vanha-Espoo subdivision
# Kanta-Espoo, Muurala-Gumböle, Bemböle, Nuuksio-Nupuri
espVanhaespoo = ["02770", # Espoon keskus
                 "02810", # Gumböle-Karhusuo
                 "02820", # Nupuri-Nuuksio
                 "02760", # Tuomarila-Suvela
                 "02940", # Lippajärvi-Järvenperä, own placement
                 "02740", # Bemböle-Pakankylä
                 "02750", # Sepänkylä-Kuurinniitty, own placement
                 "02860"] # Siikajärvi
#postal[postal.posti_alue.isin(espVanhaespoo)].plot()

#Pohjois-Espoo subdivision
# Vanhakartano-Röylä, Kalajärvi-Lakisto
espPohjoisespoo = ["02970", # Kalajärvi
                   "02920", # Niipperi
                   "02980"] # Lakisto
#postal[postal.posti_alue.isin(espPohjoisespoo)].plot()

# All of Espoo
#postal[postal.posti_alue.isin(espLeppavaara + espTapiola + espMatinkyla +
#                              espEspoonlahti + espKauklahti + espVanhaespoo +
#                              espPohjoisespoo)].plot()

# Kauniainen
kauniainen = ["02700"]

# Vantaa
# https://www.vantaa.fi/instancedata/prime_product_julkaisu/vantaa/embeds/vantaawwwstructure/124282_Vantaa_alueittain_2015.pdf
# Myyrmäki subdivision
# Linnainen, Hämevaara, Hämeenkylä, Vapaala, Varisto, Myyrmäki, Kaivoksela,
# Martinlaakso, Vantaanlaakso, Askisto, Petikko
vanMyyrmaki = ["01600", # Myyrmäki
               "01610", # Kaivoksela
               "01620", # Martinlaakso
               "01630", # Hämeenkylä
               "01640", # Hämevaara
               "01650", # Vapaala
               "01660", # Varisto
               "01670", # Vantaanlaakso
               "01680", # Askisto
               "01710", # Pähkinärinne
               "01720", # Petikko
               "01770"] # Martinlaakson teollisuusalue, own placement
#postal[postal.posti_alue.isin(vanMyyrmaki)].plot()

# Kivistö subdivision
# Piispankylä, Keimola, Kivistö, Lapinkylä, Myllymäki, Vestra, Luhtaanmäki,
# Riipilä, Seutula, Kiila
vanKivisto = ["01700", # Kivistö
              "01730", # Vantaanpuisto
              "01750", # Keimola
              "01760"] # Seutula
#postal[postal.posti_alue.isin(vanKivisto)].plot()

# Aviapoliksen
# Ylästö, Viinikkala, Tammisto, Pakkala, Veromies, Lentokenttä
vanAviapolis = ["01520", # Tammisto
                "01530", # Veromiehenkylä
                "01690", # Ylästö
                "01510", # Kirkonkylä-Veromäki, my own placement
                "01740"] # Tuupakan teollisuusalue
#postal[postal.posti_alue.isin(vanAviapolis)].plot()

# Tikkurila subdivision
# Hiekkaharju, Tikkurila, Jokiniemi, Viertola, Kuninkaala, Simonkylä,
# Hakkila, Ruskeasanta, Koivuhaka, Helsingin pitäjän kirkonkylä
vanTikkurila = ["01300", # Tikkurila
                "01350", # Hiekkaharju
                "01370", # Jokiniemi
                "01380"] # Kuusikko-Hakkila
#postal[postal.posti_alue.isin(vanTikkurila)].plot()

# Koivukylä subdivision
# Koivukylä, Ilola, Asola, Rekola, Havukoski, Päiväkumpu
vanKoivukyla = ["01360", # Koivukylä-Havukoski
                "01400", # Rekola
                "01340", # Leinelä, my own placement
                "01390", # Ruskeasanta-Ilola, my own placement
                "01420"] # Päiväkumpu
#postal[postal.posti_alue.isin(vanKoivukyla)].plot()

# Korso subdivision
# Matari, Korso, Mikkola, Metsola, Leppäkorpi, Jokivarsi, Nikinmäki, Vierumäki,
# Vallinoja
vanKorso = ["01450", # Korso
            "01480"] # Mikkola
#postal[postal.posti_alue.isin(vanKorso)].plot()

# Hakunila subdivision
# Länsisalmi, Länsimäki, Ojanko, Vaarala, Hakunila, Rajakylä, Itä-Hakkila,
# Kuninkaanmäki, Sotunki
vanHakunila = ["01200", # Hakunila
               "01230", # Vaarala
               "01260", # Itä-Hakkila
               "01280"] # Länsimäki
#postal[postal.posti_alue.isin(vanHakunila)].plot()



# All of Vantaa
#postal[postal.posti_alue.isin(vanMyyrmaki + vanKivisto + vanAviapolis +
#                              vanTikkurila + vanKoivukyla + vanKorso +
#                              vanHakunila)].plot()



# Insert subvision names to records

# Insert column for "suurpiiri" (we'll call them subdivisions) information 
# to records
records["subdiv"] = 0

# This dictionary helps assigning postal codes to dataframe records
subdiv_dict = {"hkiSouth": "Helsinki Southern",
               "hkiWest": "Helsinki Western",
               "hkiCentral": "Helsinki Central",
               "hkiNorth": "Helsinki Northern",
               "hkiNortheast": "Helsinki Northeastern",
               "hkiSoutheast": "Helsinki Southeastern",
               "hkiEast": "Helsinki Eastern",
               "hkiOster": "Helsinki Östersundom",
               "espLeppavaara": "Espoo Suur-Leppävaara",
               "espTapiola": "Espoo Suur-Tapiola",
               "espMatinkyla": "Espoo Suur-Matinkylä",
               "espEspoonlahti": "Espoo Suur-Espoonlahti",
               "espKauklahti": "Espoo Suur-Kauklahti",
               "espVanhaespoo": "Espoo Vanha-Espoo",
               "espPohjoisespoo": "Espoo Pohjois-Espoo",
               "vanMyyrmaki": "Vantaa Myyrmäki",
               "vanKivisto": "Vantaa Kivistö",
               "vanAviapolis": "Vantaa Aviapolis",
               "vanTikkurila": "Vantaa Tikkurila",
               "vanKoivukyla": "Vantaa Koivukylä",
               "vanKorso": "Vantaa Korso",
               "vanHakunila": "Vantaa Hakunila",
               "kauniainen": "Kauniainen"}

# Using dictionary subdiv_dict and lists of zipcodes, assign subdivision names
# to dataframe records.
for varname, fullname in subdiv_dict.items():

    for idx, row in enumerate(records.iterrows()):
        thisZip = row[1].zipcode
        
        if thisZip in eval(varname):
            records.loc[idx, "subdiv"] = fullname
    


#######################################
### UTILISE TRAVEL-TIME MATRIX 2018 ###
#######################################

# https://gis.stackexchange.com/questions/80367/how-to-convert-6-7-digit-coordinates-to-degrees-latitude-longitude
# utm easthing and northing

# this code is meant to compare 2018 travel time matrix results to my results
# Get origin and destination points from TTM data, then compare to zipcodes
# mean in my data

# 0,73 min parkkipaikan etsimisaika. 43,8 s

from pyproj import Proj, transform
import reverse_geocoder as rg

# Origin and destination in YKR_id
origin = "5838783"
destination = "5838787"
traveltimepath = r"C:\Sampon\Maantiede\Master of the Universe\HelsinkiTravelTimeMatrix2018\{0}xxx\travel_times_to_ {1}.txt".format(
        destination[:4], destination)

# get destination txt file and columns not about private cars
destfile = pd.read_csv(traveltimepath, sep=";")
destfile["from_id"] = pd.to_numeric(destfile["from_id"])
delCol = list(destfile.columns[2:13])
destfile = destfile.drop(delCol, axis=1)

# read ykr-grid into GeoDataFrame, then find geometry of the destination
# TÄÄ TEHDÄÄN SKRIPTIN ALUSSA, DELAA TÄÄ JOSSAIN VÄLIS
#grid = gpd.read_file(
#        wd + r"\MetropAccess_YKR_grid_EurefFIN.shp")

grid["YKR_ID"] = pd.to_numeric(grid["YKR_ID"])

destGeom = grid.loc[grid["YKR_ID"] == int(destination)].reset_index()
origGeom = grid.loc[grid["YKR_ID"] == int(origin)].reset_index()

destGeom = destGeom.centroid.geometry[0]
origGeom = origGeom.centroid.geometry[0]

# this offline reverse geocoder is kind of useless!
#def pointToLatLon(point):
#    '''
#    Get Shapely Point coordinates as tuple, in YX order for geolocator
#    '''
#    x, y = tuple(point.coords)[0]
#    
#    # Nominatim. These lines could be inside function
#    myProj = Proj(init="epsg:3067", proj="utm", zone=35)
#    lon, lat = myProj(x, y, inverse=True)
#    
#    return (lat, lon)
    
#destCoords = pointToLatLon(destGeom)
#origCoords = pointToLatLon(origGeom)

# use reverse_geocoder
#match = rg.search(destCoords)
#destMatch = match[0]["name"]

#match = rg.search(origCoords)
#origMatch = match[0]["name"]

# get correct row
thisRow = postal.loc[postal.intersects(origGeom)]

# try to add mean parktime and walktime for rush hour and non-rush hour.
# problems arise when small n causes values be unexpected: like keimola here.
# IF 0.73min IS THE USED PARKING TIME, WHAT IS THE USED WALKING TIME IN THE
# TRAVELTIME MATRIX?

# in data:
# 1 is rushhour
# 2 is weekday other than rushhour
# 3 weekend, 4 anything
thisZipcode = records.loc[records.zipcode == thisRow.posti_alue.values[0]]
thisDict = {
        "car_r_t": thisZipcode.loc[thisZipcode.timeofday == 1]["parktime"].mean(),
        "car_m_t": thisZipcode.loc[thisZipcode.timeofday == 2]["parktime"].mean(),
        "car_sl_t": thisZipcode["parktime"].mean()}

# match origin and destination
res = destfile.loc[destfile["from_id"] == int(origin)].reset_index()
res = res.drop(columns=["index"])
res = pd.concat([res]*2, ignore_index=True) # copy row

# we will assume rush hour = rush hour, midday = other than rush hour







res = res.loc[res.index[0]]

# report
#print("From YKR id {0} ({1}) to YKR id {2} ({3})".format(
#        res[0], origMatch, res[1], destMatch))
print("Origin is located in {0}. Destination in {1}".format(
        postal.loc[postal.intersects(origGeom), "nimi"].values[0],
        postal.loc[postal.intersects(destGeom), "nimi"].values[0]))
print("\n--- Travel time matrix 2018 ----")
print("\nEntire travel time in rush hour traffic: {0} min".format(res[2]))
print("Distance in meters in rush hour traffic: {0} km".format(res[3] / 1000))
print("\nEntire travel time in midday traffic: {0} min".format(res[4]))
print("Distance in meters in midday traffic: {0} km".format(res[5] / 1000))
print("Entire travel time following speed limits without any additional impedances: {0} min".format(
        res[6]))
print("\n --- Sampo Vesanen thesis ---")


# plot these two Points
base = grid.plot(linewidth=0.8, 
                 edgecolor="0.8", 
                 color="white",
                 figsize=(16, 12))
postal.plot(ax=base,
            linewidth=0.8,
            edgecolor="black",
            facecolor="none")
gpd.GeoDataFrame(geometry=[origGeom]).plot(ax=base)
gpd.GeoDataFrame(geometry=[destGeom]).plot(ax=base)
plt.tight_layout()



#################
### VISUALISE ###
#################
# needs more advanced plotting. Compare to other data I have available,
# population, area etc
#https://towardsdatascience.com/lets-make-a-map-using-geopandas-pandas-and-matplotlib-to-make-a-chloropleth-map-dddc31c1983d

# PLOT AMOUNT OF RECORDS
# Plot with layers as function
parkingPlot(postal, "answer_count", 0) #AMOUNT OF RECORDS
parkingPlot(postal, "walktime_mean", 1) #PLOT WALKTIME MEAN
parkingPlot(postal, "parktime_mean", 1) #PLOT PARKTIME MEAN
# ratio answer to total pop not useful!
parkingPlot(postal, "answ_hevakiy", 1) #PLOT ratio answers to total pop
parkingPlot(postal, "pop_dens", 1) #PLOT pop density
parkingPlot(postal, "dens_answ", 1) #PLOT ratio answercount to popdensity



# Visitors datetime chart, v2, annotate important events
########################################################
fig, ax = plt.subplots()
ax.plot_date(visitors["ts_first"], visitors.index)
ax.annotate("local max", 
            xy=("2019-05-22", 191), 
            xytext=("2019-05-30", 1500),
            arrowprops=dict(facecolor="black", shrink=0.05), )
plt.grid(True)
plt.tight_layout()
plt.show()



# Records datetime chart, v2, annotate
######################################
dates = {"Espoo 1": "2019-05-22", "Espoo 2": "2019-06-06"}

fig, ax = plt.subplots(figsize=(22,8))
ax.plot_date(records["timestamp"], records.index)

#annotation for loop
for dictkey, dictvalue in zip(dates.keys(), dates.values()):
    
    # With helper make sure annotation occurs on the first detection of the 
    # date
    helper = 0
    
    for idx, date in enumerate(records["timestamp"]):
        splitdate = str(date).split(" ")
        if splitdate[0] == dictvalue and helper == 0:
            ax.annotate(
                    dictkey, xy=(date, idx), 
                    xytext=(date - timedelta(days=2), idx + 400),
                    arrowprops=dict(facecolor='black', shrink=0.05),)
            helper = 1

plt.grid(True)
plt.tight_layout()
plt.show()



###################################
### Respondent specific reports ###
###################################
    
# TESTING, might not go anywhere

# This builds ellipses on HCR map for each unique respondent. The ellipse
# seems to be places so that it is of minimal distance to all the Points.
# The Points themselves are centroids of postal code areas inputted by the
# respondent.

# build base for map
base = postal.plot(linewidth=0.8, 
                   edgecolor="0.8", 
                   color="white", 
                   figsize=(24, 12))

# View walking area where it takes more time to reach one's car, on map
x, y = walkingBbox.xy
base.plot(x, y)

# get unique visitors
unique_visitors = list(records.ip.unique())

# get areas in Point format for each visitor, then use these Points to 
# create elliptical area of influence
for idx, visitor in enumerate(unique_visitors):
    currentZipcodes = records.zipcode[records.ip.isin([visitor])]
    currentAreas = postal.geometry[postal.posti_alue.isin(currentZipcodes)].centroid
    
    # for testing! for ellipses. idx 820, 700, 900 have many Points!
    if idx == 700:
        break
    
    # show every 500th map
    #if idx % 500 == 0:
    #    currentAreas.plot(ax=base)

# Fit ellipse.
# This works beautifully in plot if if-statement is brought in to the for loop.
# Remember to comment out base and currentAreas.plot() down there
# credit to: https://stackoverflow.com/a/47876498/9455395
if(len(currentAreas) > 1):
    theseX = currentAreas.x
    theseY = currentAreas.y
    xmean, ymean = theseX.mean(), theseY.mean()
    theseX -= xmean
    theseY -= ymean
    
    U, S, V = np.linalg.svd(np.stack((theseX, theseY)))
    
    # alkuperäinen: np.sqrt(2 / N). 140 oli jossain vaiheessa hyvä kun N = 300
    tt = np.linspace(0, 2 * np.pi, 1000)
    circle = np.stack((np.cos(tt), np.sin(tt)))    # unit circle
    transform = np.sqrt(2 / len(currentAreas)) * U.dot(np.diag(S))   # transformation matrix
    fit = transform.dot(circle) + np.array([[xmean], [ymean]])
    
    # initiate zipcodes as background
    base = postal.plot(linewidth=0.8, 
                       edgecolor="0.8", 
                       color="white", 
                       figsize=(24, 12))
    currentAreas.plot(ax=base) # the original Points
    
    # fit ellipse
    plt.plot(fit[0, :], fit[1, :], 'r')
    plt.show()
    
    
    
#####################
### OUTLIER STUFF ###
#####################
#https://medium.com/datadriveninvestor/finding-outliers-in-dataset-using-python-efc3fce6ce32
#https://towardsdatascience.com/ways-to-detect-and-remove-the-outliers-404d16608dba

# outlier graph
import seaborn as sns
sns.boxplot(x=records["parktime"])



###############################
#### Crosstabs test Pandas ####
###############################

#KATO TOI EKA
#https://www.datacamp.com/community/tutorials/categorical-data
#https://www.youtube.com/watch?v=-kTaxac-l7o
#https://statistics.laerd.com/statistical-guides/types-of-variable.php
#https://dfrieds.com/data-analysis/crosstabs-python-pandas
# Try to emulate SPSS Crosstabs --> nominal by interval --> Eta
pd.crosstab(index=records["timeofday"], 
            columns=records["parkspot"]).rename(
        columns={1: "Side of road", 
                 2: "lot", 
                 3: "garage", 
                 4: "private_reserved", 
                 5: "other"})

pd.crosstab(index=records["timeofday"], 
            columns=records["parkspot"]).unstack().reset_index().rename(
    columns={1: "Side of road", 
             2: "lot", 
             3: "garage", 
             4: "private_reserved", 
             5: "other"})


    
#####################
### ONE-WAY ANOVA ###
#####################
    
#https://pythonfordatascience.org/anova-python/
#https://docs.scipy.org/doc/scipy/reference/generated/scipy.stats.f_oneway.html
#https://ariepratama.github.io/How-to-Use-1-Way-Anova-in-Python/
#https://reneshbedre.github.io/blog/anova.html
#https://raiswell.rbind.io/post/one-way-anova-in-python/
#https://plot.ly/python/v3/anova/
#http://pytolearn.csd.auth.gr/d1-hyptest/12/anova-one.html
#https://statistics.laerd.com/statistical-guides/types-of-variable.php

    

# This emulates R and SPSS progression
# SPSS -> analyze -> compare means --> one-way anova 
# options: descriptive, homogeinity of variance test (levene), Brown-Forsythe
    
### describe: walktime~timeofday
# ratkaisu: https://www.marsja.se/pandas-python-descriptive-statistics/
# toimii samalla tavalla ku R
grouped_data = records.groupby(["timeofday"])
round(grouped_data['walktime'].describe(), 3)
    
### levene
# ei sama ku R
levene(records["walktime"], records["timeofday"], center="mean")

### anova
#stats.f_oneway(records["walktime"], records["timeofday"]) # ei sama ku R

#2
# ratkaisu: https://pythonfordatascience.org/anova-python/
# toimii samalla tavalla ku R
mod = ols('walktime ~ C(timeofday)', data=records).fit()
#mod = ols('parktime ~ likert', data=records[records.parkspot==1]).fit() #esim
#mod.summary()
sm.stats.anova_lm(mod, typ=2)

### brown-forsythe    
# ei näytä siltä että tätä olis pythonille
    
    
    
    
    
    
    
    
    
    
    
    
import statsmodels.api as sm
from statsmodels.formula.api import ols    
import seaborn as sns

# basic
f, ax = plt.subplots(figsize=(11, 9))
plt.title("parktime distribution between sample")
sns.distplot(records.parktime)

# advanced
f, ax = plt.subplots(figsize=(11, 9))

sns.distplot(records[records.parkspot == 1].parktime, 
             ax=ax, 
             label="side of street")

sns.distplot(records[records.parkspot == 2].parktime, 
             ax=ax, 
             label="parking lot")

sns.distplot(records[records.parkspot == 3].parktime, 
             ax=ax, 
             label="parking gara")

sns.distplot(records[records.parkspot == 4].parktime, 
             ax=ax, 
             label="priva/reserve")

plt.title("parktime Distribution for parkspot")
plt.legend()

# https://www.marsja.se/four-ways-to-conduct-one-way-anovas-using-python/
#boxplot
records.boxplot("parktime", by="timeofday", figsize=(12, 8))
records.boxplot("parktime", by="parkspot", figsize=(12, 8))



#####################
### EXPORT TO CSV ###
#####################

# data to csv for R. "pythonrecords.csv"
records.to_csv(wd + "records.csv", encoding="Windows-1252")
postal.to_csv(wd + "postal.csv", encoding="Windows-1252")