# -*- coding: utf-8 -*-
"""
Created on Fri May 10 00:07:36 2019

@author: Sampo Vesanen

Sampo Vesanen Thesis survey datacrunch
"Parking of private cars and spatial accessibility in Helsinki Capital Region"

TODO, maybe
    - Geographic analyses (Can't get geoplot to work)
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
from rtree import index
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
records = pd.read_csv(records_data, converters={"zipcode": lambda x: str(x)})
visitors = pd.read_csv(visitors_data)

# Shapefiles
grid = gpd.read_file("MetropAccess_YKR_grid_EurefFIN.shp", encoding="utf-8")
resarea = gpd.read_file("paavo\pno_dissolve.shp", encoding="utf-8")
postal = gpd.read_file(r"paavo\2019\pno_tilasto_2019.shp", encoding="utf-8")

# Import urban zones shapefile and select only relevant area for this study
ykr_vyoh = gpd.read_file(
        r"C:\Sampon\Maantiede\Master of the Universe\yhdyskuntarakenteenvyoh17\YKRVyohykkeet2017.shp", 
        encoding="utf-8")
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

# Process Helsinki Capital Region GeoDataFrame from PAAVO data
muns = ["091", "049", "092", "235"]
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

# My survey data and Travel Time Matrix 2018 operate in different spatial
# units. TTM is available in YKR grid cells, while my data was gathered
# with PAAVO zipcode delineations as the base unit. In order to compare TTM and 
# my findings I need to add postal area code data to grid. This enables me to
# assign each postal code area with mean data about parktime and walktime.
            
# The for loop below assigns zipcode to a cell when more than 50 percent of the
# current cell area is in that zipcode. Helsinki Capital Region is of different
# shape in grid and postal and the for loop below will not iterate through 
# cells outside of postal. Add these cells to grid_areas after this for loop.

# Establish dataframe to collect zipcode area data
grid_areas = gpd.GeoDataFrame(columns=["YKR_ID", "zipcode", "largest_area", 
                                       "total_area"])
crs = grid.crs
spatial_index = grid.sindex # geopandas spatial index

# Use i to create new rows to grid_areas
i = 0
for idx, postalarea in postal.iterrows():   
    #print("now processing {0}".format(postalarea.nimi))
    
    possible_matches_index = list(
            spatial_index.intersection(postalarea.geometry.bounds))
    possible_matches = grid.iloc[possible_matches_index]
    precise_matches = possible_matches[
            possible_matches.intersects(postalarea.geometry)]
    
    # current zipcode as df
    thisZip = gpd.overlay(precise_matches,
                        gpd.GeoDataFrame(crs=crs, geometry=[postalarea.geometry]),
                        how="intersection")
    thisZip["area1"] = thisZip.area

    # Iterate through all rows in current zipcode and see if each YKR_ID is
    # in grid_areas or not
    for inner_idx, row in thisZip.iterrows():

        # this YKR_ID does not yet exist in grid_areas
        if grid_areas[grid_areas["YKR_ID"].isin([row.YKR_ID])].empty == True:

            grid_areas.loc[i, "YKR_ID"] = row.YKR_ID
            grid_areas.loc[i, "zipcode"] = postalarea.posti_alue
            grid_areas.loc[i, "largest_area"] = row.area1
            grid_areas.loc[i, "total_area"] = row.area1
            i += 1
            
        # The current YKR_ID was found in grid_areas 
        else:   

            # This is the location of the located pre-existing ykr_id
            found_idx = grid_areas.index[grid_areas.YKR_ID == row.YKR_ID][0]
            
            # In this case the ykr id has been iterated over in some previous
            # iteration of a postal area. Only add to largest_area if area value
            # is larger than before and add to total_area
            if row.area1 > grid_areas.loc[found_idx, "largest_area"]:
                grid_areas.loc[found_idx, "largest_area"] = row.area1
                grid_areas.loc[found_idx, "zipcode"] = postalarea.posti_alue
                
            grid_areas.loc[found_idx, "total_area"] += row.area1


# Find out which grid ykr-ids do not occur in grid_areas. These are the cells 
# outside PAAVO postal code areas
notPresent = list(np.setdiff1d(list(grid.YKR_ID), list(grid_areas.YKR_ID)))

# Add cells to grid_areas
for cell_id in notPresent:
    grid_areas = grid_areas.append(
        {"YKR_ID": np.int64(cell_id), 
         "zipcode": "99999"}, 
        ignore_index=True)

# Incorporate grid_areas area data to grid
grid = pd.concat(
        [grid.set_index("YKR_ID"), grid_areas.set_index("YKR_ID")], 
        axis=1, join="inner").reset_index()

# With this shp we can verify visually that the method works great
#grid.to_file("ykrgrid2.shp")

# verify zipcodes in plot
# get postal codes to grid and separate each zipcode area. TODO: incorporate
# zipcodes to grid
#base = grid.plot(linewidth=0.8, 
#                 edgecolor="0.8", 
#                 color="white",
#                 figsize=(16, 12))

# tad obsolete now that grid has zipcode data
#for code in list(set(grid_areas3.zipcode)):
#    theseCells = grid_areas["YKR_ID"][grid_areas["zipcode"] == code]
#    thisArea = grid[grid["YKR_ID"].isin(list(theseCells))]
#    thisArea.plot(ax=base,
#            linewidth=0.8,
#            edgecolor="none",
#            facecolor=np.random.rand(3,))



############################
### RESPONDENT BEHAVIOUR ###
############################

# Create groupby aggregations of the records. One can use this dataframe to
# see how each respondent answered to the survey
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
# this only locates duplicates but does nothing about it. I will leave the
# discussion about this to the thesis.
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

# Drop the records which have values equal or above 60 as answer. Drop the same
# IP address codes from visitors as well.
records = records.drop(records.index[delList])
records = records.reset_index()
visitors = visitors[~visitors["ip"].isin(illegal)]
visitors = visitors.reset_index()



#################################
### ADD DATA TO GEODATAFRAMES ###
#################################

# Reclassify ykr_vyoh. Reclassification is created as in Finnish Environment
# Institute website (Only available in Finnish)
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

# Add ykr_vyoh columns
postal["ykr_kesk_jalan"] = 0
postal["ykr_kesk_reuna"] = 0
postal["ykr_int_joukko"] = 0
postal["ykr_joukkoliik"] = 0
postal["ykr_alakesk_jalan"] = 0
postal["ykr_autovyoh"] = 0
postal["ykr_novalue"] = 0

# Add Urban Atlas 2012 forest amount
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

# The idea is to overlay yhdyskuntarakenteen vyöhykkeet on dataframe postal and 
# see the percentage how much of each zone is included in the zipcode in 
# question. Together with checking out how much forest is there in research 
# area we could find some interesting results in the statistical analysis.


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
# MERGE YKR ZONES AND FOREST TO RECORDS #
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

# Helsinki subdivisions, see:
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


# Espoo subdivisions:
#https://www.espoo.fi/fi-fi/Espoon_kaupunki/Tietoa_Espoosta/Tilastot_ja_tutkimukset/Aluejakokartat
# See Excel sheet for exact place names:
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


# Vantaa subdivisions:
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

# Create column for subdivisions information to records
records["subdiv"] = 0

# This dictionary helps assigning postal codes to DataFrame records
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

# This code is meant to compare 2018 Travel-Time Matrix results to my results.
# Get origin and destination points from TTM data, then compare to zipcodes
# mean in my data

# Above I sorted out zipcodes for each cell in grid to enable comparison here.
# Use originId and destinationId to find which zipcode origin and destination
# are, then use this zipcode to find the corresponding data on mean parktime
# and walktime in records.

# In TTM, searching for parking is 0.73 minutes (43.8 s). Searching for parking
# includes the parking itself.

# Origin and destination in YKR_id
originId = "5838783"
destinationId = "5838787"
traveltimepath = r"C:\Sampon\Maantiede\Master of the Universe\HelsinkiTravelTimeMatrix2018\{0}xxx\travel_times_to_ {1}.txt".format(
        destinationId[:4], destinationId)

# Get destination txt file. Remove all columns not about private cars
destfile = pd.read_csv(traveltimepath, sep=";")
destfile["from_id"] = pd.to_numeric(destfile["from_id"])
delCol = list(destfile.columns[2:13])
destfile = destfile.drop(delCol, axis=1)

# destination and origin in grid
dest = grid.loc[grid["YKR_ID"] == int(destinationId)].reset_index()
orig = grid.loc[grid["YKR_ID"] == int(originId)].reset_index()
destGeom = dest.centroid.geometry[0]
origGeom = orig.centroid.geometry[0]

# Get the row of origin and destination in postal. maybe unnecessary, can
# just use orig there below in thisZipcode
#origZipData = postal[postal["posti_alue"] == orig["zipcode"][0]]

# Try to calculate mean parktime and walktime for rush hour and non-rush hour.
# Problems arise when small n causes values be unexpected: like Keimola here.

# IF 0.73min IS THE USED PARKING TIME, WHAT IS THE USED WALKING TIME IN THE
# TRAVELTIME MATRIX?
# --- ANSWER MAY BE THAT WALKING TO DESTINATION IS INCLUDED IN THIS 0.73min

# in data:
# 1 is rushhour (equivalent of car_r_t)
# 2 is weekday other than rushhour (equivalent of car_m_t)
# 3 weekend, 4 anything

# car_sl_t could be the entire area as mean
thisZipcode = records.loc[records.zipcode == orig.zipcode[0]]
thisDict = {
        "values_in_zip": len(thisZipcode),
        "car_r_t": thisZipcode.loc[thisZipcode.timeofday == 1]["parktime"].mean(),
        "car_m_t": thisZipcode.loc[thisZipcode.timeofday == 2]["parktime"].mean(),
        "car_sl_t": thisZipcode["parktime"].mean()}

# Get TTM2018 data for the origin
# Match origin and destination
res = destfile.loc[destfile["from_id"] == int(originId)].reset_index()
res = res.drop(columns=["index"])
res = pd.concat([res]*2, ignore_index=True) # copy row for the use of my data

# We will assume:
# rush hour (car_r_t) = rush hour, 
# midday (car_m_t) = other than rush hour
# full throttle (car_sl_t) = everything as mean






res = res.loc[res.index[0]]

# Report
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


# Plot origin and destination
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