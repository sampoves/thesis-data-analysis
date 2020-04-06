# -*- coding: utf-8 -*-
"""
Created on Fri May 10 00:07:36 2019

@author: Sampo Vesanen

Sampo Vesanen Thesis survey datacrunch
"Parking of private cars and spatial accessibility in Helsinki Capital Region"

This script was created using Anaconda Python Distribution 2019.7. In addition
to all packages delivered with Anaconda, GeoPandas 0.5.0 with its dependencies 
was installed for this work. Unfortunately I can't guarantee the smooth execution 
of this code on newer package versions.

Please note that this script talks about "respondents" and "IP addresses"
interchangeably. For the most part an unique IP address can be considered to
be a unique person, but this can't be verified as households and public venues
share a wifi and therefore IP addresses. For simplicity this script assumes
an unique IP address is an unique person.


TODO, maybe
    - user "oe4guro3dh", remove from visitors. Also, see what's going on with
    the discrepancy. the three records really do not exist in visitors
"""

import os
import pandas as pd
import geopandas as gpd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.pyplot import figure
from datetime import timedelta
from shapely.geometry import Polygon, MultiPolygon, Point, LinearRing
from rtree import index
import random

# Working directories
wd = r"C:\Sampon\Maantiede\Master of the Universe"
script_wd = r"C:\Sampon\Maantiede\Master of the Universe/python"
ttm_path = r"HelsinkiTravelTimeMatrix2018\{0}xxx\travel_times_to_ {1}.txt"

# Survey data file paths
records_data = r"leaflet_survey_results\records.csv"
visitors_data = r"leaflet_survey_results\visitors.csv"

# Spatial data paths
grid_path = r"python\MetropAccess_YKR_grid_EurefFIN.shp"
resarea_path = r"python\paavo\pno_dissolve.shp"
postal_path = r"python\paavo\2019\pno_tilasto_2019.shp"
ykr_path = r"yhdyskuntarakenteenvyoh17\YKRVyohykkeet2017.shp"
ua_forest_path = r"python\FI001L3_HELSINKI\ua2012_research_area.shp"

# Import functions and lists of zipcodes. Has to be done after inserting 
# os.chdir()
os.chdir(script_wd)
from thesis_data_analysis_funcs import *
from thesis_data_zipcodes import *
os.chdir(wd)


###################
### IMPORT DATA ###
###################

# Survey data. Lambda x to preserve leading zeros
records = pd.read_csv(records_data, 
                      converters={"zipcode": lambda x: str(x)}, 
                      index_col=[0])
visitors = pd.read_csv(visitors_data, index_col=[0])

# Shapefiles
grid = gpd.read_file(grid_path, encoding="utf-8")
resarea = gpd.read_file(resarea_path, encoding="utf-8")
postal = gpd.read_file(postal_path, encoding="utf-8")

# Import urban zones shapefile and select only relevant area for this study
ykr_vyoh = gpd.read_file(ykr_path, encoding="utf-8")
ykr_vyoh = gpd.overlay(ykr_vyoh, 
                       gpd.GeoDataFrame(geometry=resarea.buffer(500)), 
                       how="intersection")

# From Urban Atlas 2012 Helsinki area preserve only forests
forest = gpd.read_file(ua_forest_path, encoding="utf-8")
forest = forest[forest.ITEM2012 == "Forests"]



###########################
### PROCESS SURVEY DATA ###
###########################

# Timestamps to datetime format
records["timestamp"] = pd.to_datetime(
        records.timestamp, format="%d-%m-%Y %H:%M:%S") 
visitors["ts_first"] = pd.to_datetime(
        visitors.ts_first, format="%Y-%m-%d %H:%M:%S")
visitors["ts_latest"] = pd.to_datetime(
        visitors.ts_latest, format="%Y-%m-%d %H:%M:%S")

# visitors, records: remove author's home visits and three records inputted by
# the author. Additionally, I have visited the survey from the university, 
# work, and with mobile phone. These visits are impossible to trace.
records = records.drop([0, 5, 6])
visitors = visitors.iloc[1:] 
visitors = visitors.drop([3753])

# I was contacted on 24.5.2019 22.41 with the message that a respondent had 
# filled three responses erroneously: for Kallio, Käpylä and one other 
# (probably Pukinmäki-Savela). Remove these records.
records = records.drop([1277, 1278, 1279])

# Alert if records has IP codes which do not exist in visitors
diff = list(set(records.ip) - set(visitors.ip))
if diff != 0:
    print("NB! The following IP codes exist only in records: {0}".format(
            diff))
    print("This discrepancy may distort analysis of visitors.")



######################
### PROCESS POSTAL ###
######################

# Process Helsinki Capital Region GeoDataFrame from PAAVO data
muns = ["091", "049", "092", "235"]
postal = postal[postal.kunta.isin(muns)]
postal = postal.reset_index().drop(columns=["index", "euref_x", "euref_y"])

# Remove islands unreachable by car. This process would have been easy to do 
# in QGIS, for example, but here we have the same process realised in an
# overcomplicated way.

# Preserve two largest Polygons from these postal code areas
preserveTwoLargest = ["00830", "00570", "00340", "00200", "00890"]

# Preserve largest includes island postal code areas
preserveLargest = ["00210", "00860", "00840", "00850", "00870", "00590"]

# Suvisaaristo needs special attention. Define a Polygon to help detect 
# areas in Suvisaaristo reachable by car
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
# setting with an iterable". Will remain in GeoPandas version 0.5.0 for this
# thesis.
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
# my findings I need to add postal area code data to the GeoDataFrame grid. 
# This enables me to assign each postal code area with mean data about parktime 
# and walktime.
            
# The for loop below assigns zipcode to a cell when more than 50 percent of the
# current cell area is in that zipcode. Helsinki Capital Region is of different
# shape in grid and postal and the for loop below will not iterate through 
# cells outside of postal. Add these cells to grid_areas after this for loop.

# Establish dataframe to collect zipcode area data
grid_areas = gpd.GeoDataFrame(columns=["YKR_ID", "zipcode", "largest_area", 
                                       "total_area"])
crs = grid.crs
grid_sindex = grid.sindex # GeoPandas spatial index

# Use i to create new rows in GeoDataFrame grid_areas
i = 0
for postalarea in postal.itertuples():   

    precise_matches = spatialIndexFunc(grid_sindex, grid, postalarea.geometry)
    
    # Make the current zipcode to a GeoDataFrame
    thisZip = gpd.overlay(precise_matches,
                          gpd.GeoDataFrame(crs=crs, geometry=[postalarea.geometry]),
                          how="intersection")
    thisZip["area1"] = thisZip.area

    # Iterate through all rows in current zipcode and see if each YKR_ID is
    # in grid_areas or not
    for row in thisZip.itertuples():

        # this YKR_ID does not yet exist in grid_areas
        if grid_areas[grid_areas.YKR_ID.isin([row.YKR_ID])].empty == True:

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


# Find out which grid ykr ids do not occur in GeoDataFrame grid_areas. These 
# cells are outside of PAAVO postal code area boundaries
notPresent = list(np.setdiff1d(list(grid.YKR_ID), list(grid_areas.YKR_ID)))

# Add these cells in GeoDataFrame grid_areas as zipcode 99999
for cell_id in notPresent:
    grid_areas = grid_areas.append(
        {"YKR_ID": np.int64(cell_id), 
         "zipcode": "99999"}, 
        ignore_index=True)

# Incorporate "grid_areas" to the original GeoDataFrame grid
grid = pd.concat(
        [grid.set_index("YKR_ID"), grid_areas.set_index("YKR_ID")], 
        axis=1, 
        join="inner").reset_index()



############################
### RESPONDENT BEHAVIOUR ###
############################

# Create groupby aggregations of the records. One can use this DataFrame to
# see how each respondent (or, specifically, one IP address) answered to the 
# survey.
visitors_grp = records.groupby("ip").agg({
        "id": "count", 
        "timestamp": lambda x: x.tolist(),
        "zipcode": lambda x: x.tolist(),
        "likert": lambda x: x.tolist(),
        "parkspot": lambda x: x.tolist(),
        "parktime": lambda x: x.tolist(),
        "walktime": lambda x: x.tolist(),
        "timeofday": lambda x: x.tolist()})
visitors_grp = visitors_grp.rename(columns={"id": "amount"})
#visitors_grp.to_excel("visitors_grouped.xlsx")
        


#################################
### DETECTION OF ILLEGAL DATA ###
#################################

# Erase contents of duplicates.txt before writing anything (wipe old data). 
# Wrap the following for loop in a "with" loop. This enables us to write the 
# duplicates report as a text file in the working directory for easy viewing.
# Edit Pandas max_colwidth to show even the lengthiest rows. Pandas default is 
# 50.
pd.options.display.max_colwidth = 100
open("duplicates.txt", "w").close()

# Detect if a respondent has answered to the same area multiple times. Please 
# note that this only locates duplicates but does nothing about it. I will leave 
# the discussion about this to the thesis.
with open(os.path.join(wd, "duplicates.txt"), "a+") as f:
    for visitor in records.ip.unique():
        theseRecords = records.loc[records.ip == visitor]
        
        if(theseRecords.zipcode.is_unique) == False:
            f.write("\nDuplicates detected for ip code {0}\n".format(
                    theseRecords.ip.unique()[0]))
            dupl = theseRecords[theseRecords.zipcode.duplicated(keep=False)]
            dupl = dupl.groupby("zipcode").agg(
                    {"id": lambda x: x.tolist(),
                     "timestamp": lambda x: x.tolist(), 
                     "ip": "first",
                     "zipcode": "first", 
                     "likert": lambda x: identicaltest(x), # defined in funcs.py
                     "parkspot": lambda x: identicaltest(x),
                     "parktime": lambda x: identicaltest(x), 
                     "walktime": lambda x: identicaltest(x), 
                     "timeofday": lambda x: identicaltest(x)})
        
            # Produce current rows in the text file
            for idx, row in dupl.drop(["ip"], axis=1).iterrows():
                f.write("{}\n\n".format(row.to_string()))
                
print("\nA report on duplicate answers saved to disk in path {0}\n".format(
        os.path.join(wd, "duplicates.txt")))


# Remove illegal answers. At this time any value equal or above 60 minutes is 
# deemed illegal. Value invalid starts at 6 as at this point I have removed 3 
# of my own records and 3 others which were reported to me as false. See above,
# part "Process survey data"
delList = []
illegal = []
invalid = 6

for idx, (park_value, walk_value, ip_value) in enumerate(
        zip(records.parktime, records.walktime, records.ip)):
    
    if (park_value >= 60 or walk_value >= 60):
        print("Illegal walktime and/or parktime detected:", idx, park_value, 
              walk_value)
        delList.append(idx)
        illegal.append(ip_value)
        invalid += 1
 
# Illegal contains all IPs which have values equal or above 60 as answers. 
# DataFrame "illegal_df" shows all records made by these IP addresses
illegal = list(set(illegal))
illegal_df = records[records.ip.isin(illegal)]

# Drop the records which have values equal or above 60 as answer. Drop the same
# IP address codes from visitors as well.
records = records.drop(records.index[delList])
records = records.reset_index()
visitors = visitors[~visitors.ip.isin(illegal)]
visitors = visitors.reset_index()



#################################
### ADD DATA TO GEODATAFRAMES ###
#################################

# Reclassify ykr_vyoh. Reclassification is created as in Finnish Environment
# Institute website (Only available in Finnish)
#https://www.ymparisto.fi/fi-FI/Elinymparisto_ja_kaavoitus/Yhdyskuntarakenne/Tietoa_yhdyskuntarakenteesta/Yhdyskuntarakenteen_vyohykkeet
ykr_vyoh["vyohselite"] = ykr_vyoh.vyohselite.replace(
        {"keskustan reunavyöhyke/intensiivinen joukkoliikenne": 
            "keskustan reunavyöhyke", 
         "keskustan reunavyöhyke/joukkoliikenne": 
             "keskustan reunavyöhyke",
         "alakeskuksen jalankulkuvyöhyke/intensiivinen joukkoliikenne": 
             "alakeskuksen jalankulkuvyöhyke",
         "alakeskuksen jalankulkuvyöhyke/joukkoliikenne": 
             "alakeskuksen jalankulkuvyöhyke"})

# Add additional columns
postal["ykr_kesk_jalan"] = 0
postal["ykr_kesk_reuna"] = 0
postal["ykr_int_joukko"] = 0
postal["ykr_joukkoliik"] = 0
postal["ykr_alakesk_jalan"] = 0
postal["ykr_autovyoh"] = 0
postal["ykr_novalue"] = 0
postal["ua_forest"] = 0

# Calculate column "answer_count". Answer count for each zipcode area
answ = records.zipcode.value_counts().rename("answer_count")
postal = postal.join(answ, on="posti_alue")

# Calculate mean and median for parktime and walktime. Join by zipcode
roundmean = lambda x: round(x.mean(), 2)
calcs = records.groupby("zipcode").agg(
        {"parktime": {"Mean": roundmean, "Median": "median"},
         "walktime": {"Mean": roundmean, "Median": "median"}})
calcs.columns = "parktime_mean", "parktime_median", "walktime_mean", "walktime_median"
postal = postal.join(calcs, on="posti_alue")

# Prepare annotation of Polygon features
postal["coords"] = polygonCoordsToTuple(postal)



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

for row in postal.itertuples():
    
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
        thisRow.vyohselite: 
            round(thisRow.geometry.area / thisPol.unary_union.area, 3)
        } for thisRow in group.itertuples()]
    
    # Iterate thisDict, list of dictionaries to insert values in "postal"
    for item in thisDict:
        for k, v in item.items():
            
            # Add data to postal. In .loc square brackets select correct row of 
            # placement then use dictKey dictionary to tell which column to 
            # append in. Finally append current value v
            postal.loc[postal.posti_alue == row.posti_alue, dictKey[k]] = v
  
# Calculate ykr_novalue
postal["ykr_novalue"] = postal.apply(
        lambda row: 1 - (row.ykr_kesk_jalan + row.ykr_kesk_reuna +
        row.ykr_int_joukko + row.ykr_joukkoliik + 
        row.ykr_alakesk_jalan + row.ykr_autovyoh), axis=1)

# Make sure rounding does not cause trouble in the calculation above
postal["ykr_novalue"].loc[postal.ykr_novalue < 0] = 0


# Urban Atlas 2012 forest

# Reproject the geometries by replacing the values with projected ones
forest = forest.to_crs(epsg=3067)
crs = forest.crs

# Employ GeoPandas spatial index. We will reduce the extent to process
# the intersections and this will greatly trim down the time needed for the
# calculation.
forest_sindex = forest.sindex

for row in postal.itertuples():
    
    thisPol = gpd.GeoDataFrame(geometry=[row.geometry], crs=crs)
    thisGeom = thisPol.loc[0, "geometry"]
    precise_matches = spatialIndexFunc(forest_sindex, forest, thisGeom)
    
    # precise_matches may be empty, so check for that
    if precise_matches.empty == False:
        thisIntersect = gpd.overlay(thisPol, precise_matches, how="intersection")
    
        if thisIntersect.empty == False:
            forestpercent = thisIntersect.unary_union.area / thisPol.area
            
            # append forest data to column "ua_forest"
            postal.loc[postal.posti_alue == row.posti_alue, "ua_forest"] = round(
                    forestpercent[0], 3)



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
#breaks = getJenksBreaks(records.ua_forest.tolist(), 5)

# See the breaks on plot
#plt.figure(figsize=(10, 8))
#hist = plt.hist(records.ua_forest, bins=20, align="left", color="g")
#for b in breaks:
#    plt.vlines(b, ymin=0, ymax=max(hist[0]))

# Reclassify using this clumsy nested np.where(). Use values calculated in
# getJenksBreaks()
records["ua_forest"] = np.where(
        records.ua_forest < 0.062,
        "Scarce forest", 
        (np.where(records.ua_forest < 0.167,
                  "Some forest", 
                  (np.where(records.ua_forest < 0.288, 
                            "Moderate forest", 
                            (np.where(records.ua_forest < 0.495, 
                                      "Mostly forest", 
                                      (np.where(records.ua_forest < 1, 
                                                "Predominantly forest",
                                                "novalue"))))))))).tolist()

# Use largest percentage for a class in each zipcode
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

# Reverse dictKey, add entry for "ykr_novalue". Then rename ykr zone records 
# to human readable versions
dictKey = {v: k for k, v in dictKey.items()}
dictKey["ykr_novalue"] = "novalue"
records["ykr_zone"] = records.ykr_zone.map(dictKey)



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

# View further information about subdivisions in script thesis_data_zipcodes.py.
            
# Create column for subdivision name in DataFrame "records"
records["subdiv"] = 0

# Using dictionary subdiv_dict and lists of zipcodes from 
# "thesis_data_zipcodes.py", assign subdivision names to dataframe "records"
for varname, fullname in subdiv_dict.items():
    records.loc[records.zipcode.isin(eval(varname)), "subdiv"] = fullname



#### This point marks the Python complete data of my thesis. ####



#######################################
### UTILISE TRAVEL-TIME MATRIX 2018 ###
#######################################

# IF 0.42min IS THE USED PARKING TIME, WHAT IS THE USED WALKING TIME IN 
# THE TRAVELTIME MATRIX?
l = []
i = 0
while i < 50:
    # In "valuerange" make sure no grid cells outside research area are accepted
    valuerange = set(grid.YKR_ID.astype(str)) - set(list(map(str, notPresent)))
    vals = random.sample(valuerange, 2)
    l.append(tuple(vals))
    i += 1

#l = [("5985086", "5866836"), ("5981923", "5980266")]
traveltime = travelTimeComparison(grid, forest, postal, records, l, ttm_path, 
                                  printStats=False, plotIds=False)

# get means for all columns, see differences
round(traveltime[["car_r_drivetime", "car_m_drivetime", "car_sl_drivetime",
                  "car_r_pct", "car_m_pct", "car_sl_pct",
                  "thesis_r_drivetime","thesis_m_drivetime", 
                  "thesis_sl_drivetime", "thesis_r_pct", "thesis_m_pct", 
                  "thesis_sl_pct"]].mean(axis=0), 2)


    
############################
### VISUALISE & DESCRIBE ###
############################
# These visualisations are mostly obsolete. Most visual mapping is now done
# in R.

# PLOT AMOUNT OF RECORDS
# Plot with layers as function
parkingPlot(postal, "answer_count", 0) 
parkingPlot(postal[postal.kunta == "091"], "answer_count", 0) #amount for Hki
parkingPlot(postal, "walktime_mean", 1) # plot walktime mean
parkingPlot(postal, "parktime_mean", 1) # plot parktime mean
parkingPlot(postal, "ua_forest", 1) # plot parktime mean

# Get mean-std-min-max-quantiles of municipalities
descri_postal = postal.iloc[:, [4, 113, 114, 115, 116, 117]]
descri_postal.describe()
descri_postal[postal.kunta == "091"].describe() #hki
descri_postal[postal.kunta == "092"].describe() #esp
descri_postal[postal.kunta == "235"].describe() #kau
descri_postal[postal.kunta == "049"].describe() #van



#####################
### EXPORT TO CSV ###
#####################

# Data to csv for more processing in R.
#records.to_csv("records_for_r.csv", encoding="Windows-1252")
#postal.to_csv("postal_for_r.csv", encoding="Windows-1252")

# For Shinyapps.io. There are huge problems with character encoding down the 
# line if Ascii-UTF-8 conversion is not done here.
#import unicodedata
#records["subdiv"] = records.subdiv.apply(lambda val: unicodedata.normalize("NFKD", val).encode("ascii", "ignore").decode())
#records["ykr_zone"] = records.ykr_zone.apply(lambda val: unicodedata.normalize("NFKD", val).encode("ascii", "ignore").decode())
#records.to_csv("shinyrecords.csv", encoding="utf-8")

#postal["nimi"] = postal.nimi.apply(lambda val: unicodedata.normalize("NFKD", val).encode("ascii", "ignore").decode())
#postal["namn"] = postal.namn.apply(lambda val: unicodedata.normalize("NFKD", val).encode("ascii", "ignore").decode())
#postal.to_csv("shinypostal.csv", encoding="utf-8")