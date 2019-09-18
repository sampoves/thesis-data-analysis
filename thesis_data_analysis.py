# -*- coding: utf-8 -*-
"""
Created on Fri May 10 00:07:36 2019

@author: Sampo Vesanen

Sampo Vesanen Thesis survey datacrunch

TODO:
    - See how long user took to first visit survey and answer to the survey
    - Respondent specific reports
    - Geographic analyses (Can't get geoplot to work)
    - Detect outliers
    - Have same IPs sent records for same areas more than once?
    - Descriptive statistics, correlation charts
    - Plot records compared to population and area in zipcodes
"""

import os
import pandas as pd
import geopandas as gpd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.pyplot import figure
from datetime import timedelta
from shapely.geometry import Polygon, MultiPolygon, Point
import shapely.affinity

import operator
#from heapq import nlargest
#import mapclassify #scheme="fisher_jenks" needs mapclassify

wd = r"C:\Sampon\Maantiede\Master of the Universe\python"
datawd = r"C:\Sampon\Maantiede\Master of the Universe\leaflet_survey_results"

os.chdir(wd)

# Import functions. Has to be done after inserting os.chdir()
from thesis_data_analysis_funcs import *



################################
### SOME IMPORTANT VARIABLES ###
################################

# Up to date "records"
records_data = "records180919.csv"
visitors_data = "visitors180919.csv"



###################
### IMPORT DATA ###
###################

# Survey data. Lambda x to preserve leading zeros
records = pd.read_csv(os.path.join(datawd, records_data), 
                      converters={'zipcode': lambda x: str(x)})
visitors = pd.read_csv(os.path.join(datawd, visitors_data))

# Shapefiles
grid = gpd.read_file("MetropAccess_YKR_grid_EurefFIN.shp", encoding='utf-8')
resarea = gpd.read_file("paavo\pno_dissolve.shp", encoding='utf-8')



#######################
### FIX SOURCE DATA ###
#######################

# Datetimes to datetime format
records["timestamp"] = convertToDatetime(records, "timestamp")
visitors["ts_first"] = convertToDatetimeVar2(visitors, "ts_first")
visitors["ts_latest"] = convertToDatetimeVar2(visitors, "ts_latest")

# visitors, records: remove author's visits and three records inputted by
# the author
visitors = visitors.iloc[1:]
records = records.drop([0, 5, 6])

# I was contacted on 24.5.2019 22.41 that someone had filled three zipcodes
# erroneously: Kallio, Käpylä and one other (probably Pukinmäki-Savela).
# The following removes those records.
records = records.drop([1277, 1278, 1279])

# Anonymisation of ip addresses
for ip in records.ip:
    records = records.replace(to_replace = ip, value = anonymise()) 



##########################
### PROCESS SHAPEFILES ###
##########################

# Process Helsinki Capital Region geodataframe from PAAVO data
muns = ["091", "049", "092", "235"]
postal = gpd.read_file(r"paavo\2019\pno_tilasto_2019.shp", encoding='utf-8')
postal = postal[postal.kunta.isin(muns)]
postal = postal.reset_index().drop(columns=["index"])


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
for idx, geom in enumerate(postal.geometry):
    if geom.geom_type == "MultiPolygon":
        thisGeom = MultiPolygon(
                [pol for idx, pol in enumerate(geom) if pol.intersects(mainland.buffer(-10))])
        if thisGeom.is_empty == False:
            postal.at[idx, "geometry"] = thisGeom
            


############################
### RESPONDENT BEHAVIOUR ###
############################

# Create groupby aggregations of the records
visitor_grp = records.groupby(["ip"]).agg({
        "id": "count", 
        "timestamp": lambda x: x.tolist(),
        "zipcode": lambda x: x.tolist(),
        "likert": lambda x: x.tolist(),
        "parkspot": lambda x: x.tolist(),
        "parktime": lambda x: x.tolist(),
        "walktime": lambda x: x.tolist(),
        "timeofday": lambda x: x.tolist()})



    
###############################
# respondent specific reports
# IN CONSTRUCTION  

# build base for map
base = postal.plot(linewidth=0.8, 
                   edgecolor="0.8", 
                   color="white", 
                   figsize=(24, 12))

# get unique visitors
unique_visitors = list(records.ip.unique())

# get areas in Point format for each visitor, then use these Points to 
# create elliptical area of influence
for idx, visitor in enumerate(unique_visitors):
    currentZipcodes = records.zipcode[records.ip.isin([visitor])]
    currentAreas = postal.geometry[postal.posti_alue.isin(currentZipcodes)].centroid
    
    # for testing! for ellipses
    #if idx == 1000:
    #    break
    
    # show every 500th map
    if idx % 500 == 0:
        currentAreas.plot(ax=base)

# trying to fit ellipse
xmean, ymean = x.mean(), y.mean()
theseX = currentAreas.x
theseY = currentAreas.y
xmean, ymean = theseX.mean(), theseY.mean()
theseX -= xmean
theseY -= ymean

U, S, V = np.linalg.svd(np.stack((theseX, theseY)))

tt = np.linspace(0, 2*np.pi, 1000)
circle = np.stack((np.cos(tt), np.sin(tt)))    # unit circle
transform = np.sqrt(2/N) * U.dot(np.diag(S))   # transformation matrix
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

#point = postal[postal.posti_alue.isin(currentZipcodes)].unary_union.centroid
#gpd.GeoDataFrame(geometry=Point(point))
##############################################



postal[postal.posti_alue.isin(zipcodes)].plot(
        ax=base,  
        marker='o', 
        linewidth=0.8,
        figsize=(24, 12), 
        edgecolor="0.8",
        legend=True)

for idx, respondent in visitor_grp.iterrows():
    if respondent.id > 1:
        zipcodes = respondent.zipcode
        postal[postal.posti_alue.isin(zipcodes)].plot(
                ax=base,  
                marker='o', 
                linewidth=0.8,
                figsize=(24, 12), 
                edgecolor="0.8",
                legend=True)
        
        
        
        
        
#################################
### DETECTION OF ILLEGAL DATA ###
#################################

# Detect if a user has answered a same area multiple times.
# PLEASE NOTE THAT THIS ONLY LOCATES DUPLICATES
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
    
        # produce report
        for idx, row in dupl.drop(["ip"], axis=1).iterrows():
            print(row.to_string(), "\n") # suppress dtype


# Remove illegal answers. At this time any value 99 is deemed illegal. invalid
# starts at 6 as at this point I have removed 3 of my own records and 3 others
# which were reported to me as false. See above.
delList = []
illegal = []
invalid = 6

for idx, (value, value2, value3) in enumerate(zip(records["parktime"], records["walktime"], records["ip"])):
    if (value == 99 or value2 == 99):
        print("Illegal walktime or parktime detected:", idx, value, value2)
        delList.append(idx)
        illegal.append(value3)
        invalid += 1
 
# Illegal contains all ips which have 99 as answers. illegal_df shows all
# records made by these ip addresses
illegal = list(set(illegal))
illegal_df = records[records["ip"].isin(illegal)]

# Drop the records which have 99 as answer. Drop the ip addresses from visitors
# as well.
records = records.drop(records.index[delList])
records = records.reset_index()
visitors = visitors[~visitors["ip"].isin(illegal)]
visitors = visitors.reset_index()



#################################
### ADD DATA TO GEODATAFRAMES ###
#################################

postal["answer_count"] = 0

# Add column "answer count" to postal.answer_count
for zipcode in records.zipcode:
    for idx, postal_zip in enumerate(postal.posti_alue):
        if postal_zip == zipcode:
            postal.loc[idx, "answer_count"] += 1

### add columns parktime mean and walktime mean
postal["parktime_mean"] = 0
postal["walktime_mean"] = 0

# Calculate mean to postal.parktime_mean
for idx, postal_zip in enumerate(postal.posti_alue):
    this_mean = records.loc[
            records['zipcode'] == postal_zip]["parktime"].mean()
    postal.loc[idx, "parktime_mean"] = round(this_mean, 2)

# Calculate mean to postal.walktime_mean
for idx, postal_zip in enumerate(postal.posti_alue):
    this_mean = records.loc[
            records['zipcode'] == postal_zip]["walktime"].mean()
    postal.loc[idx, "walktime_mean"] = round(this_mean, 2)

# Prepare annotation of plot features
postal['coords'] = postal['geometry'].apply(lambda x: x.representative_point().coords[:])
postal['coords'] = [coords[0] for coords in postal['coords']]



###################################
### PREPARE AND SHOW STATISTICS ###
###################################
    
statistics = Stats(records, postal, visitors, invalid)
statistics.calculateStats()



#####################
### OUTLIER STUFF ###
#####################
#https://medium.com/datadriveninvestor/finding-outliers-in-dataset-using-python-efc3fce6ce32
#https://towardsdatascience.com/ways-to-detect-and-remove-the-outliers-404d16608dba
plt.scatter(records.index, records.parktime)
plt.scatter(records.index, records.walktime)
plt.scatter(records.parktime, records.walktime)

# z-score
outlier_datapoints = detect_outlier(records.parktime)

# iqr
#dataset = records.parktime
dataset = records.walktime

dataset = sorted(dataset)
q1, q3 = np.percentile(dataset, [25,75])

iqr = q3 - q1
lower_bound = q1 - (1.5 * iqr) 
upper_bound = q3 + (1.5 * iqr) 

# outlier graph
import seaborn as sns
sns.boxplot(x=records["parktime"])



# TOISEN SIVUN MEININKI, KESKEN
from scipy import stats
z = np.abs(stats.zscore(
        records[records.columns.difference(['timestamp', "ip", "zipcode"])]))

# test removing outlier data
boston_df_out = boston_df_o1[~((boston_df_o1 < (Q1 - 1.5 * IQR)) | (boston_df_o1 > (Q3 + 1.5 * IQR))).any(axis=1)]



#################
### VISUALISE ###
#################
# needs more advanced plotting. Compare to other data I have available,
# population, area etc
#https://towardsdatascience.com/lets-make-a-map-using-geopandas-pandas-and-matplotlib-to-make-a-chloropleth-map-dddc31c1983d
# see this:
# https://www.dummies.com/education/math/statistics/how-to-interpret-a-correlation-coefficient-r/

#likert-problematiikka
# https://www.theanalysisfactor.com/can-likert-scale-data-ever-be-continuous/
# https://www.researchgate.net/post/Can_we_use_Likert_scale_data_in_multiple_regression_analysis
# https://www.researchgate.net/post/How_can_I_assess_statistical_significance_of_Likert_scale



# REGRESSION LINE TESTING
# STATISTICAL SIGNIFICANCE
from scipy import stats
rho, pval = stats.spearmanr(records.likert, records.parktime)
rho, pval = stats.spearmanr(records.parktime, records.timeofday) #pieni pval!
rho, pval = stats.spearmanr(records.parkspot, records.walktime)

np.random.seed(0)
x = np.random.rand(100, 1)
y = 2 + 3 * x + np.random.rand(100, 1)
plt.scatter(x, y, s=10)
plt.xlabel('liikert/x')
plt.ylabel('parkkitiem/y')
plt.show()

plt.scatter(records.likert, records.parktime, s=10)
plt.xlabel('liikert/x')
plt.ylabel('parkkitiem/y')
plt.show()




# PLOT AMOUNT OF RECORDS
# Plot with layers. Base is basemap for zipcodes without answers
base = postal.plot(linewidth=0.8, 
                   edgecolor="0.8", 
                   color="white", 
                   figsize=(24, 12))

# now plot all non-zero areas on top of base
postal.loc[postal["answer_count"]!=0].plot(
        ax=base, column="answer_count", 
        cmap="OrRd", 
        linewidth=0.8,
        figsize=(24, 12), 
        edgecolor="0.8", 
        scheme='fisher_jenks',
        legend=True)

# annotate
annotationFunction(postal, "answer_count")
plt.tight_layout()




# PLOT WALKTIME MEAN
base = postal.plot(linewidth=0.8, edgecolor="0.8", color="white", 
                   figsize=(24, 12))

postal.loc[~postal["walktime_mean"].isnull()].plot(
        ax=base, 
        column="walktime_mean", 
        cmap="OrRd", 
        linewidth=0.8,
        figsize=(24, 12), 
        edgecolor="0.8", 
        scheme='fisher_jenks',
        legend=True)

# annotate
annotationFunction(postal, "walktime_mean")
plt.tight_layout()




# PLOT PARKTIME MEAN
base = postal.plot(linewidth=0.8, edgecolor="0.8", color="white", 
                   figsize=(24, 12))

postal.loc[~postal["parktime_mean"].isnull()].plot(
        ax=base, 
        column="parktime_mean", 
        cmap="OrRd", 
        linewidth=0.8,
        figsize=(24, 12), 
        edgecolor="0.8", 
        scheme='fisher_jenks',
        legend=True)

# annotate
annotationFunction(postal, "parktime_mean")
plt.tight_layout()





# Visitors datetime chart, v2, annotate important events
########################################################
fig, ax = plt.subplots()
ax.plot_date(visitors["ts_first"], visitors.index)
ax.annotate('local max', 
            xy=("2019-05-22", 191), 
            xytext=("2019-05-30", 1500),
            arrowprops=dict(facecolor='black', shrink=0.05),)
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