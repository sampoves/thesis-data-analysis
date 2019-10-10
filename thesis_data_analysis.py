# -*- coding: utf-8 -*-
"""
Created on Fri May 10 00:07:36 2019

@author: Sampo Vesanen

Sampo Vesanen Thesis survey datacrunch

TODO:
    - Respondent specific reports
    - Geographic analyses (Can't get geoplot to work)
    - Detect outliers
    - Descriptive statistics, correlation charts
    - Plot records compared to population and area in zipcodes

Important questions etc
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
import shapely.affinity
import operator
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



###########################
### PREPARE SOURCE DATA ###
###########################

# Timestamps to datetime format
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
postal = gpd.read_file(r"paavo\2019\pno_tilasto_2019.shp", encoding='utf-8')
postal = postal[postal.kunta.isin(muns)]
postal = postal.reset_index().drop(columns=["index"])

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
    
        # produce report
        for idx, row in dupl.drop(["ip"], axis=1).iterrows():
            print(row.to_string(), "\n") # suppress dtype


# Remove illegal answers. At this time any value equal or above 90 is deemed 
# illegal. invalid starts at 6 as at this point I have removed 3 of my own 
# records and 3 others which were reported to me as false. See above.
delList = []
illegal = []
invalid = 6

for idx, (park_value, walk_value, ip_value) in enumerate(
        zip(records["parktime"], records["walktime"], records["ip"])):
    
    if (park_value >= 90 or walk_value >= 90):
        print("Illegal walktime or parktime detected:", idx, park_value, 
              walk_value)
        delList.append(idx)
        illegal.append(ip_value)
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

# Add column "answer count", "parktime mean", and "walktime mean"
postal["answer_count"] = 0
postal["parktime_mean"] = 0
postal["walktime_mean"] = 0

# Calculate answers
for zipcode in records.zipcode:
    for idx, postal_zip in enumerate(postal.posti_alue):
        if postal_zip == zipcode:
            postal.loc[idx, "answer_count"] += 1

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

# Ratio: answer_count/total population in postal code
# NB This is not useful for plotting, think of something else
postal["answ_hevakiy"] = postal.apply(
        lambda row: round(row["answer_count"]/row["he_vakiy"], 5), axis=1)

# population density
postal["pop_dens"] = postal.apply(
        lambda row: round(row["he_vakiy"]/(row["pinta_ala"]/1000000), 2), axis=1)

# ratio pop density to answer_count
postal["dens_answ"] = postal.apply(
        lambda row: round(row["pop_dens"]/row["answer_count"], 3), axis=1)



###################################
### PREPARE AND SHOW STATISTICS ###
###################################
    
statistics = Stats(records, postal, visitors, invalid)
statistics.calculateStats()



#####################
### EXPORT TO CSV ###
#####################

# data to csv for R. "pythonrecords.csv"
records.to_csv(wd + "records.csv")



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



###############################
#### Crosstabs test Pandas ####
###############################

#KATO TOI EKA
#https://www.datacamp.com/community/tutorials/categorical-data
#https://www.youtube.com/watch?v=-kTaxac-l7o
#https://statistics.laerd.com/statistical-guides/types-of-variable.php
#https://dfrieds.com/data-analysis/crosstabs-python-pandas
# Try to emulate SPSS Crosstabs --> nominal by interval --> Eta
pd.crosstab(index=records["timeofday"], columns=records["parkspot"]).rename(
        columns={1: "Side of road", 2: "lot", 3: "garage", 4: "private_reserved", 
                 5: "other"})

pd.crosstab(index=records["timeofday"], columns=records["parkspot"]).unstack().reset_index().rename(
    columns={1: "Side of road", 2: "lot", 3: "garage", 4: "private_reserved", 
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
    
    

################################
### REGRESSION LINE TESTING  ###
### STATISTICAL SIGNIFICANCE ###
################################
#likert-problematiikka
# https://www.theanalysisfactor.com/can-likert-scale-data-ever-be-continuous/
# https://www.researchgate.net/post/Can_we_use_Likert_scale_data_in_multiple_regression_analysis
# https://www.researchgate.net/post/How_can_I_assess_statistical_significance_of_Likert_scale
# https://www.dummies.com/education/math/statistics/how-to-interpret-a-correlation-coefficient-r/
# https://statistics.laerd.com/statistical-guides/types-of-variable.php

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



#######################
#### Let's try Principal component analysis ####
#######################

# distributing the dataset into two components X and Y 
X = records.iloc[:, 5:9].values 
y = records.iloc[:, 9].values

# Splitting the X and Y into the Training set and Testing set 
from sklearn.model_selection import train_test_split 
from sklearn.preprocessing import StandardScaler 
from sklearn.decomposition import PCA 
from sklearn.linear_model import LogisticRegression     
from sklearn.metrics import confusion_matrix 
from matplotlib.colors import ListedColormap 

X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size = 0.2, random_state = 0) 

# performing preprocessing part 
sc = StandardScaler() 
  
X_train = sc.fit_transform(X_train) 
X_test = sc.transform(X_test) 

# Applying PCA function on training and testing set of X component 
pca = PCA(n_components = 2) 
  
X_train = pca.fit_transform(X_train) 
X_test = pca.transform(X_test) 
  
explained_variance = pca.explained_variance_ratio_ 

# Fitting Logistic Regression To the training set 
classifier = LogisticRegression(random_state = 0) 
classifier.fit(X_train, y_train) 

# Predicting the test set result using predict function under LogisticRegression  
y_pred = classifier.predict(X_test) 

# making confusion matrix between test set of Y and predicted value. 
cm = confusion_matrix(y_test, y_pred) 



# Predicting the training set result through scatter plot  
X_set, y_set = X_train, y_train 
X1, X2 = np.meshgrid(np.arange(start = X_set[:, 0].min() - 1, 
                     stop = X_set[:, 0].max() + 1, step = 0.01), 
                     np.arange(start = X_set[:, 1].min() - 1, 
                     stop = X_set[:, 1].max() + 1, step = 0.01)) 
  
plt.contourf(X1, X2, classifier.predict(np.array([X1.ravel(), 
             X2.ravel()]).T).reshape(X1.shape), alpha = 0.75, 
             cmap = ListedColormap(('yellow', 'white', 'aquamarine'))) 
  
plt.xlim(X1.min(), X1.max()) 
plt.ylim(X2.min(), X2.max()) 
  
for i, j in enumerate(np.unique(y_set)): 
    plt.scatter(X_set[y_set == j, 0], X_set[y_set == j, 1], 
                c = ListedColormap(('red', 'green', 'blue'))(i), label = j) 
  
plt.title('Logistic Regression (Training set)') 
plt.xlabel('PC1') # for Xlabel 
plt.ylabel('PC2') # for Ylabel 
plt.legend() # to show legend 
  
# show scatter plot 
plt.show() 



# Visualising the Test set results through scatter plot 
X_set, y_set = X_test, y_test 
  
X1, X2 = np.meshgrid(np.arange(start = X_set[:, 0].min() - 1, 
                     stop = X_set[:, 0].max() + 1, step = 0.01), 
                     np.arange(start = X_set[:, 1].min() - 1, 
                     stop = X_set[:, 1].max() + 1, step = 0.01)) 
  
plt.contourf(X1, X2, classifier.predict(np.array([X1.ravel(), 
             X2.ravel()]).T).reshape(X1.shape), alpha = 0.75, 
             cmap = ListedColormap(('yellow', 'white', 'aquamarine')))  
  
plt.xlim(X1.min(), X1.max()) 
plt.ylim(X2.min(), X2.max()) 
  
for i, j in enumerate(np.unique(y_set)): 
    plt.scatter(X_set[y_set == j, 0], X_set[y_set == j, 1], 
                c = ListedColormap(('red', 'green', 'blue'))(i), label = j) 
  
# title for scatter plot 
plt.title('Logistic Regression (Test set)')  
plt.xlabel('PC1') # for Xlabel 
plt.ylabel('PC2') # for Ylabel 
plt.legend() 
  
# show scatter plot 
plt.show() 