# -*- coding: utf-8 -*-
"""
Created on Fri May 10 00:07:36 2019

@author: Sampo Vesanen

Sampo Vesanen Thesis survey datacrunch

TODO:
    - See how long user took to first visit survey and answer to the survey
    - Respondent specific reports
    - Filter results with 99 (probably me)
    - Geographic analyses (geoplot not working sadface)
    - Detect outliers
    - Have same IPs sent records for same areas more than once?
    - se yks tyyppi joka sanoi lähettäneensä virhedataa
    - Descriptive statistics, correlation charts
"""

import os
import pandas as pd
import geopandas as gpd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.pyplot import figure
from datetime import timedelta
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
records_data = "records260619.csv"
visitors_data = "visitors260619.csv"



###################
### IMPORT DATA ###
###################

# Survey data
records = pd.read_csv(os.path.join(datawd, records_data), 
                      converters={'zipcode': lambda x: str(x)})
visitors = pd.read_csv(os.path.join(datawd, visitors_data))

# Shapefiles
grid = gpd.read_file("MetropAccess_YKR_grid_EurefFIN.shp", encoding='utf-8')
postal = gpd.read_file("paavo\pno_research_area.shp", encoding='utf-8')
resarea = gpd.read_file("paavo\pno_dissolve.shp", encoding='utf-8')



#######################
### FIX SOURCE DATA ###
#######################

# Due to some shapefile saving shenanigans my source encoding string encoding
# was Windows-1252. Fix the encoding with these list comprehensions
postal.nimi = [x.encode("windows-1252").decode("utf-8") for x in list(postal.nimi)]
postal.namn = [x.encode("windows-1252").decode("utf-8") for x in list(postal.namn)]

# Datetimes to datetime format
records["timestamp"] = convertToDatetime(records, "timestamp")
visitors["ts_first"] = convertToDatetimeVar2(visitors, "ts_first")
visitors["ts_latest"] = convertToDatetimeVar2(visitors, "ts_latest")



#################################
### DETECTION OF ILLEGAL DATA ###
#################################

# Remove illegal answers. At this time any value 99 is deemed illegal
delList = []
invalid = 0

for idx, (value, value2) in enumerate(zip(records["parktime"], records["walktime"])):
    if (value == 99 or value2 == 99):
        print("Illegal walktime or parktime detected:", idx, value, value2)
        delList.append(idx)
        invalid += 1
        
records = records.drop(records.index[delList])
records = records.reset_index()

# Anonymisation of ip addresses
for ip in records.ip:
    records = records.replace(to_replace = ip, value = anonymise()) 




#################################
### ADD DATA TO GEODATAFRAMES ###
#################################

postal["answer_count"] = 0

# Add answer count to postal.answer_count
for zipcode in records.zipcode:
    for idx, postal_zip in enumerate(postal.posti_alue):
        if postal_zip == zipcode:
            postal.loc[idx, "answer_count"] += 1

### add parktime and walktime means
postal["parktime_mean"] = 0
postal["walktime_mean"] = 0

# Add mean to postal.parktime_mean
for idx, postal_zip in enumerate(postal.posti_alue):
    this_mean = records.loc[
            records['zipcode'] == postal_zip]["parktime"].mean()
    postal.loc[idx, "parktime_mean"] = round(this_mean, 2)

# Add mean to postal.walktime_mean
for idx, postal_zip in enumerate(postal.posti_alue):
    this_mean = records.loc[
            records['zipcode'] == postal_zip]["walktime"].mean()
    postal.loc[idx, "walktime_mean"] = round(this_mean, 2)


###################################
### PREPARE AND SHOW STATISTICS ###
###################################
    
statistics = Stats(records, postal, visitors, invalid)
statistics.calculateStats()

# Tells us how many records each user has sent to me
#average_answers = records.groupby('ip', as_index=False)['id'].count()
#answer_sum = postal["answer_count"].sum()
#answer_mean = round(postal["answer_count"].mean(), 3)
#areasWithAnswers = postal["answer_count"][postal["answer_count"]!=0].count()
#areasWithout = list(postal["nimi"][postal["answer_count"]==0])
#areasMoreThanHundred = len(postal[postal["answer_count"] >= 100])
#areasMoreThanTen = len(postal[postal["answer_count"] >= 10]) - areasMoreThanHundred
#areasMoreThanOne = len(postal[postal["answer_count"] >= 1]) - areasMoreThanTen - areasMoreThanHundred
#uniqueVisitors = np.count_nonzero(visitors["ip"].unique())
#visitorcount_mean = round(visitors["count"].mean(), 3)
#visitors_returned = visitors["count"][visitors["count"] > 1].count()
#uniqueRecords = np.count_nonzero(records["ip"].unique())

### PRINT STATISTICS
#print("-----------")
#print("---Stats---")
#print("-----------")
#print("Total answers: {0}".format(answer_sum))
#print("Areas with answers: {0}".format(areasWithAnswers))
#print("Areas without answers: {0}".format(167 - areasWithAnswers))
#print("Answers per area, mean: {0}".format(answer_mean))
#print("Unanswered areas: {0}".format(areasWithout))
#print("Areas with more than 100 answers: {0}".format(areasMoreThanHundred))
#print("Areas with  10-99 answers: {0}".format(areasMoreThanTen))
#print("Areas with  1-9 answers: {0}".format(areasMoreThanOne))
#print("Amount of unique IP addresses (visitors): {0}".format(
#        uniqueVisitors))
#print("Amount of visitors returned once or more: {0}".format(visitors_returned))
#print("How many times visited, mean: {0}".format(visitorcount_mean))
#print("Amount of unique IP addresses of responses (records): {0}".format(
#        uniqueRecords))
#print("Number of invalid records not taken into account: {0}".format(invalid))
##sama tulema kuin yllä
##len(set(records['ip']).intersection(set(visitors['ip'])))
#print("{0} % of visitors sent me records".format(
#        round(uniqueRecords / uniqueVisitors * 100, 2)))

#print("Mean amount of received records per user: {0}". format(
#        round(average_answers["id"].mean(), 2)))



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
#https://towardsdatascience.com/lets-make-a-map-using-geopandas-pandas-and-matplotlib-to-make-a-chloropleth-map-dddc31c1983d

# prepare annotation
postal['coords'] = postal['geometry'].apply(lambda x: x.representative_point().coords[:])
postal['coords'] = [coords[0] for coords in postal['coords']]

# Plot with layers. Base is basemap for zipcodes without answers
base = postal.plot(linewidth=0.8, edgecolor="0.8", color="white", 
                   figsize=(15, 12))

# then remove all zip codes with no answers
#postal = postal.loc[postal["answer_count"]!=0]

# now plot all non-zero areas on top of base
postal.loc[postal["answer_count"]!=0].plot(
        ax=base, column="answer_count", cmap="OrRd", linewidth=0.8,
        figsize=(15, 12), edgecolor="0.8", scheme='fisher_jenks',
        legend=True)

# annotate
for idx, row in postal.iterrows():
    annotation = "{0}, {1}".format(row['nimi'], str(row["answer_count"]))
    #annotation = "{0}".format(str(row["answer_count"]))
    plt.annotate(s=annotation, xy=row['coords'],
                 horizontalalignment='center')

plt.tight_layout()




# en saa asennettuu geoplottii
#ax = geoplot.kdeplot(injurious_collisions.sample(1000),
#                     shade=True, shade_lowest=False,
#                     clip=boroughs.geometry)
#geoplot.polyplot(boroughs, ax=ax)



# PLOT WALKTIME MEAN
base = postal.plot(linewidth=0.8, edgecolor="0.8", color="white", 
                   figsize=(15, 12))

postal.loc[~postal["walktime_mean"].isnull()].plot(
        ax=base, column="walktime_mean", cmap="OrRd", linewidth=0.8,
        figsize=(15, 12), edgecolor="0.8", scheme='fisher_jenks',
        legend=True)

# annotate
for idx, row in postal.iterrows():
    annotation = "{0}, {1}".format(row['nimi'], str(row["walktime_mean"]))
    #annotation = "{0}".format(str(row["answer_count"]))
    plt.annotate(s=annotation, xy=row['coords'],
                 horizontalalignment='center')

plt.tight_layout()




# PLOT PARKTIME MEAN
base = postal.plot(linewidth=0.8, edgecolor="0.8", color="white", 
                   figsize=(15, 12))

postal.loc[~postal["parktime_mean"].isnull()].plot(
        ax=base, column="parktime_mean", cmap="OrRd", linewidth=0.8,
        figsize=(15, 12), edgecolor="0.8", scheme='fisher_jenks',
        legend=True)

# annotate
for idx, row in postal.iterrows():
    annotation = "{0}, {1}".format(row['nimi'], str(row["parktime_mean"]))
    #annotation = "{0}".format(str(row["answer_count"]))
    plt.annotate(s=annotation, xy=row['coords'],
                 horizontalalignment='center')

plt.tight_layout()





#visitors datetime chart, v2, annotate important events
fig, ax = plt.subplots()
ax.plot_date(visitors["ts_first"], visitors.index)
ax.annotate('local max', xy=("2019-05-22", 191), xytext=("2019-05-30", 1500),
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


# merkitsevyys scatter plot
# outo?
###########################
fig, ax = plt.subplots(figsize=(8,8))
ax.scatter(records["timeofday"], records["parktime"])
plt.grid(True)
plt.tight_layout()
plt.show()
