# -*- coding: utf-8 -*-
"""
Created on Sun May 12 22:54:34 2019

@author: Sampo Vesanen

Functions for thesis data crunch
"""

# Import functions
import geopandas as gpd
import math
import numpy as np
import pandas as pd 
import matplotlib.pyplot as plt
from descartes import PolygonPatch
from shapely.geometry import Point, MultiPoint
from shapely.ops import cascaded_union
from matplotlib.offsetbox import (TextArea, DrawingArea, OffsetImage,
                                  AnnotationBbox)
import random

def plot_polygon(polygonList):
    '''
    Insert Polygons in lists! If only one Polygon, then use list with one
    value, for example plot_polygon([Polygon]).
    
    Code by Kevin Dwyer, HumanGeo blog. Edited by Sampo Vesanen
    View alpha shape polygons in pylab
    
    An useful general Polygon viewing tool, just stack them and view them
    
    Input       List of Polygons
    Returns     matplotlib view
    '''
    
    def createPolygonPatch(pol, alpha):
        '''
        Declutter code, create PolygonPatch inside this function. For now
        the function only accepts the input Polygon and a value for transparency.
        '''
        thisPatch = PolygonPatch(pol, 
                         fc=np.random.rand(3,), # each object gets a random color
                         ec=np.random.rand(3,), 
                         fill=True,
                         alpha=alpha,
                         zorder=-1)
        return thisPatch
    
    # keeps track if Point is detected
    polIsPoint = 0
    
    # Allow user to forget the use of unary_union when building list of
    # GeoDataFrames. If item in loop is instance GeoDataFrame, perform
    # unary_union on it, else don't make changes.
    polygonList = [item.unary_union if isinstance(item, gpd.GeoDataFrame) else \
                   item for item in polygonList]
    
    fig = plt.figure(figsize=(14,12))
    ax = fig.add_subplot(111)
    margin = 1000 # Give resuls more space. Original was .3
    x_min, y_min, x_max, y_max = cascaded_union(polygonList).bounds
    envArea = cascaded_union(polygonList).envelope.area
    ax.set_xlim([x_min - margin, x_max + margin])
    ax.set_ylim([y_min - margin, y_max + margin])
    
    # Prevents Polygon distortion in matplotlib window
    ax.axes.set_aspect("equal")
    
    for pol in polygonList:
        # allow Points and MultiPoints in plot_polygon(). Also implemented 
        # cascaded union envelope size based Point buffer
        if isinstance(pol, (Point, MultiPoint)):
            # if envArea is zero, buffer is 10, otherwise formula
            pol = pol.buffer(10 if envArea == 0.0 else 20 * math.log(envArea))
            polIsPoint = 1

        thisPatch = createPolygonPatch(pol, 1)
        ax.add_patch(thisPatch)
        
        # insert halo of sorts for Points for better visibility
        if polIsPoint == 1 and envArea != 0:
            bgpol = createPolygonPatch(pol.buffer(1000), 0.5)
            ax.add_patch(bgpol)
            polIsPoint = 0
            
    plt.tight_layout()

    return fig



def plot_polygons(dictionary):
    '''
    Plot as many Polygons as needed. All presented in one figure. For testing
    purposes. Pretty useful for checking out multiple Polygons fast.
    
    # Function expects dictionaries with following logic. facecolor and 
    # edgecolor are to be presented in hex
    # {"polygonName": "facecolor_edgecolor_alpha_fillboolean"}
    
    Input       a dictionary in the format presented above
    Returns     Matplotlib map view
    
    '''
    
    def testEval(str):
        '''
        Check if string has # in front. If no, then eval()
        
        Input       a string
        Returns     eval'ed string or the same as input
        '''
        if str[:1] != "#":
            return eval(str)
        
        else:
            return str
    
    
    fig = plt.figure(figsize=(14,12))
    ax = fig.add_subplot(111)
    margin = 1000 # Give resuls more space. Original was .3
    
    # Get all Polygons as shapely features in list, then cascaded_union and 
    # bounds
    x_min, y_min, x_max, y_max = cascaded_union(
            [eval(pol) for pol in dictionary]).bounds
    
    ax.set_xlim([x_min-margin, x_max+margin])
    ax.set_ylim([y_min-margin, y_max+margin])
    
    # prevents Polygon distortion in matplotlib window
    ax.axes.set_aspect("equal") 
    
    for pol, allsettings in dictionary.items():
        # Split settings of a Polygon to parts
        theseSettings = allsettings.split("_") 
        
        # allow Points in plot_polygons() NOT TESTED
        #if isinstance(pol, Point):
        #    pol = pol.centroid.buffer(8)
        
        thisPatch = PolygonPatch(eval(pol), 
                                 fc=testEval(theseSettings[0]), # facecolor
                                 ec=testEval(theseSettings[1]), # edgecolor
                                 alpha=eval(theseSettings[2]), # opacity
                                 fill=eval(theseSettings[3]), # fill boolean
                                 zorder=-1)
        ax.add_patch(thisPatch)
    
    return fig



def parkingPlot(df, column, zeroAllowed):
    '''
    Plot results as function. Functionally it is quite limited.
    '''
    # Plot with layers. Base is basemap for zipcodes without answers
    base = df.plot(linewidth=0.8, 
                   edgecolor="0.8", 
                   color="white", 
                   figsize=(24, 12))

    if zeroAllowed == 0:
        # now plot all non-zero areas on top of base
        df.loc[df[column]!=0].plot(
                ax=base, 
                column=column, 
                cmap="OrRd", 
                linewidth=0.8,
                figsize=(24, 12), 
                edgecolor="0.8", 
                scheme='fisher_jenks',
                legend=True)
    else:
        # zero allowed but null is forbidden
        df.loc[~df[column].isnull()].plot(
                ax=base, 
                column=column, 
                cmap="OrRd", 
                linewidth=0.8,
                figsize=(24, 12), 
                edgecolor="0.8", 
                scheme='fisher_jenks',
                legend=True)
    
    # annotate
    annotationFunction(df, column)
    plt.tight_layout()



def annotationFunction(df, rowname):
    '''
    Annotate postal code areas in Matplotlib plot.
    Df is the dataframe to fetch shapes from, rowname gives the value to
    the annotations.
    '''
    for idx, row in df.iterrows():
        annotation = "{0}, {1}".format(row['nimi'], str(row[rowname]))
        plt.annotate(s=annotation, xy=row['coords'],
                     horizontalalignment='center')



def polygonCoordsToTuple(gdf):
    '''
    Shapely Polygons to tuple. Used in annotation
    '''
    geoSeries = gdf["geometry"].apply(lambda x: x.representative_point().coords[:])
    geoSeries = [coords[0] for coords in geoSeries]
    
    return geoSeries



def convertToDatetime(dataframe, columnName):
    '''
    Declutter code
    '''
    return pd.to_datetime(dataframe[columnName], format="%d-%m-%Y %H:%M:%S") 



def convertToDatetimeVar2(dataframe, columnName):
    '''
    Declutter code
    '''
    return pd.to_datetime(dataframe[columnName], format="%Y-%m-%d %H:%M:%S")



def detect_outlier(data):
    """
    Input may be a list or a Series.
    
    1) We write a function that takes numeric data as an input argument.
    2) We find the mean and standard deviation of the all the data points
    3) We find the z score for each of the data point in the dataset and if the 
    z score is greater than 3 than we can classify that point as an outlier. 
    Any point outside of 3 standard deviations would be an outlier.
    
    Adapted from code by Renu Khandelwal:
    https://medium.com/datadriveninvestor/finding-outliers-in-dataset-using-python-efc3fce6ce32
    """
    outliers = []
    threshold = 3
    mean_1 = np.mean(data)
    std_1 = np.std(data)
    
    # adding idx enables us to keep track of outlier value position in a
    # dataframe. Return answers as a list of tuples
    for idx, value in enumerate(data):
        z_score = (value - mean_1) / std_1 
        if np.abs(z_score) > threshold:
            outliers.append((idx, value))
            
    return outliers



class Stats:
    '''
    Calculate statistics and print
    '''
    def __init__(self, records, postal, visitors, invalid):
        self.records = records
        self.postal = postal
        self.visitors = visitors
        self.invalid = invalid
    
    def calculateStats(self):
        '''
        Perform calculations and show them.
        '''
        # Total answers
        self.average_answers = self.records.groupby(
                'ip', as_index=False)['id'].count()
        
        # Amount of areas with answers
        self.answer_sum = self.postal[
                "answer_count"].sum()
        
        # Amount of areas without answers
        self.answer_mean = round(
                self.postal["answer_count"].mean(), 3)
        
        # Answers per area, mean
        self.areasWithAnswers = self.postal[
                "answer_count"][self.postal["answer_count"]!=0].count()
        
        # Names of unanswered areas
        self.areasWithout = list(
                self.postal["nimi"][self.postal["answer_count"]==0])
        
        # Amount of areas with more than 100 answers
        self.areasMoreThanHundred = len(
                self.postal[self.postal["answer_count"] >= 100])
        
        # Amount of areas with 10-99 answers
        self.areasMoreThanTen = len(self.postal[
                self.postal["answer_count"] >= 10]) - self.areasMoreThanHundred
        
        # Amount of areas with 1-9 answers
        self.areasMoreThanOne = len(self.postal[
                self.postal["answer_count"] >= 1]) - self.areasMoreThanTen - self.areasMoreThanHundred
        
        # Amount of total visits
        self.totalVisits = self.visitors["count"].sum()
        
        # Amount of unique IP addresses in visitors
        self.uniqueVisitors = np.count_nonzero(
                self.visitors["ip"].unique())
        
        # How many times visited, mean
        self.visitorcount_mean = round(
                self.visitors["count"].mean(), 3)
        
        # Amount of visitors returned more than once
        self.visitors_returned = self.visitors[
                "count"][self.visitors["count"] > 1].count()
        
        # Amount of unique IP addresses of responses in records
        self.uniqueRecords = np.count_nonzero(
                self.records["ip"].unique())
        
        # TOP 10 most answers per user
        self.topten = list(self.records['ip'].value_counts().nlargest(10))
        
        # Show stats
        self.printStats()
        
    def printStats(self):
        '''
        Print statistics. Does not need to be run by itself.
        '''
        print("\n\n-----------")
        print("---Stats---")
        print("-----------")
        print("Total answers: {0}".format(
                self.answer_sum))
        print("Areas with answers: {0}".format(
                self.areasWithAnswers))
        print("Areas without answers: {0}".format(
                167 - self.areasWithAnswers)) # 167 is the amount of zip codes
        print("Answers per area, mean: {0}".format(
                self.answer_mean))
        print("Unanswered areas: {0}".format(
                self.areasWithout))
        print("Areas with more than 100 answers: {0}".format(
                self.areasMoreThanHundred))
        print("Areas with 10-99 answers: {0}".format(
                self.areasMoreThanTen))
        print("Areas with 1-9 answers: {0}".format(
                self.areasMoreThanOne))
        print("Total amount of visits: {0}".format(
                self.totalVisits))
        print("Amount of unique IP addresses (visitors): {0}".format(
                self.uniqueVisitors))
        print("Amount of visitors returned once or more: {0}".format(
                self.visitors_returned))
        print("How many times visited, mean: {0}".format(
                self.visitorcount_mean))
        print("Amount of unique IP addresses of responses (records): {0}".format(
                self.uniqueRecords))
        print("Number of invalid records not taken into account: {0}".format(
                self.invalid))
        #sama tulema kuin yllä
        #len(set(records['ip']).intersection(set(visitors['ip'])))
        print("{0} % of visitors sent me records".format(
                round(self.uniqueRecords / self.uniqueVisitors * 100, 2)))
        
        print("Mean amount of received records per user: {0}". format(
                round(self.average_answers["id"].mean(), 2)))
        print("TOP 10 most answers per user: {0}".format(
                self.topten))
        


def identicaltest(x):
    '''
    This anonymous function is to be used with groupby objects. It will
    determine from Pandas Series whether the values in that series are
    identical or unidentical. Return string answer or list.
    '''
    return "identical" if len(set(x)) == 1 else x.tolist()



def getJenksBreaks(dataList, numClass):
  
  # This code is adapted from GitHub user Drew Dara-Abrams, at
  # https://gist.github.com/drewda/1299198
  # 
  # The function requires a list of values to be analysed and number of
  # classes required by user.
  #
  # Drewda himself took this code from following nonfunctional web addresses:
  # Code from http://danieljlewis.org/files/2010/06/Jenks.pdf
  # Described at http://danieljlewis.org/2010/06/07/jenks-natural-breaks-algorithm-in-python/
     
  dataList.sort()
  mat1 = []
  
  for i in range(0, len(dataList) + 1):
    temp = []
    for j in range(0, numClass + 1):
      temp.append(0)
    mat1.append(temp)
    
  mat2 = []
  
  for i in range(0, len(dataList) + 1):
    temp = []
    for j in range(0, numClass + 1):
      temp.append(0)
    mat2.append(temp)
    
  for i in range(1, numClass + 1):
    mat1[1][i] = 1
    mat2[1][i] = 0
    for j in range(2, len(dataList) + 1):
      mat2[j][i] = float('inf')
      
  v = 0.0
  
  for l in range(2, len(dataList) + 1):
    s1 = 0.0
    s2 = 0.0
    w = 0.0
    for m in range(1, l + 1):
      i3 = l - m + 1
      val = float(dataList[i3 - 1])
      s2 += val * val
      s1 += val
      w += 1
      v = s2 - (s1 * s1) / w
      i4 = i3 - 1
      if i4 != 0:
        for j in range(2, numClass + 1):
          if mat2[l][j] >= (v + mat2[i4][j - 1]):
            mat1[l][j] = i3
            mat2[l][j] = v + mat2[i4][j - 1]
    mat1[l][1] = 1
    mat2[l][1] = v
    
  k = len(dataList)
  kclass = []
  
  for i in range(0, numClass + 1):
    kclass.append(0)
    
  kclass[numClass] = float(dataList[len(dataList) - 1])
  countNum = numClass
  
  while countNum >= 2:#print "rank = " + str(mat1[k][countNum])
    id = int((mat1[k][countNum]) - 2)
    #print "val = " + str(dataList[id])
    kclass[countNum - 1] = dataList[id]
    k = int((mat1[k][countNum] - 1))
    countNum -= 1
    
  return kclass



def random_color():
    '''
    Get a light random color for matplotlib
    
    Code adapted from Stack Overflow user Yakir Tsuberi:
    https://stackoverflow.com/a/50906743/9455395
    '''
    
    rand = lambda: random.randint(150, 255)
    return "#%02X%02X%02X" % (rand(), rand(), rand())



def travelTimeComparison(listOfTuples, ttmpath, detect_outliers=False,
                         printStats=False, plotIds=False):
    '''
    Compare Travel-Time Matrix 2018 (from here on TTM) data with my Thesis 
    survey data. This function produces a dataframe with one row for each tuple 
    inputted into it. This function calculates many descriptives about these 
    two datasets.
    
    In this thesis we assume:
    - rush hour (car_r_t) = "Weekday, rush hour (07:00-09:00 and 15:00-17:00)", 
    - midday (car_m_t) = "Weekday, other than rush hour"
    - entire travel time with speed limits, no other impedances (car_sl_t) = 
        all parktime data as averaged
        
    Additional note: If thesis_r_ or thesis_m_ is nan, it means that this
    destination does not have data for that timeofday option. This is more
    likely to happen in postal code areas with low amounts of responses.
    
    If car_r_t, car_m_t and car_sl_t are nan, it means that there is no way
    to reach the destination from the origin.
    
    Resulting columns in the result dataframe are as follows:
        "from_id"               YKR ID of origin
        "from_name"             Postal area name of origin
        "to_id"                 YKR ID of destination
        "to_name"               Postal area name of destination
        "car_r_t"               TTM: entire travel time in rush hour traffic
                                (minutes)
        "car_m_t"               TTM: entire travel time in midday traffic
                                (minutes)
        "car_sl_t"              TTM: entire travel time following speed limits
                                without any additional impedances (minutes)
        "car_r_drivetime"       TTM: entire travel time in rush hour traffic 
                                minus searching for parking (SFP: 0.42min)
                                (minutes)
        "car_m_drivetime"       TTM: entire travel time in midday traffic minus
                                searching for parking (SFP: 0.42min) (minutes)
        "car_sl_drivetime"      TTM: entire travel time following speed limits
                                without any additional impedances minus
                                searching for parking (SFP: 0.42min) (minutes)
        "car_r_pct"             TTM: how many percent is SFP of car_r_t
                                (percent)
        "car_m_pct"             TTM: how many percent is SFP of car_m_t  
                                (percent)
        "car_sl_pct"            TTM: how many percent is SFP of car_sl_t
                                (percent)
        "values_in_dest"        Thesis: amount of records in destination postal
                                code area
        "thesis_r_sfp"          Thesis: searching for parking in destination
                                postal code area in rush hour traffic
                                (minutes)
        "thesis_m_sfp"          Thesis: SFP in destination postal code area
                                in midday traffic (minutes)
        "thesis_sl_sfp"         Thesis: SFP in destination postal code area
                                generally (all parktime values averaged)
                                (minutes)
        "thesis_r_pct"          Thesis: how many percent is thesis SFP of
                                TTM entire travel time in rush hour traffic
                                (percent)
        "thesis_m_pct"          Thesis: how many percent is thesis SFP of
                                TTM entire travel time in midday traffic 
                                (percent)
        "thesis_sl_pct"         Thesis: how many percent is thesis SFP of
                                TTM entire travel time following speed limits
                                without any additional impedances (percent)
        "thesis_r_drivetime"    TTM: Entire travel time in rush hour traffic
                                minus Thesis data rush hour SFP (minutes)
        "thesis_m_drivetime"    TTM: Entire travel time in midday traffic minus
                                thesis data midday SFP (minutes)
        "thesis_sl_drivetime"   TTM: Entire travel time following speed limits
                                without any additional impedances minus thesis
                                data general (all values averaged) SFP (minutes)
    '''
    
    result = pd.DataFrame(
            columns=["from_id", "from_name", "to_id", "to_name", "car_r_t", 
                     "car_m_t", "car_sl_t", "car_r_drivetime", "car_m_drivetime", 
                     "car_sl_drivetime", "car_r_pct", "car_m_pct", "car_sl_pct", 
                     "values_in_dest", "thesis_r_sfp", "thesis_m_sfp", 
                     "thesis_sl_sfp", "thesis_r_pct", "thesis_m_pct", 
                     "thesis_sl_pct", "thesis_r_drivetime", "thesis_m_drivetime", 
                     "thesis_sl_drivetime"])
    template = result.copy()
    template.loc[0] = 0
    
    if plotIds == True:
        # background layers for matplotlib. Use these to have all origin and
        # destination pairs on the same map
        base = grid.plot(linewidth=0.8, 
                         edgecolor="0.8", 
                         color="white",
                         figsize=(16, 12))
        forest.plot(ax=base,
                    linewidth=0.8,
                    edgecolor="none",
                    facecolor="green")
        postal.plot(ax=base,
                    linewidth=0.8,
                    edgecolor="black",
                    facecolor="none")

    # Iterate through all ids inputted by user
    for originId, destinationId in list(listOfTuples):
        
        # This code is meant to compare 2018 Travel-Time Matrix results to my 
        # results. Get origin and destination points from TTM data, then 
        # compare to zipcodes mean in my data
        
        # copy dataframe template to produce this row of data
        thisRow = template.copy()

        # Origin and destinations are read from list of tuples. Read relevant 
        # text file location from "ttm_path"
        traveltimepath = ttm_path.format(destinationId[:4], destinationId)
        
        # Get destination text file. Remove all columns not about private cars
        destfile = pd.read_csv(traveltimepath, sep=";")
        destfile["from_id"] = pd.to_numeric(destfile["from_id"])
        delCol = list(destfile.columns[2:13])
        destfile = destfile.drop(delCol, axis=1)
        
        # Slice destination and origin from GeoDataFrame grid. 
        dest = grid.loc[grid["YKR_ID"] == int(destinationId)].reset_index()
        orig = grid.loc[grid["YKR_ID"] == int(originId)].reset_index()
        
        # ID data to current dataframe row
        thisRow.loc[0, "from_id"] = originId
        thisRow.loc[0, "to_id"] = destinationId
        thisRow.loc[0, "from_name"] = postal.loc[
                postal.intersects(orig.geometry[0]), "nimi"].values[0]
        thisRow.loc[0, "to_name"] = postal.loc[
                postal.intersects(dest.geometry[0]), "nimi"].values[0]
        
        # Convert dest and orig geometry to Point for later use in plotting.
        # If this is done earlier, the naming of the zipcode above can fail
        # in cases
        dest["geometry"] = dest.centroid
        orig["geometry"] = orig.centroid
        
        # Get TTM2018 data for the origin
        # Match origin and destination
        car = destfile.loc[destfile["from_id"] == int(originId)].reset_index()
        car = car.loc[car.index[0]]

        # Get all thesis survey data about destination postal code area
        thisZipcode = records.loc[records.zipcode == dest.zipcode[0]]
        
        # Travel-time Matrix 2018, entire travel times. Detect nodata values
        # (-1) and assign np.nan if detected.
        thisRow.loc[0, "car_r_t"] = np.nan if car[3] == -1 else car[3]
        thisRow.loc[0, "car_m_t"] = np.nan if car[5] == -1 else car[5]
        thisRow.loc[0, "car_sl_t"] =  np.nan if car[7] == -1 else car[7]
        thisRow.loc[0, "values_in_dest"] = len(thisZipcode)
        car_r_t = thisRow.loc[0, "car_r_t"]
        car_m_t = thisRow.loc[0, "car_m_t"] 
        car_sl_t = thisRow.loc[0, "car_sl_t"]
        values_in_dest = thisRow.loc[0, "values_in_dest"]
        
        # Searching for parking for thesis survey data, in destination postal
        # code area. In this phase we optionally test if there are any outliers
        # in parktime values. This can be helpful to prevent seemingly erroneous
        # data from distorting a series of values in a postal code area with
        # low amount of responses.
        
        # Record all values
        parktime1 = thisZipcode.loc[thisZipcode.timeofday == 1]["parktime"]
        parktime2 = thisZipcode.loc[thisZipcode.timeofday == 2]["parktime"]
        parktime_all = thisZipcode["parktime"]
        
        if detect_outliers == True:
            
            # only detect outliers if enough values
            if len(parktime1) > 3:
                outliers = detect_outlier(parktime1)
                
                # report to user if outliers found
                if len(outliers) > 0:
                    print("Outliers detected in rush hour for destinationId {0}: {1}"
                          .format(destinationId, outliers))
                
                # Iteratively remove outliers from current Series
                for idx, value in outliers:
                    parktime1 = parktime1[parktime1.index != idx]
                
            if len(parktime2) > 3:
                outliers = detect_outlier(parktime2)
                if len(outliers) > 0:
                    print("Outliers detected in midday traffic for destinationId {0}: {1}"
                          .format(destinationId, outliers))
                for idx, value in outliers:
                    parktime2 = parktime2[parktime2.index != idx]
                    
            if len(parktime_all) > 3:
                outliers = detect_outlier(parktime_all)
                if len(outliers) > 0:
                    print("Outliers detected in parktime_all for destinationId {0}: {1}"
                          .format(destinationId, outliers))
                for idx, value in outliers:
                    parktime_all = parktime_all[parktime_all.index != idx]

        thisRow.loc[0, "thesis_r_sfp"] = round(parktime1.mean(), 2)
        thisRow.loc[0, "thesis_m_sfp"] = round(parktime2.mean(), 2)
        thisRow.loc[0, "thesis_sl_sfp"] = round(parktime_all.mean(), 2)
        
        thesis_r_sfp =  thisRow.loc[0, "thesis_r_sfp"]
        thesis_m_sfp = thisRow.loc[0, "thesis_m_sfp"]
        thesis_sl_sfp = thisRow.loc[0, "thesis_sl_sfp"]
        

        # Travel-Time Matrix 2018 travel times minus Travel-Time Matrix 
        # searching for parking, using default value 0.42 mins
        ttm_sfp = 0.42
        thisRow.loc[0, "car_r_drivetime"] = car_r_t - ttm_sfp
        thisRow.loc[0, "car_m_drivetime"] = car_m_t - ttm_sfp
        thisRow.loc[0, "car_sl_drivetime"] = car_sl_t - ttm_sfp
        car_r_drivetime = thisRow.loc[0, "car_r_drivetime"]
        car_m_drivetime = thisRow.loc[0, "car_m_drivetime"]
        car_sl_drivetime = thisRow.loc[0, "car_sl_drivetime"]
        
        # Of Travel-Time Matrix data, how many percent searching for parking 
        # constituted of the total travel time?
        thisRow.loc[0, "car_r_pct"] = round(ttm_sfp / car_r_t, 3)
        thisRow.loc[0, "car_m_pct"] = round(ttm_sfp / car_m_t, 3)
        thisRow.loc[0, "car_sl_pct"] = round(ttm_sfp / car_sl_t, 3)
        car_r_pct = thisRow.loc[0, "car_r_pct"]
        car_m_pct = thisRow.loc[0, "car_m_pct"]
        car_sl_pct = thisRow.loc[0, "car_sl_pct"]
        
        # According to thesis SFP data, How many percent searching for parking 
        # constituted of the total travel time (TTM data)?
        thisRow.loc[0, "thesis_r_pct"] = round(thesis_r_sfp / car_r_t, 3)
        thisRow.loc[0, "thesis_m_pct"] = round(thesis_m_sfp / car_m_t, 3)
        thisRow.loc[0, "thesis_sl_pct"] = round(thesis_sl_sfp / car_sl_t, 3)
        thesis_r_pct = thisRow.loc[0, "thesis_r_pct"]
        thesis_m_pct = thisRow.loc[0, "thesis_m_pct"]
        thesis_sl_pct = thisRow.loc[0, "thesis_sl_pct"]
        
        # Travel-Time Matrix 2018 travel times minus thesis survey data
        # searching for parking, using values for rush hour, midday and general,
        # respectively
        thisRow.loc[0, "thesis_r_drivetime"] = car_r_t - thesis_r_sfp
        thisRow.loc[0, "thesis_m_drivetime"] = car_m_t - thesis_m_sfp
        thisRow.loc[0, "thesis_sl_drivetime"] = car_sl_t - thesis_sl_sfp
        thesis_r_drivetime = thisRow.loc[0, "thesis_r_drivetime"]
        thesis_m_drivetime = thisRow.loc[0, "thesis_m_drivetime"]
        thesis_sl_drivetime = thisRow.loc[0, "thesis_sl_drivetime"]
        
        # append all gathered results to the df result
        result = result.append(thisRow, sort=False)
        
        if printStats == True:
            print("Origin is located in postal code area {0}. Destination in {1}"
                  .format(thisRow.loc[0, "from_name"], thisRow.loc[0, "to_name"]))
            print("\n--- Travel time matrix 2018 ----")
            print("\nEntire travel time in rush hour traffic: {0} min"
                  .format(car_r_t))
            print("Entire travel time in midday traffic: {0} min"
                  .format(car_m_t))
            print("Entire travel time following speed limits without any additional impedances: {0} min"
                  .format(car_sl_t))
            
            print("\n--- Inferred facts from TTM 2018 ---")
            print("\nSearching for parking is 0.42 minutes in this context")
            print("\nEntire travel time in rush hour traffic minus searching for parking: {0} min"
                  .format(car_r_drivetime))
            print("-- SFP represents {0} % of total travel time in rush hour"
                  .format(car_r_pct * 100))
            print("Entire travel time in midday traffic minus searching for parking: {0} min"
                  .format(car_m_drivetime))
            print("-- SFP represents {0} % of total travel time in midday traffic"
                  .format(car_m_pct * 100))
            print("Entire travel time in speed limits minus searching for parking: {0} min"
                  .format(car_sl_drivetime))
            print("-- SFP represents {0} % of total travel time in speed limits"
                  .format(car_sl_pct * 100))
            
            print("\n --- Sampo Vesanen thesis ---")
            if values_in_dest < 20:
                print("Warning, low amount (< 20) of responses in origin zipcode: {0}"
                      .format(values_in_dest))
            if len(detect_outlier(list(thisZipcode.parktime))) != 0:
                print("Additional warning, possible outliers detected in origin parktime data: {0}"
                      .format(detect_outlier(list(thisZipcode.parktime))))
            print("\nParktime (Searching for parking) in rush hour traffic: {0} min"
                  .format(thesis_r_sfp))
            print("Parktime (SFP) in midday traffic: {0} min"
                  .format(thesis_m_sfp))
            print("Parktime (SFP) generally: {0} min".format(thesis_sl_sfp))
            print("\nEntire travel time in rush hour traffic minus thesis data searching for parking: {0} min"
                  .format(thesis_r_drivetime))
            print("-- SFP represents {0} % of total travel time in rush hour"
                  .format(thesis_r_pct * 100))
            print("Entire travel time in midday traffic minus thesis data searching for parking: {0} min"
                  .format(thesis_m_drivetime))
            print("-- SFP represents {0} % of total travel time in midday traffic"
                  .format(thesis_m_pct * 100))
            print("Entire travel time following speed limits without any additional impedances minus thesis data searching for parking: {0} min"
                  .format(thesis_sl_drivetime))
            print("-- SFP represents {0} % of total travel time when following speed limits\n\n"
                  .format(thesis_sl_pct * 100))
        
        # Plot origin and destination
        if plotIds == True:
            # background layers for matplotlib. Activate these to get 
            # individual maps for each origin and destination pairs.
            #base = grid.plot(linewidth=0.8, 
            #                 edgecolor="0.8", 
            #                 color="white",
            #                 figsize=(16, 12))
            #forest.plot(ax=base,
            #            linewidth=0.8,
            #            edgecolor="none",
            #            facecolor="green")
            #postal.plot(ax=base,
            #            linewidth=0.8,
            #            edgecolor="black",
            #            facecolor="none")

    
            # For loop for plotting origin and destination on map. Prepare
            # annotation with these lists
            identifierlist = ["Origin: ", "Destination: "]
            namelist = [thisRow.from_name[0], thisRow.to_name[0]]
            ykrlist = ["\nYKR-ID: " + str(orig.YKR_ID.item()), 
                       "\nYKR-ID: " + str(dest.YKR_ID.item())]
            
            # Random light color for annotation bbox background to identify 
            # dataframe rows from each other
            rnd_col = random_color()
            
            for item, identifier, name, ykr in zip([orig, dest], identifierlist, 
                                                   namelist, ykrlist):
                # Annotation coordinates to tuple
                item["coords"] = polygonCoordsToTuple(item)
                item.plot(ax=base)
                
                # Annotate with a text box
                # Show acknowledgement in annotationbbox if destination is not 
                # reachable from origin (TTM18 nodata value -1 changed to np.nan)
                if np.isnan(car_r_t):
                    ykr = ykr + "\nNodata, route not navigable"
                anno = identifier + name + ykr
                
                offsetbox = TextArea(anno, minimumdescent=False)
                ab = AnnotationBbox(offsetbox, item["coords"][0],
                                    xybox=(-20, 40),
                                    xycoords="data",
                                    boxcoords="offset points",
                                    bboxprops=dict(facecolor=rnd_col,
                                                   boxstyle="round"),
                                    arrowprops=dict(arrowstyle="->"))

                base.add_artist(ab)
            
            plt.tight_layout()

    result = result.reset_index()
    result = result.drop(columns=["index"])
    
    return result