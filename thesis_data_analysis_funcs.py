# -*- coding: utf-8 -*-
"""
Created on Sun May 12 22:54:34 2019

@author: Sampo Vesanen

Functions for thesis data processing. Find essential functions at the top.
"""

# Import functions
import geopandas as gpd
import math
import numpy as np
import pandas as pd 
import matplotlib.pyplot as plt
from descartes import PolygonPatch
from shapely.geometry import Point, MultiPoint, Polygon, MultiPolygon
from shapely.ops import cascaded_union
from matplotlib.offsetbox import (TextArea, DrawingArea, OffsetImage,
                                  AnnotationBbox)
import random
from heapq import nlargest


def preserve_nlargest(inputgeom, n):
    '''
    Insert a MultiPolygon in a GeoSeries object, get n largest Polygons out.
    Utilises nlargest from heapq.
    '''
    result = nlargest(n, inputgeom.geometry, key=lambda a: a.area)
    result = MultiPolygon(result)
    
    return result 



def spatialIndexFunc(s_index, s_index_source, thisGeom):
    '''
    Employ spatial index in a for loop. This function assumes you have already
    set up a SpatialIndex and entered for loop. Use this to get the precise
    matches for a specific Shapely geometry.
    
    s_index             your SpatialIndex
    s_index_source      the GeoDataFrame which was used to create s_index
    thisGeom            Shapely geometry
    '''
    
    possible_matches_idx = list(s_index.intersection(thisGeom.bounds))
    possible_matches = s_index_source.iloc[possible_matches_idx]
    precise_matches = possible_matches[possible_matches.intersects(thisGeom)]
    
    return precise_matches



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
                "ip", as_index=False)["id"].count()
        
        # Amount of areas with answers
        self.answer_sum = self.postal.answer_count.sum()
        
        # Amount of areas without answers
        self.answer_mean = round(self.postal.answer_count.mean(), 3)
        
        # Answers per area, mean
        self.areasWithAnswers = self.postal[
                "answer_count"][self.postal.answer_count!=0].count()
        
        # Quartiles of answers per area
        self.quartiles = self.records.zipcode.value_counts().quantile(
                [0.25, 0.5, 0.75])
        
        # Names of unanswered areas
        self.areasWithout = list(
                self.postal.nimi[self.postal.answer_count==0])
        
        # Amount of areas with more than 100 answers
        self.areasMoreThanHundred = len(
                self.postal[self.postal.answer_count >= 100])
        
        # Amount of areas with 10-99 answers
        self.areasMoreThanTen = len(self.postal[
                self.postal.answer_count >= 10]) - self.areasMoreThanHundred
        
        # Amount of areas with 1-9 answers
        self.areasMoreThanOne = len(self.postal[
                self.postal.answer_count >= 1]) - self.areasMoreThanTen - self.areasMoreThanHundred
        
        # Amount of total visits
        self.totalVisits = self.visitors["count"].sum()
        
        # Amount of unique IP addresses in visitors
        self.uniqueVisitors = np.count_nonzero(self.visitors.ip.unique())
        
        # How many times visited, mean
        self.visitorcount_mean = round(self.visitors["count"].mean(), 3)
        
        # Amount of visitors returned more than once
        self.visitors_returned = self.visitors["count"][
                self.visitors["count"] > 1].count()
        
        # Amount of unique IP addresses of responses in records
        self.uniqueRecords = np.count_nonzero(self.records.ip.unique())
        
        # TOP 10 most answers per user
        self.topten = list(self.records.ip.value_counts().nlargest(10))
        
        # Show stats
        self.printStats()
        
    def printStats(self):
        '''
        Print statistics
        '''
        print("\n\n-----------")
        print("---Stats---")
        print("-----------")
        print(f"Total answers: {self.answer_sum}")
        print(f"Areas with answers: {self.areasWithAnswers}")
        print(f"Areas without answers: {167 - self.areasWithAnswers}") # 167 is the amount of zip codes
        print(f"Answers per area, mean: {self.answer_mean}")
        print(f"Quantiles of answers per area:\n{self.quartiles.to_string()}")
        print(f"Unanswered areas: {self.areasWithout}")
        print(f"Areas with more than 100 answers: {self.areasMoreThanHundred}")
        print(f"Areas with 10-99 answers: {self.areasMoreThanTen}")
        print(f"Areas with 1-9 answers: {self.areasMoreThanOne}")
        print(f"Total amount of visits: {self.totalVisits}")
        print(f"Amount of unique IP addresses (visitors): {self.uniqueVisitors}")
        print("Amount of visitors returned once or more: "
              f"{self.visitors_returned}")
        print(f"How many times visited, mean: {self.visitorcount_mean}")
        print("Amount of unique IP addresses of responses (records): "
              f"{self.uniqueRecords}")
        print("Number of invalid records not taken into account: "
              f"{self.invalid}")
        print(f"{round(self.uniqueRecords / self.uniqueVisitors * 100, 2)} % "
              "of visitors sent me records")
        print("Mean amount of received records per user: "
              f"{round(self.average_answers.id.mean(), 2)}")
        print(f"TOP 10 most answers per user: {self.topten}")
        


def identicaltest(x):
    '''
    This function is to be used with groupby objects. It will determine from 
    pandas Series whether the values in that series are
    identical or unidentical. Return string answer "identical (unique x)" or 
    list.
    '''
    return f"identical ({''.join(str(e) for e in set(x))})" if len(set(x)) == 1 else x.tolist()



def getJenksBreaks(dataList, numClass):
    '''
    This code is adapted from GitHub user Drew Dara-Abrams, at
    https://gist.github.com/drewda/1299198
       
    The function requires a list of values to be analysed and number of
    classes required by user.
      
    Drewda himself took this code from following nonfunctional web addresses:
    Code from http://danieljlewis.org/files/2010/06/Jenks.pdf
    Described at http://danieljlewis.org/2010/06/07/jenks-natural-breaks-algorithm-in-python/
    '''
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
        mat2[j][i] = float("inf")
          
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
  
    while countNum >= 2: #print "rank = " + str(mat1[k][countNum])
      id = int((mat1[k][countNum]) - 2)
      #print "val = " + str(dataList[id])
      kclass[countNum - 1] = dataList[id]
      k = int((mat1[k][countNum] - 1))
      countNum -= 1
    
    return kclass



def niceround(val):
    '''
    Parameters
    ----------
    val : Any value.

    Returns
    -------
     A value transformed to string and rounded to one decimal.
    '''
    return str(round(val, 2))


def polygonCoordsToTuple(gdf):
    '''
    Shapely Polygons to tuple. Used in annotation
    '''
    geoSeries = gdf.geometry.apply(lambda x: x.representative_point().coords[:])
    geoSeries = [coords[0] for coords in geoSeries]
    
    return geoSeries



def random_color():
    '''
    Get a light random color for matplotlib
    
    Code adapted from Stack Overflow user Yakir Tsuberi:
    https://stackoverflow.com/a/50906743/9455395
    '''
    
    rand = lambda: random.randint(150, 255)
    return "#%02X%02X%02X" % (rand(), rand(), rand())



### For visualisation ---------------------------------------------------------

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