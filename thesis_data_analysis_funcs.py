# -*- coding: utf-8 -*-
"""
Created on Sun May 12 22:54:34 2019

@author: Sampo Vesanen

Functions for thesis data crunch. Find essential functions on top.
"""

# Import functions
import geopandas as gpd
import math
import numpy as np
import pandas as pd 
import matplotlib.pyplot as plt
from descartes import PolygonPatch
from shapely.geometry import Point, MultiPoint, Polygon
from shapely.ops import cascaded_union
from matplotlib.offsetbox import (TextArea, DrawingArea, OffsetImage,
                                  AnnotationBbox)
import random



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
    This anonymous function is to be used with groupby objects. It will
    determine from Pandas Series whether the values in that series are
    identical or unidentical. Return string answer or list.
    '''
    return "identical" if len(set(x)) == 1 else x.tolist()



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



### Travel time comparison function and assisting functions -------------------

def travelTimeComparison(grid, forest, postal, records, listOfTuples, ttm_path, 
                         printStats=False, plotIds=False):
    '''
    Compare Travel-Time Matrix 2018 (from here on "TTM") private car data with 
    my thesis survey data. This function produces a DataFrame with one row for 
    each tuple inputted into it. This function calculates many descriptives 
    about these two datasets.
    
    Use printStats=True to see descriptives. Please be aware that a report
    for one origin-destination tuple is about 50 rows long.
    
    Use plotIds=True to map origin-destination tuples on a single map. Not
    recommended for len(listOfTuples) > 10 as the map will become visually
    uninformative and its calculation becomes resource intensive.
    
    In this thesis we assume:
    - rush hour (ttm_r_t) = "Weekday, rush hour (07:00-09:00 and 15:00-17:00)", 
    - midday (ttm_m_t) = "Weekday, other than rush hour"
    - entire travel time with speed limits, no other impedances (ttm_sl_t) = 
        all parktime data as averaged
    - as in the literature, searching for parking in TTM is 0.42 minutes.
    - as stated in literature, walking to destination in TTM is 2.5 minutes in 
        Helsinki center and 2.0 minutes elsewhere
        
    Additional note: If "thesis_r_" or "thesis_m_" is nan, it means that this
    destination does not have data for that timeofday option. This is more
    likely to happen in postal code areas with low amounts of responses.
    
    If "ttm_r_t", "ttm_m_t" and "ttm_sl_t" are nan, it means that there is no 
    way to reach the destination from the origin in TTM.
    
    Resulting columns in the result DataFrame are as follows:
        "from_id"               YKR ID of origin
        "from_name"             Postal area name of origin
        "to_id"                 YKR ID of destination
        "to_name"               Postal area name of destination
        
        "ttm_r_t"               TTM: entire travel time in rush hour traffic
                                in a private car (minutes)
        "ttm_m_t"               TTM: entire travel time in midday traffic
                                in a private car (minutes)
        "ttm_sl_t"              TTM: entire travel time following speed limits
                                without any additional impedances in a private
                                car (minutes)
        "ttm_sfp"               TTM: Time used to search for parking (0.42 min)
        "ttm_wtd"               TTM: Time used to walk to the destination from 
                                the parked car (2.5 or 2.0 minutes)
        "ttm_r_drivetime"       TTM: entire travel time in rush hour traffic 
                                minus searching for parking and walking to the
                                destination (minutes)
        "ttm_m_drivetime"       TTM: entire travel time in midday traffic minus
                                SFP and WTD (minutes)
        "ttm_sl_drivetime"      TTM: entire travel time following speed limits
                                without any additional impedances minus
                                SFP and WTD (minutes)
        "ttm_r_pct"             TTM: how much is SFP and WTD of ttm_r_t
                                (percent)
        "ttm_m_pct"             TTM: how much is SFP and WTD of ttm_m_t  
                                (percent)
        "ttm_sl_pct"            TTM: how much is SFP and WTD of ttm_sl_t
                                (percent)
                                
        "values_in_dest"        Thesis: amount of records in destination postal
                                code area
        "thesis_r_sfp"          Thesis: Averaged searching for parking in 
                                destination postal code area in rush hour 
                                traffic (minutes)
        "thesis_m_sfp"          Thesis: Averaged SFP in destination postal code 
                                area in midday traffic (minutes)
        "thesis_sl_sfp"         Thesis: Averaged SFP in destination postal code 
                                area generally (all parktime values averaged)
                                (minutes)
        "thesis_r_wtd"          Thesis: Averaged walking to the destination 
                                from the parked car in destination postal code 
                                area in rush hour traffic hours (minutes)
        "thesis_m_wtd"          Thesis: averaged WTD in destination postal code 
                                area in midday hours (minutes)
        "thesis_sl_wtd"         Thesis: Averaged WTD in destination postal code 
                                area generally (all walktime values are 
                                averaged) (minutes)
        "thesis_r_drivetime"    Thesis: Entire TTM travel time in rush hour 
                                traffic minus Thesis data rush hour SFP and
                                WTD (minutes)
        "thesis_m_drivetime"    Thesis: Entire TTM travel time in midday traffic 
                                minus thesis data midday SFP and WTD (minutes)
        "thesis_sl_drivetime"   Thesis: Entire TTM travel time following speed 
                                limits without any additional impedances minus 
                                thesis data general (all values averaged) SFP 
                                and WTD (minutes)
        "thesis_r_pct"          Thesis: how much is thesis SFP and thesis WTD 
                                of the entire TTM travel time in rush hour 
                                traffic (percent)
        "thesis_m_pct"          Thesis: how much is thesis SFP and thesis WTD 
                                of the entire TTM travel time in midday traffic 
                                (percent)
        "thesis_sl_pct"         Thesis: how much is thesis SFP and thesis WTD 
                                of the entire TTM travel time following speed 
                                limits without any additional impedances 
                                (percent)
    '''
    
    result = pd.DataFrame(
            columns=["from_id", "from_name", "to_id", "to_name", "ttm_r_t", 
                     "ttm_m_t", "ttm_sl_t", "ttm_sfp", "ttm_wtd", 
                     "ttm_r_drivetime", "ttm_m_drivetime", "ttm_sl_drivetime", 
                     "ttm_r_pct", "ttm_m_pct", "ttm_sl_pct", "values_in_dest", 
                     "thesis_r_sfp", "thesis_m_sfp", "thesis_sl_sfp",
                     "thesis_r_wtd", "thesis_m_wtd", "thesis_sl_wtd",
                     "thesis_r_drivetime", "thesis_m_drivetime", 
                     "thesis_sl_drivetime", "thesis_r_pct", "thesis_m_pct", 
                     "thesis_sl_pct"])
    template = result.copy()
    template.loc[0] = 0
    
    
    # Determine longer walking times in the Helsinki center
    
    # In the Travel time matrix 2018 all parking takes 0.42 minutes. The 
    # research team used the bounding box below to define an area in center of 
    # Helsinki to denote an area where people walk a longer time to their cars. 
    # This can, conversely, used to measure walking distances from car to main
    # destination. Produce list "walk_center" to keep track of postal code 
    # areas inside the walking center Helsinki. 
    
    # Inside the walking center people walk 180 meters (2.5 min) to their 
    # destination. Outside the walking center people walk 130 meters (2 minutes) 
    # to their destination. Polygon source: Henrikki Tenkanen. Walking 
    # distances are from "Kurri & Laakso, 2002. Parking policy measures and 
    # their effects in the Helsinki metropolitan area".
    walkingHki = Polygon([(387678.024778, 6675360.99039), 
                          (387891.53396, 6670403.35286),
                          (383453.380944, 6670212.21613), 
                          (383239.871737, 6675169.85373),
                          (387678.024778, 6675360.99039)])
    walkingHki = gpd.GeoDataFrame(geometry=[walkingHki], crs=postal.crs)
    
    full_walk_center = postal[postal.intersects(walkingHki.unary_union)].reset_index()
    inters_walk_center = gpd.overlay(postal, 
                                     walkingHki,
                                     how="intersection").reset_index()
    walk_center = full_walk_center[inters_walk_center.area / 
                                   full_walk_center.area > 0.5]
    walk_center = list(walk_center.posti_alue)
    
    
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

        # Identify route number, for plotting
        route_no = 1

    # Iterate through all ids inputted by user in the parameter "listOfTuples"
    for originId, destinationId in list(listOfTuples):
        
        # This code is meant to compare 2018 Travel-Time Matrix results to my 
        # thesis results. Get origin and destination points from TTM data, then 
        # compare to zipcodes mean in my data
        
        # Copy DataFrame template to produce this row of data
        thisRow = template.copy()

        # Origins and destinations are read from a list of tuples. Read relevant 
        # text file location from "ttm_path"
        traveltimepath = ttm_path.format(destinationId[:4], destinationId)
        
        # Get destination text file. Remove all columns not about private cars
        destfile = pd.read_csv(traveltimepath, sep=";")
        destfile["from_id"] = pd.to_numeric(destfile.from_id)
        delCol = list(destfile.columns[2:13])
        destfile = destfile.drop(delCol, axis=1)
        
        # Slice destination and origin from GeoDataFrame "grid". 
        dest = grid.loc[grid.YKR_ID == int(destinationId)].reset_index()
        orig = grid.loc[grid.YKR_ID == int(originId)].reset_index()
        
        # Insert id data into "thisRow", the current row being processed
        thisRow.loc[0, "from_id"] = originId
        thisRow.loc[0, "to_id"] = destinationId
        thisRow.loc[0, "from_name"] = postal.loc[
                postal.intersects(orig.geometry[0]), "nimi"].values[0]
        thisRow.loc[0, "to_name"] = postal.loc[
                postal.intersects(dest.geometry[0]), "nimi"].values[0]
        
        # Convert dest and orig geometry to Point for later use in plotting.
        # If this is done earlier, the naming of the zipcode above can fail.
        dest["geometry"] = dest.centroid
        orig["geometry"] = orig.centroid

        # Get TTM2018 data for the origin
        # Match origin and destination
        ttm = destfile.loc[destfile.from_id == int(originId)].reset_index()
        ttm = ttm.loc[ttm.index[0]]

        # Get all thesis survey data about destination postal code area
        thisZip = records.loc[records.zipcode == dest.zipcode[0]]
        
        # parktime and walktime in thesis survey data, destination postal
        # code area. Get timeofday == 1 (rush hour), 2 (midday) and all.
        parktime1 = thisZip.loc[thisZip.timeofday == 1]["parktime"]
        parktime2 = thisZip.loc[thisZip.timeofday == 2]["parktime"]
        parktime_all = thisZip.parktime
        
        walktime1 = thisZip.loc[thisZip.timeofday == 1]["walktime"]
        walktime2 = thisZip.loc[thisZip.timeofday == 2]["walktime"]
        walktime_all = thisZip.walktime
        
        
        #### Populate current row ####
        
        # Travel-time Matrix 2018, entire travel times with private car. Detect 
        # nodata values (-1) and assign np.nan if detected.
        thisRow.loc[0, "ttm_r_t"] = np.nan if ttm[3] == -1 else ttm[3]
        thisRow.loc[0, "ttm_m_t"] = np.nan if ttm[5] == -1 else ttm[5]
        thisRow.loc[0, "ttm_sl_t"] =  np.nan if ttm[7] == -1 else ttm[7]
        ttm_r_t = thisRow.loc[0, "ttm_r_t"]
        ttm_m_t = thisRow.loc[0, "ttm_m_t"] 
        ttm_sl_t = thisRow.loc[0, "ttm_sl_t"]
           
        # "ttm_xx_drivetime" is the entire TTM travel time minus searching for 
        # parking (TTM value) and walking from the parked car to destination 
        # (TTM value), in the given time of day
        ttm_sfp = 0.42
        ttm_wtd = 2.5 if thisZip.zipcode.isin(walk_center).any() else 2.0
        ttm_park_process = ttm_sfp + ttm_wtd

        thisRow.loc[0, "ttm_sfp"] = ttm_sfp
        thisRow.loc[0, "ttm_wtd"] = ttm_wtd
        thisRow.loc[0, "ttm_r_drivetime"] = ttm_r_t - ttm_park_process
        thisRow.loc[0, "ttm_m_drivetime"] = ttm_m_t - ttm_park_process
        thisRow.loc[0, "ttm_sl_drivetime"] = ttm_sl_t - ttm_park_process
        ttm_r_drivetime = thisRow.loc[0, "ttm_r_drivetime"]
        ttm_m_drivetime = thisRow.loc[0, "ttm_m_drivetime"]
        ttm_sl_drivetime = thisRow.loc[0, "ttm_sl_drivetime"]
        
        # In Travel-Time Matrix data, how much searching for parking 
        # and walking to destination from parked car constituted of the total 
        # travel time?
        thisRow.loc[0, "ttm_r_pct"] = round(ttm_park_process / ttm_r_t, 3)
        thisRow.loc[0, "ttm_m_pct"] = round(ttm_park_process / ttm_m_t, 3)
        thisRow.loc[0, "ttm_sl_pct"] = round(ttm_park_process / ttm_sl_t, 3)
        ttm_r_pct = thisRow.loc[0, "ttm_r_pct"]
        ttm_m_pct = thisRow.loc[0, "ttm_m_pct"]
        ttm_sl_pct = thisRow.loc[0, "ttm_sl_pct"]

        # Amount of thesis survey responses in destination postal code   
        thisRow.loc[0, "values_in_dest"] = len(thisZip)
        values_in_dest = thisRow.loc[0, "values_in_dest"]
        
        # Averaged searching for parking in destination postal code using
        # thesis survey data
        thisRow.loc[0, "thesis_r_sfp"] = round(parktime1.mean(), 2)
        thisRow.loc[0, "thesis_m_sfp"] = round(parktime2.mean(), 2)
        thisRow.loc[0, "thesis_sl_sfp"] = round(parktime_all.mean(), 2)
        thesis_r_sfp =  thisRow.loc[0, "thesis_r_sfp"]
        thesis_m_sfp = thisRow.loc[0, "thesis_m_sfp"]
        thesis_sl_sfp = thisRow.loc[0, "thesis_sl_sfp"]

        # Averaged walk to destination from parked car in destination postal 
        # code using thesis survey data
        thisRow.loc[0, "thesis_r_wtd"] = round(walktime1.mean(), 2)
        thisRow.loc[0, "thesis_m_wtd"] = round(walktime2.mean(), 2)
        thisRow.loc[0, "thesis_sl_wtd"] = round(walktime_all.mean(), 2)
        thesis_r_wtd =  thisRow.loc[0, "thesis_r_wtd"]
        thesis_m_wtd = thisRow.loc[0, "thesis_m_wtd"]
        thesis_sl_wtd = thisRow.loc[0, "thesis_sl_wtd"]
        
        # Travel-Time Matrix 2018 entire travel time minus thesis survey
        # searching for parking and walking from the parked car to the 
        # destination, using values for rush hour, midday and general,
        # respectively
        thisRow.loc[0, "thesis_r_drivetime"] = ttm_r_t - thesis_r_sfp - thesis_r_wtd
        thisRow.loc[0, "thesis_m_drivetime"] = ttm_m_t - thesis_m_sfp - thesis_m_wtd
        thisRow.loc[0, "thesis_sl_drivetime"] = ttm_sl_t - thesis_sl_sfp - thesis_sl_wtd
        thesis_r_drivetime = thisRow.loc[0, "thesis_r_drivetime"]
        thesis_m_drivetime = thisRow.loc[0, "thesis_m_drivetime"]
        thesis_sl_drivetime = thisRow.loc[0, "thesis_sl_drivetime"]
        
        # According to thesis SFP and WTD data, How much searching for parking 
        # and walking from the parked car to the destination constituted of 
        # the total travel time (using TTM data)?
        thisRow.loc[0, "thesis_r_pct"] = round(
                (thesis_r_sfp + thesis_r_wtd) / ttm_r_t, 3)
        thisRow.loc[0, "thesis_m_pct"] = round(
                (thesis_m_sfp + thesis_m_wtd) / ttm_m_t, 3)
        thisRow.loc[0, "thesis_sl_pct"] = round(
                (thesis_sl_sfp + thesis_sl_wtd) / ttm_sl_t, 3)
        thesis_r_pct = thisRow.loc[0, "thesis_r_pct"]
        thesis_m_pct = thisRow.loc[0, "thesis_m_pct"]
        thesis_sl_pct = thisRow.loc[0, "thesis_sl_pct"]
        
        # append all gathered results to the DataFrame "result"
        result = result.append(thisRow, sort=False)
        
        
        # Print description of statistics to the console. Print an additional
        # note if the route is non-navigable in TTM18.
        if printStats == True:
            print("\n=========================================")
            print(f"=== STATISTICS for {thisRow.loc[0, 'from_id']} "
                  f"to {thisRow.loc[0, 'to_id']} ===")
            print("=========================================")
            
            print(f"Origin is {orig.zipcode[0]} {thisRow.loc[0, 'from_name']}."
                  f"\nDestination is {dest.zipcode[0]} {thisRow.loc[0, 'to_name']}.")
            
            if(math.isnan(ttm_r_t)):
                print("\n*** NB! This route is non-navigable according to "
                      "TTM18 ***")
            
            print("\n==== Travel time matrix 2018 ====")
            
            print(f"\nSearching for parking in destination zipcode: {ttm_sfp} min")
            print("Walking to the destination from one's parked car in "
                  f"destination zipcode: {ttm_wtd} min")
            print(f"Total length of the parking process: {ttm_park_process} min")
            
            print("\n--- Rush hour traffic ---")
            print(f"Entire travel time: {ttm_r_t} min")
            print("Entire travel time without the parking process: "
                  f"{ttm_r_drivetime} min")
            print("-- The parking process represents "
                  f"{niceround(ttm_r_pct * 100)} % of the total")
            
            print("\n--- Midday traffic ---")
            print(f"Entire travel time: {ttm_m_t} min")
            print("Entire travel time without the parking process: "
                  f"{ttm_m_drivetime} min")
            print("-- The parking process represents "
                  f"{niceround(ttm_m_pct * 100)} % of the total travel time")

            print("\n--- Following speed limits without any additional"
                  " impedances ---")
            print(f"Entire travel time: {ttm_sl_t} min")
            print("Entire travel time without the parking process: "
                  f"{ttm_sl_drivetime} min")
            print("-- The parking process represents "
                  f"{niceround(ttm_sl_pct * 100)} % of the total travel time")
            
            print("\n\n==== Sampo Vesanen thesis ====")
            print("Amount of responses in destination "
                  f"({thisRow.loc[0, 'to_name']}): {values_in_dest}")
            
            print("\n--- Rush hour traffic ---")
            print("Searching for parking in destination zipcode (mean): "
                  f"{thesis_r_sfp} min")
            print("Walking to the destination from one's parked car in "
                  f"destination zipcode (mean): {thesis_r_wtd} min")
            print("Total length of the parking process: "
                  f"{niceround(thesis_r_sfp + thesis_r_wtd)} min")
            
            print(f"\nEntire travel time (TTM data): {ttm_r_t} min")
            print("Entire travel time without the parking process: "
                  f"{niceround(thesis_r_drivetime)} min")
            print("-- The parking process represents "
                  f"{niceround(thesis_r_pct * 100)} % of total travel time")
            
            print("\n--- Midday traffic ---")
            print(f"SFP (mean): {thesis_m_sfp} min")
            print(f"WTD (mean): {thesis_m_wtd} min")
            print("Total length of the parking process: "
                  f"{niceround(thesis_m_sfp + thesis_m_wtd)} min")
            
            print(f"\nEntire travel time (TTM data): {ttm_m_t} min")
            print("Entire travel time without the parking process: "
                  f"{niceround(thesis_m_drivetime)} min")
            print("-- The parking process represents "
                  f"{niceround(thesis_m_pct * 100)} % of total travel time")
            
            print("\n--- Generally ---")
            
            print(f"SFP (mean): {thesis_sl_sfp} min")
            print(f"WTD (mean): {thesis_sl_wtd} min")
            print("Total length of the parking process: "
                  f"{niceround(thesis_sl_sfp + thesis_sl_wtd)} min")
            
            print(f"\nEntire travel time (TTM data): {ttm_sl_t} min")
            print("Entire travel without the parking process: "
                  f"{niceround(thesis_sl_drivetime)} min")
            print("-- The parking process represents "
                  f"{niceround(thesis_sl_pct * 100)} % of total travel "
                  "time\n\n")
        
        # Plot origin and destination
        if plotIds == True:
 
            # For loop for plotting origin and destination on the map. Prepare
            # annotation with these variables
            route = f"Route: {str(route_no)}\n"
            identifierlist = ["Origin: ", "Destination: "]
            namelist = [thisRow.from_name[0], thisRow.to_name[0]]
            ykrlist = [f"\nYKR-ID: {str(orig.YKR_ID.item())}", 
                       f"\nYKR-ID: {str(dest.YKR_ID.item())}"]
            
            # Random light color for annotation bbox background to identify 
            # DataFrame rows from each other
            rnd_col = random_color()
            
            for item, identifier, name, ykr in zip([orig, dest], identifierlist, 
                                                   namelist, ykrlist):
                # Annotation coordinates to tuple
                item["coords"] = polygonCoordsToTuple(item)
                item.plot(ax=base)
                
                # Annotate with a text box
                # Show acknowledgement in annotationbbox if destination is not 
                # reachable from origin (TTM18 nodata value -1 changed to np.nan)
                if np.isnan(ttm_r_t):
                    ykr = f"{ykr}\nNodata, route not navigable"
                anno = f"{route} {identifier} {name} {ykr}"
                
                offsetbox = TextArea(anno, minimumdescent=False)
                ab = AnnotationBbox(offsetbox, item.coords[0],
                                    xybox=(-20, 40),
                                    xycoords="data",
                                    boxcoords="offset points",
                                    bboxprops=dict(facecolor=rnd_col,
                                                   boxstyle="round"),
                                    arrowprops=dict(arrowstyle="->"))
                base.add_artist(ab)
            
            route_no += 1
            plt.tight_layout()

    result = result.reset_index()
    result = result.drop(columns=["index"])
    
    return result


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
                scheme="fisher_jenks",
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
                scheme="fisher_jenks",
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
        annotation = f"{row.nimi}, {str(row[rowname])}"
        plt.annotate(s=annotation, xy=row.coords,
                     horizontalalignment="center")