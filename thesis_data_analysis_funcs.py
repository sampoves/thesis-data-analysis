# -*- coding: utf-8 -*-
"""
Created on Sun May 12 22:54:34 2019

@author: Sampo Vesanen

Functions for thesis data crunch
"""

# Import functions
import numpy as np
import pandas as pd 
import matplotlib.pyplot as plt



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



def detect_outlier(data_1):
    """
    Thanks: 
    https://medium.com/datadriveninvestor/finding-outliers-in-dataset-using-python-efc3fce6ce32
    """
    outliers = []
    threshold = 3
    mean_1 = np.mean(data_1)
    std_1 = np.std(data_1)
    
    for y in data_1:
        z_score = (y - mean_1)/std_1 
        if np.abs(z_score) > threshold:
            outliers.append(y)
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