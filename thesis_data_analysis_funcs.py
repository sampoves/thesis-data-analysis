# -*- coding: utf-8 -*-
"""
Created on Sun May 12 22:54:34 2019

@author: Sampo

Functions for thesis data crunch
"""

# Import functions
import numpy as np
import pandas as pd 
import random
import string
import matplotlib.pyplot as plt


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


def anonymise(stringLength=10):
    """
    Generate a random string of fixed length 
    Thank you: https://pynative.com/python-generate-random-string/
    """
    lettersAndDigits = string.ascii_lowercase + string.digits
    return ''.join(random.choice(lettersAndDigits) for i in range(stringLength))


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
