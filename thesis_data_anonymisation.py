# -*- coding: utf-8 -*-
"""
Created on Thu Oct 10 19:31:52 2019

@author: Sampo Vesanen

For University of Helsinki master thesis:
"Parking of private cars and spatial accessibility in Helsinki Capital Region"

This script anonymises the thesis data used in the master thesis by Sampo
Vesanen.
"""

# Import functions
import pandas as pd
import os
import random
import string

# Anonymisation function
def anonymise(stringLength=10):
    
    """
    Generate a random string of fixed length. Enter integer to change the
    length of generated string. Default is ten characters.
    
    Code adapted from Pynative.
    https://pynative.com/python-generate-random-string/
    """
    
    lettersAndDigits = string.ascii_lowercase + string.digits
    return ''.join(random.choice(lettersAndDigits) for i in range(stringLength))


# data folder path
datawd = r"C:\Sampon\Maantiede\Master of the Universe\leaflet_survey_results"

# Import survey data. Lambda x to preserve leading zeros
records = pd.read_csv(os.path.join(datawd, "records180919.csv"), 
                      converters={'zipcode': lambda x: str(x)})
visitors = pd.read_csv(os.path.join(datawd, "visitors180919.csv"))

# Anonymise thesis data with the help of for loop to ensure identical
# anonymised identifiers both in records and in visitors. Use list
# comprehension to scan through ip data. IP address is only anonymised if it
# matches with the one currently selected by for loop.
for address in visitors.ip:
    thisAnonymisation = anonymise()
    records.ip = [thisAnonymisation if x==address else x for x in records.ip]
    visitors.ip = [thisAnonymisation if x==address else x for x in visitors.ip]
    
# Export newly anonymised data to csv.
records.to_csv(datawd + r"\records.csv")
visitors.to_csv(datawd + r"\visitors.csv")