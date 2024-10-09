
import pandas as pd
import numpy as np 
import math

#import the cdip file
LTE_removal_df = pd.read_csv('/Users/aleynaloughran-pierce/Desktop/CHMBE_Summer_2024/Raw Files/LTE_All_Species_Biomass_at_transect_20240501.csv')
#extract months and years
LTE_removal_df["month"] = LTE_removal_df["datetime"].dt.month
LTE_removal_df["year"] = LTE_removal_df["datetime"].dt.year 
#website: https://pandas.pydata.org/docs/getting_started/intro_tutorials/09_timeseries.html
print(LTE_removal_df)
