import pandas as pd
import numpy as np 
import math

#import LTE File
LTE_removal_df = pd.read_csv('/Users/aleynaloughran-pierce/Desktop/CHMBE_Summer_2024/Raw Files/LTE_All_Species_Biomass_at_transect_20240501.csv')


#convert to datetime
LTE_removal_df["datetime"] = pd.to_datetime(LTE_removal_df["DATE"]) 
LTE_removal_df["month"] = LTE_removal_df["datetime"].dt.month
LTE_removal_df["year"] = LTE_removal_df["datetime"].dt.year 
#website: https://pandas.pydata.org/docs/getting_started/intro_tutorials/09_timeseries.html
# print(LTE_removal_df)
LTER_data_df = pd.read_csv("/Users/aleynaloughran-pierce/Desktop/CHMBE_Summer_2024/Raw Files/Annual_All_Species_Biomass_at_transect_20240501.csv")
print(LTER_data_df.MONTH.unique())
print(LTE_removal_df.month.unique())

LTE_removal_summer = [7,8,9,10]
LTE_removal_summer_df = LTE_removal_df[LTE_removal_df['month'].isin(LTE_removal_summer)]

print(LTE_removal_summer_df)
exit()
#fix the rest in excel
LTE_removal_summer_df.to_csv('LTE_removal_summer_nojune.csv', index = False)
