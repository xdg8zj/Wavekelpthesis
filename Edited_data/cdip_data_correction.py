import pandas as pd
import numpy as np 
import math


#import the cdip file
cdip_daily_df = pd.read_csv('/Users/aleynaloughran-pierce/Desktop/CHMBE_Summer_2024/Raw Files/cdip_mop_hs_tp_daily_20240509 copy.csv')

#make new subset for sites, dates, mean height, max heights, latitude, longitude
new_cdip_daily_df = cdip_daily_df.loc[:,['site', 'date_GMT','Mean_Hs_m', 'Max_Hs_m','latitude', 'longitude']]

#convert to datetime
new_cdip_daily_df["datetime"] = pd.to_datetime(new_cdip_daily_df["date_GMT"]) 
#extract months and years
new_cdip_daily_df["month"] = new_cdip_daily_df["datetime"].dt.month
new_cdip_daily_df["year"] = new_cdip_daily_df["datetime"].dt.year 
#website: https://pandas.pydata.org/docs/getting_started/intro_tutorials/09_timeseries.html

wave_peak_months = [1,2,3,10,11,12]
filt_cdip_daily_df = new_cdip_daily_df[new_cdip_daily_df['month'].isin(wave_peak_months)]
print(filt_cdip_daily_df.head(120)) #certifies that it works yay :D

#fix the rest in excel
filt_cdip_daily_df.to_csv('modified_cdip_wave_data.csv', index = False)



