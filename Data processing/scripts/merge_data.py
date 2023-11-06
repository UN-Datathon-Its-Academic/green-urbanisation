# %%
import pandas as pd
import geopandas as gpd
from shapely.geometry import shape
import xarray as xr
import numpy as np
import matplotlib.pyplot as plt
import json

# %%
def load_buildings_data(city):
    # Load the CSV file
    data = pd.read_csv(f'../data/open-buildings/{city}.csv', index_col='system:index')

    # Convert the 'geo' column to a GeoSeries
    geos = [shape(json.loads(data['.geo'][i])) for i in range(len(data))]
    geoseries = gpd.GeoSeries(geos)

    # Convert the GeoSeries to a GeoDataFrame
    geodataframe = gpd.GeoDataFrame(data, geometry=geoseries)

    # Drop the original 'geo' column if needed
    geodataframe.drop('.geo', axis=1, inplace=True)

    return geodataframe

# %%
def merge_popdensity_data(city, geodataframe):
    # replace hyphen in city string with space
    city = city.replace('-', ' ')
    newdelhipop = gpd.read_file(f'../data/pop-density/Pop_density_{city}')

    # intersection = geodataframe.sjoin(newdelhipop, how="left", op="intersects")
    intersection = geodataframe.sjoin_nearest(newdelhipop, how="left")
    geodataframe['population_density'] = intersection.groupby('system:index')['Pp_dnst'].mean()
    geodataframe.dropna(inplace=True)

    return geodataframe
# %%
def read_temperature_data():
    # read temperature netcdf
    temp_ds = xr.open_dataset('../data/ERA5-land_skt_202210-202309_timmean_anomaly_regridtotwentiethdegree.nc')
    xarr = temp_ds['skt']
    df = xarr.to_dataframe().reset_index()

    temp_gdf = gpd.GeoDataFrame(
        df.skt, geometry=gpd.points_from_xy(df.longitude,df.latitude))

    return temp_gdf

def merge_temperature_data(geodataframe, temp_gdf):
    intersection = geodataframe.sjoin_nearest(temp_gdf, how="left")
    geodataframe['temperature_anomaly'] = intersection.groupby('system:index')['skt'].mean()
    geodataframe.dropna(inplace=True)

    return geodataframe

# %%
def read_greenness_data():
    # read NDVI netcdf
    greenness_ds = xr.open_dataset('../data/VIIRS-Land_v001-preliminary_NPP13C1_S-NPP_daily_2023_yearmean_NDVI_regridtotwentiethdegree_nc4.nc')
    xarr = greenness_ds['NDVI']
    df = xarr.to_dataframe().reset_index()

    greenness_gdf = gpd.GeoDataFrame(
        df.NDVI, geometry=gpd.points_from_xy(df.longitude,df.latitude))

    # remove spurious values from remapping
    greenness_gdf['NDVI'][greenness_gdf['NDVI'] < 0] = np.nan
    greenness_gdf['NDVI'][greenness_gdf['NDVI'] > 1] = np.nan

    return greenness_gdf

def merge_greenness_data(geodataframe, greenness_gdf):
    intersection = geodataframe.sjoin_nearest(greenness_gdf, how="left")
    geodataframe['greenness'] = intersection.groupby('system:index')['NDVI'].mean()
    geodataframe.dropna(inplace=True)

    return geodataframe
# %%
cities = ['Buenos-Aires','Cairo','Jakarta','Kinshasa','Kuala-Lumpur','Lagos','Mexico-City','New-Delhi','Sao-Paulo','Singapore']
temp_gdf = read_temperature_data()
greenness_gdf = read_greenness_data()
for city in cities:

    print(f'Processing data for {city}')

    geodataframe = load_buildings_data(city)
    geodataframe = merge_popdensity_data(city, geodataframe)
    geodataframe = merge_temperature_data(geodataframe, temp_gdf)
    geodataframe = merge_greenness_data(geodataframe, greenness_gdf)

    # save the merged data to csv
    geodataframe.to_csv(f'../data/merged-final/{city}_merged.csv')
