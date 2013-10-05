import pandas as pd
from pandas import DataFrame, Series
import numpy as np
import os
import csv

names = ["amount paid", "paid duration mins", "start date", "start day", "end date", "end day", "start time", "end time", "DesignationType", "Hours of Control", "Tariff", "Max Stay", "Spaces", "Street", "x coordinate", "y coordinate", "latitude", "longitude"]

#df = pd.read_csv('Data\ParkingCashlessDenorm.csv', names =names)

# pick out St Johns Wood High Street
#SJWHS = df[df['Street'] == 'St John\'s Wood High Street']

def get_occupancy(df, x_coord):
	# pick an arbitrary Tues
	SWTues = df[(df['start date'] == '2013-04-30 00:00:00') & (df['x coordinate'] == x_coord)]

	SWTues['start date'] = pd.to_datetime(SWTues['start date'])
	SWTues['end date'] = pd.to_datetime(SWTues['end date'])

	# make a datetime for today at midnight
	ts_now=pd.to_datetime("2013-10-05")
	SWTues['start time'] = pd.to_datetime(SWTues['start time'])  # defaults to today (5th Oct) with correct hour:min
	SWTues['start time'] =SWTues['start time'] - ts_now  # get relative difference, leaves a timedelta
	SWTues['start datetime'] = SWTues['start time'] + SWTues['start date']  # combine date with midnight & timedelta to get new datetime

	SWTues['end time'] = pd.to_datetime(SWTues['end time'])  # defaults to today (5th Oct) with correct hour:min
	SWTues['end time'] =SWTues['end time'] - ts_now  # get relative difference, leaves a timedelta
	SWTues['end datetime'] = SWTues['end time'] + SWTues['end date']  # combine date with midnight & timedelta to get new datetime



	s1 = Series(np.ones(SWTues.shape[0]), index=SWTues['start datetime'])
	s2 = Series(-1*np.ones(SWTues.shape[0]), index=SWTues['end datetime'])

	mean_occ = s1.append(s2).sort_index().cumsum().mean()
	max_occ = SWTues['Spaces'].max()
	tariff = SWTues['Tariff'].max()
	max_stay = SWTues['Max Stay'].max()
	total_takings = SWTues['amount paid'].sum()

	occ_prop = mean_occ/max_occ
	return mean_occ, max_occ, occ_prop, tariff, max_stay, total_takings

def get_all_occupancies(df):
	with open("occupancy_by_lat_long.csv", "w") as f:
		writer = csv.writer(f)
		out = {}
		dfgb = df.groupby(['latitude','longitude','x coordinate'])
		sgb = dfgb['Spaces']
		mapping_of_lat_long_to_spaces = sgb.mean()
		dict_of_lat_long_to_spaces = dict(mapping_of_lat_long_to_spaces)
		print len(mapping_of_lat_long_to_spaces)
		for key_nbr, (lat, lng, place) in enumerate(mapping_of_lat_long_to_spaces.keys()):
			print key_nbr

			mean_occ, max_occ, occ_prop, tariff,  max_stay, total_takings = get_occupancy(df, place)
			#print mean_occ, max_occ, occ_prop, min(occ_prop,1.0)
			writer.writerow([lat, lng, mean_occ, max_occ, occ_prop, min(occ_prop,1.0), tariff,  max_stay, total_takings])
