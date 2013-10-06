#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Read our hackathon csv file, output as lat/lng with stats shapefile for QGIS"""
# Written for Future Cities Hackathon 2013
# https://github.com/idontgetoutmuch/ParkingWestminster
# Ian Ozsvald ianozsvald.com
# in collaboration with:
# Amit Nandi, Bart Baddeley, Dominic Steinitz, Jackie Steinitz, Mateusz Łapsa-Malawski
from __future__ import division  # 1/2 == 0.5, as in Py3
from __future__ import absolute_import  # avoid hiding global modules with locals
from __future__ import unicode_literals  # force unadorned strings "" to be unicode without prepending u""
import os
import csv
import pandas as pd
import numpy as np
import shapefile

#PARKING_CASHLESS = "ParkingCashlessDenorm100.csv"
#PARKING_CASHLESS = "ParkingCashlessDenorm100000.csv"
PARKING_CASHLESS = "ParkingCashlessDenorm.csv"  # 1.4GB of parking data for Westminster
OUTPUT_DIRECTORY = "shapefiles"
OUTPUT_SHAPEFILE = "coords_augmented"

COLUMN_NAMES = ["amount paid", "paid duration mins", "start date", "start day", "end date", "end day", "start time", "end time", "DesignationType", "Hours of Control", "Tariff", "Max Stay", "Spaces", "Street", "x coordinate", "y coordinate", "latitude", "longitude"]

if __name__ == "__main__":
    if 'rows' not in dir():  # the main data is 1.4GB so we'll keep it in-memory in an IPython session
        rows = pd.io.parsers.read_csv(PARKING_CASHLESS, names=COLUMN_NAMES)
    # drop any rows with invalid latitude and longitude, accept that maybe
    # these could be repaired later (sometimes we have the lat/lng elsewhere in
    # the file for the matching street name)
    rows = rows[pd.notnull(rows.latitude)]
    rows = rows[pd.notnull(rows.longitude)]

    # now fix the "start date" and "start time" into a new "start datetime" field
    rows['start date'] = pd.to_datetime(rows['start date'])
    # make a datetime for today at midnight
    ts_now = pd.to_datetime("2013-10-05")
    rows['start time'] = pd.to_datetime(rows['start time'])  # defaults to today (5th Oct) with correct hour:min
    rows['start time'] = rows['start time'] - ts_now  # get relative difference, leaves a timedelta
    rows['start datetime'] = rows['start time'] + rows['start date']  # combine date with midnight & timedelta to get new datetime

    dfgb = rows.groupby(['latitude', 'longitude'])
    sgb = dfgb['Spaces']
    mapping_of_lat_lng_to_spaces = sgb.mean()
    dict_of_lat_lng_to_spaces = dict(mapping_of_lat_lng_to_spaces)

    dict_of_occupancies = {}
    with open("occupancy_by_lat_long.csv") as f:
        reader = csv.reader(f)
        for line in reader:
            #lat, lng, mean_occ, max_occ, occ_prop, occ_prop_max1,  = line
            #tariff, max_stay, total_takings = 0, 0, 0
            lat, lng, mean_occ, max_occ, occ_prop, occ_prop_max1, tariff, max_stay, total_takings = line
            if not np.isnan(float(max_occ)) and not np.isnan(float(tariff)):
                dict_of_occupancies[(float(lat), float(lng))] = {'mean_occ': float(mean_occ),
                                                                 'max_occ': float(max_occ),
                                                                 'occ_prop': float(occ_prop),
                                                                 'occ_prop_max1': float(occ_prop_max1),
                                                                 'tariff': float(tariff),
                                                                 'max_stay': max_stay,
                                                                 'total_takings': float(total_takings)}
            else:
                print("Ignoring a nan!", line)
    # setup a shapefile write, make sure it automatically writes enough records
    # for each shape that's added
    # POLYLINE for lines, POINT for singular points
    w = shapefile.Writer(shapefile.POINT)
    w.autoBalance = 1  # auto balance records and geometry
    # add fields (columns) of data to be associated with a list of points
    # note use of byte not unicode type for the strings for shapefile records
    w.field(b'count', b'N', 5)
    w.field(b'mean_prop', b'F', 13)
    w.field(b'max_prop', b'F', 13)
    w.field(b'occ_prop', b'F', 13)
    w.field(b'tariff', b'F', 13)
    w.field(b'max_stay', b'F', 13)
    w.field(b'total_takings', b'F', 13)
    for lat, lng in dict_of_lat_lng_to_spaces.keys():
        #print lat, lng
        count = dict_of_lat_lng_to_spaces[(lat, lng)]
        coordinates = [[lng, lat]]
        w.poly([coordinates], shapeType=shapefile.POINT)
        key = (lat, lng)
        if key in dict_of_occupancies:
            occ_prop_max1 = dict_of_occupancies[key]['occ_prop_max1']
            mean_occ = dict_of_occupancies[key]['mean_occ']
            max_occ = dict_of_occupancies[key]['max_occ']
            tariff = dict_of_occupancies[key]['tariff']
            max_stay = dict_of_occupancies[key]['max_stay']
            total_takings = dict_of_occupancies[key]['total_takings']
            w.record(count, mean_occ, max_occ, occ_prop_max1, tariff, max_stay, total_takings)
    if not os.path.exists(OUTPUT_DIRECTORY):
        os.makedirs(OUTPUT_DIRECTORY)
    output_path = os.path.join(OUTPUT_DIRECTORY, OUTPUT_SHAPEFILE)
    w.save(output_path)
