import pandas as pd
from pandas import Series, DataFrame, Panel

import numpy as np

from datetime import datetime, date, time

import matplotlib.pyplot as plt
import matplotlib as mpl

t = pd.read_csv('ParkingCashlessDenorm.csv', header=None)

u = t.rename(columns= {0:"amount_paid",
                       1:"paid_duration_mins",
                       2:"start_date",
                       3:"start_day",
                       4:"end_date",
                       5:"end_day",
                       6:"start_time",
                       7:"end_time",
                       8:"DesignationType",
                       9:"Hours_of_Control",
                       10:"Tariff",
                       11:"Max_Stay",
                       12:"Spaces",
                       13:"Street",
                       14:"x_coordinate",
                       15:"y_coordinate",
                       16:"latitude",
                       17:"longitude"})

v = u[(u.Street == 'Devonshire Place')  &
      (pd.to_datetime(u.start_date) == pd.to_datetime('2013-02-28 00:00:00'))]

v['start_date'] = pd.to_datetime(v['start_date'])
v['end_date']   = pd.to_datetime(v['end_date'])

# make a datetime for the selected day at midnight
ts_now = pd.to_datetime("2013-02-28 00:00:00")

# defaults to today (5th Oct) with correct hour:min
v['start_time'] = pd.to_datetime(v['start_time'])
v['end_time']   = pd.to_datetime(v['end_time'])

# get relative difference, leaves a timedelta
v['start_time'] = v['start_time'] - ts_now
v['end_time']   = v['end_time'] - ts_now

# combine date with midnight & timedelta to get new datetime
v['start_datetime'] = v['start_time'] + v['start_date']
v['end_datetime']   = v['end_time'] + v['end_date']

start_date = pd.to_datetime(Series(v.start_datetime))
start_date.sort()

end_date = pd.to_datetime(Series(v.end_datetime))
end_date.sort()

ts1 = Series(np.ones(len(start_date)), start_date)
ts2 = Series(-1*np.ones(len(end_date)), end_date)

ts = ts1.append(ts2)
us = ts.sort_index()
cs = us.cumsum()

mpl.rc('figure', figsize = (10, 8))
cs.plot()
plt.show()






