#TODO: types of violations in specific areas


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt


p_tickets = pd.read_csv('ParkingTickets.csv', names=['date', 'time', 'code', 'offence', 'address'])


dows = pd.Series([ d.dayofweek for d in pd.to_datetime(p_tickets['date']) ])
p_tickets['dow'] = dows

hours = pd.Series([ t.hour for t in pd.to_datetime(p_tickets['time']) ])
p_tickets['hour'] = hours

ts = p_tickets.groupby(['hour', 'dow']).size()
#ts.plot()


#xs = [ x for (x,y) in grouped.keys() ]
#ys = [ y for (x,y) in grouped.keys() ]


values = pd.Series(ts.values)
normalised = values / float(values.max())
mapped = [normalised[i:i+7] for i in range(0, len(normalised), 7)]



def hinton(matrix, max_weight=None, ax=None, xlabel="", ylabel="", axis=None):
    """Draw Hinton diagram for visualizing a weight matrix."""
    ax = ax if ax is not None else plt.gca()

    if not max_weight:
        max_weight = 2**np.ceil(np.log(np.abs(matrix).max())/np.log(2))

    ax.patch.set_facecolor('white')
    ax.set_aspect('equal', 'box')
#    ax.xaxis.set_major_locator(plt.NullLocator())
#    ax.yaxis.set_major_locator(plt.NullLocator())

    for (x,y),w in np.ndenumerate(matrix):
        color = 'red' if w > 0 else 'blue'
        size = np.sqrt(np.abs(w))
        rect = plt.Rectangle([x - size / 2, y - size / 2], size, size,
                             facecolor=color, edgecolor=color)
        ax.add_patch(rect)

    ax.autoscale_view()
    if axis:
        ax.axis(axis)
    ax.set_ylabel(ylabel)
    ax.set_xlabel(xlabel)

hinton(mapped, xlabel="hour", ylabel="day of week", axis=[-1, 24, -1, 7])
plt.show()
#plt.savefig("fines.png",dpi=300)
