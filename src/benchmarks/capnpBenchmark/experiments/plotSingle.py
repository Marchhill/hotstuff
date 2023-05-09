import sys
import os
import numpy as np
import pandas as pd
import matplotlib as mpl
import matplotlib.pyplot as plt
import seaborn as sns
import seaborn.objects as so

color = sns.color_palette("colorblind")

test_name = sys.argv[1]
filename = sys.argv[2]

# create directory to output graphs to
graph_dir = f'./graphs/{test_name}/'
subtests_dir = f'./graphs/{test_name}/subtests/'
if not os.path.exists(graph_dir):
	os.mkdir(graph_dir)
if not os.path.exists(subtests_dir):
	os.mkdir(subtests_dir)

times = pd.read_csv(f'./data/{test_name}/subtests/{filename}.csv', skipinitialspace=True)
times['send_time'] = times['send_time'] * 1000.

# cumulative latency
ax = sns.ecdfplot(x='send_time', data=times)
ax.set(xlabel = 'send time (ms)', ylabel = '% of sends', xscale = 'log')
fig = ax.get_figure()
fig.savefig(f'{subtests_dir + filename}_cumlatency.pgf')
fig.savefig(f'{subtests_dir + filename}_cumlatency.png')
plt.close(fig)
plt.clf()