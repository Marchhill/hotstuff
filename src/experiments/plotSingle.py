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
times['sent(s)'] = times['sent'] / 1_000_000_000
times['latency'] = (times['rec'] - times['sent']) / 1_000_000_000

# cumulative latency
ax = sns.ecdfplot(x='latency', data=times, palette = color)
ax.set(xlabel = 'latency (s)', ylabel = 'fraction of requests')
fig = ax.get_figure()
fig.savefig(f'{subtests_dir + filename}_cumlatency.pgf')
fig.savefig(f'{subtests_dir + filename}_cumlatency.png')
plt.close(fig)
plt.clf()

# time / latency heatmap
fig, ax = plt.subplots(1, 1, figsize=(10, 5), constrained_layout=True)
# ax = sns.histplot(times, x="sent(s)", y="latency", binwidth=(0.05, 10), ax=ax)
ax = sns.histplot(times, x="sent(s)", y="latency", bins = 50, ax=ax, palette = color)
ax.set(xlabel="time (s)", xlim=(0,20), ylim=(0., None), ylabel = "latency (s)")
fig.savefig(f'{subtests_dir + filename}_timelatencyheatmap.pgf')
fig.savefig(f'{subtests_dir + filename}_timelatencyheatmap.png')
plt.close(fig)
plt.clf()