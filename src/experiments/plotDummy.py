import sys
import os
import numpy as np
import pandas as pd
import matplotlib as mpl
import matplotlib.pyplot as plt
import seaborn as sns
import seaborn.objects as so

color = sns.color_palette("colorblind")

# stats = pd.read_csv('./graphs/data/dummystats.csv', skipinitialspace=True)
test_name = sys.argv[1]

# create directory to output graphs to
graph_dir = f'./experiments/graphs/{test_name}/'
if not os.path.exists(graph_dir):
	os.mkdir(graph_dir)

# open stats file and add column
stats = pd.read_csv(f'./experiments/data/{test_name}/stats.csv', skipinitialspace=True)
stats['lost'] = 1. - (stats['rec'] / stats['sent'])
stats['diff'] = np.abs(stats['throughput'] - stats['goodput']) / stats['throughput']
stats['throughputfiltered'] = np.where(stats['diff'] <= 0.05, stats['throughput'], None)

ax = sns.lineplot(x='throughput', y='goodput', data=stats, hue='msg_size', palette = color)
ax.set(xlabel = 'throughput (req/s)', ylabel = 'goodput (req/s)')
ax.legend(title = 'message size')
fig = ax.get_figure()
fig.savefig(graph_dir + 'throughputgoodput.png')
plt.close(fig)

ax = sns.lineplot(x='throughput', y='lost', data=stats, hue='msg_size', palette = color)
ax.set(xlabel = 'throughput (req/s)', ylabel = 'requests lost')
ax.legend(title = 'message size')
fig = ax.get_figure()
fig.savefig(graph_dir + 'throughputlost.png')
plt.close(fig)

ax = sns.lineplot(x='throughputfiltered', y='mean', data=stats, hue='msg_size', palette = color) #err_style="band", estimator=np.median, ci='sd'
ax.set(xlabel = 'throughput (req/s)', ylabel = 'mean latency (ms)')
ax.legend(title = 'message size')
fig = ax.get_figure()
fig.savefig(graph_dir + 'throughputlatency.png')
plt.close(fig)