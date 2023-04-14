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

ax = sns.lineplot(x='throughput', y='goodput', data=stats, hue='nodes', palette = color)
ax.set(xlabel = 'throughput (req/s)', ylabel = 'goodput (req/s)')
fig = ax.get_figure()
fig.savefig(graph_dir + 'throughputgoodput.png')
plt.close(fig)

ax = sns.lineplot(x='throughput', y='lost', data=stats, hue='nodes', palette = color)
ax.set(xlabel = 'throughput (req/s)', ylabel = 'requests lost')
fig = ax.get_figure()
fig.savefig(graph_dir + 'throughputlost.png')
plt.close(fig)

ax = sns.lineplot(x='throughputfiltered', y='mean', data=stats, hue='msg_size', palette = color) #err_style="band", estimator=np.median, ci='sd'
ax.set(xlabel = 'throughput (req/s)', ylabel = 'mean latency (ms)')
ax.legend(title = 'message size')
fig = ax.get_figure()
fig.savefig(graph_dir + 'throughputlatency.png')
plt.close(fig)

for file in os.listdir(f'./experiments/data/{test_name}/subtests'):
	filename = os.fsdecode(file)
	times = pd.read_csv(f'./experiments/data/{test_name}/subtests/{filename}', skipinitialspace=True)
	filename = os.path.splitext(filename)[0]
	times['sent(s)'] = times['sent'] / 1_000_000_000
	times['latency'] = (times['rec'] - times['sent']) / 1_000_000

	# cumulative latency
	ax = sns.ecdfplot(x='latency', data=times, palette = color)
	ax.set(xlabel = 'latency (ms)', ylabel = 'fraction of requests', xscale = 'log')
	fig = ax.get_figure()
	fig.savefig(f'{graph_dir + filename}_cumlatency.png')
	plt.close(fig)

	# time / latency heatmap
	fig, ax = plt.subplots(1, 1, figsize=(10, 5), constrained_layout=True)
	# ax = sns.histplot(times, x="sent(s)", y="latency", binwidth=(0.05, 10), ax=ax)
	ax = sns.histplot(times, x="sent(s)", y="latency", bins = 50, ax=ax, palette = color)
	ax.set(xlabel="time (s)", xlim=(0,10), ylabel = "latency (ms)")
	fig.savefig(f'{graph_dir + filename}_timelatencyheatmap.png')
	plt.close(fig)