import sys
import os
import numpy as np
import pandas as pd
import matplotlib as mpl
import matplotlib.pyplot as plt
import seaborn as sns
import seaborn.objects as so

# stats = pd.read_csv('./graphs/data/dummystats.csv', skipinitialspace=True)
test_name = sys.argv[1]

# create directory to output graphs to
graph_dir = f'./experiments/graphs/{test_name}/'
os.mkdir(graph_dir)

# open stats file and add column
stats = pd.read_csv(f'./experiments/data/{test_name}/stats.csv', skipinitialspace=True)
stats['lost'] = 1. - (stats['rec'] / stats['sent'])

fig = sns.lineplot(x='throughput (req/s)', y='goodput (req/s)', data=stats, hue='nodes').get_figure()
fig.savefig(graph_dir + 'throughputgoodput.png')
plt.close(fig)

fig = sns.lineplot(x='throughput (req/s)', y='lost', data=stats, hue='nodes').get_figure()
fig.savefig(graph_dir + 'throughputlost.png')
plt.close(fig)

fig = sns.lineplot(x='goodput (req/s)', y='mean latency(ms)', data=stats, hue='nodes').get_figure()
fig.savefig(graph_dir + 'goodputlatency.png')
plt.close(fig)

for file in os.listdir(f'./experiments/data/{test_name}/subtests'):
	filename = os.fsdecode(file)
	times = pd.read_csv(f'./experiments/data/{test_name}/subtests/{filename}', skipinitialspace=True)
	times['sent(s)'] = times['sent'] / 1_000_000_000
	times['latency'] = (times['rec'] - times['sent']) / 1_000_000

	# cumulative latency
	fig = sns.ecdfplot(x='latency (ms)', data=times).get_figure()
	fig.savefig(f'{graph_dir + filename}_cumlatency.png')
	plt.close(fig)

	# time / latency heatmap
	fig, ax = plt.subplots(1, 1, figsize=(10, 5), constrained_layout=True)
	# ax = sns.histplot(times, x="sent(s)", y="latency", binwidth=(0.05, 10), ax=ax)
	ax = sns.histplot(times, x="sent(s)", y="latency", bins = 50, ax=ax)
	ax.set(xlabel="time (s)", xlim=(0,10), ylabel = "latency (ms)")
	fig.savefig(f'{graph_dir + filename}_timelatencyheatmap.png')
	plt.close(fig)