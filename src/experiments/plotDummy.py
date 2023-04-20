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

file_stats = []
for file in os.listdir(f'./experiments/data/{test_name}/subtests'):
	filename = os.fsdecode(file)
	times = pd.read_csv(f'./experiments/data/{test_name}/subtests/{filename}', skipinitialspace=True)
	filename = os.path.splitext(filename)[0]
	times['sent(s)'] = times['sent'] / 1_000_000_000
	times['latency'] = (times['rec'] - times['sent']) / 1_000_000
	file_stats.append({
		'name': filename,
		'medianlatency': times['latency'].median(),
		'99latency': times['latency'].quantile(0.99),
		'meanlatency': times['latency'].mean()
	})

file_stats_df = pd.DataFrame(file_stats)
print(file_stats_df)

# open stats file and add column
stats = pd.read_csv(f'./experiments/data/{test_name}/stats.csv', skipinitialspace=True)
stats['lost'] = 100. * (1. - (stats['rec'] / stats['sent']))
stats['diff'] = np.abs(stats['throughput'] - stats['goodput']) / stats['throughput']
stats['throughputfiltered'] = np.where(stats['diff'] <= 0.05, stats['throughput'], None)
stats = stats.merge(file_stats_df, on='name')

ax = sns.lineplot(x='throughput', y='goodput', data=stats, hue='msg_size', palette = color, linestyle='--')
ax.set(xlabel = 'throughput (req/s)', ylabel = 'goodput (req/s)', xscale = 'log', yscale = 'log')
ax.legend(title = 'message size')
fig = ax.get_figure()
fig.savefig(graph_dir + 'throughputgoodput.png')
plt.close(fig)

ax = sns.lineplot(x='throughput', y='lost', data=stats, hue='msg_size', palette = color, linestyle='--')
ax.set(xlabel = 'throughput (req/s)', ylabel = 'requests lost (%)', xscale = 'log')
ax.legend(title = 'message size')
fig = ax.get_figure()
fig.savefig(graph_dir + 'throughputlost.png')
plt.close(fig)

ax = sns.lineplot(x='throughputfiltered', y='meanlatency', data=stats, hue='msg_size', palette = color, linestyle='--') #err_style="band", estimator=np.median, ci='sd'
ax.set(xlabel = 'throughput (req/s)', ylabel = 'mean latency (ms)', xscale = 'log')
ax.legend(title = 'message size')
fig = ax.get_figure()
fig.savefig(graph_dir + 'throughputflatency.png')
plt.close(fig)

ax = sns.lineplot(x='throughput', y='meanlatency', data=stats, hue='msg_size', palette = color, linestyle='--') #err_style="band", estimator=np.median, ci='sd'
ax.set(xlabel = 'throughput (req/s)', ylabel = 'mean latency (ms)', xscale = 'log')
ax.legend(title = 'message size')
fig = ax.get_figure()
fig.savefig(graph_dir + 'throughputlatency.png')
plt.close(fig)