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

# create directory to output graphs to
graph_dir = f'./experiments/graphs/{test_name}/'
subtests_dir = f'./experiments/graphs/{test_name}/subtests/'
if not os.path.exists(graph_dir):
	os.mkdir(graph_dir)
if not os.path.exists(subtests_dir):
	os.mkdir(subtests_dir)

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

	# cumulative latency
	ax = sns.ecdfplot(x='latency', data=times, palette = color)
	ax.set(xlabel = 'latency (ms)', ylabel = 'fraction of requests', xscale = 'log')
	fig = ax.get_figure()
	fig.savefig(f'{subtests_dir + filename}_cumlatency.png')
	plt.close(fig)

	# time / latency heatmap
	fig, ax = plt.subplots(1, 1, figsize=(10, 5), constrained_layout=True)
	# ax = sns.histplot(times, x="sent(s)", y="latency", binwidth=(0.05, 10), ax=ax)
	ax = sns.histplot(times, x="sent(s)", y="latency", bins = 50, ax=ax, palette = color)
	ax.set(xlabel="time (s)", xlim=(0,10), ylabel = "latency (ms)")
	fig.savefig(f'{subtests_dir + filename}_timelatencyheatmap.png')
	plt.close(fig)

file_stats_df = pd.DataFrame(file_stats)
print(file_stats_df)

# open stats file and add column
stats = pd.read_csv(f'./experiments/data/{test_name}/stats.csv', skipinitialspace=True)
stats['lost'] = 100. * (1. - (stats['rec'] / stats['sent']))
stats['diff'] = np.abs(stats['throughput'] - stats['goodput']) / stats['throughput']
stats['throughputfiltered'] = np.where(stats['diff'] <= 0.05, stats['throughput'], None)
stats = stats.merge(file_stats_df, on='name')
print(stats)

ax = sns.lineplot(x='throughput', y='goodput', data=stats, hue='batch_size', palette = color, linestyle='--')
ax.set(xlabel = 'throughput (req/s)', ylabel = 'goodput (req/s)')
fig = ax.get_figure()
ax.legend(title = 'batch size')
fig.savefig(graph_dir + 'throughputgoodput.png')
plt.close(fig)

ax = sns.lineplot(x='throughput', y='lost', data=stats, hue='batch_size', palette = color, linestyle='--')
ax.set(xlabel = 'throughput (req/s)', ylabel = 'requests lost (%)', xscale = 'log')
fig = ax.get_figure()
ax.legend(title = 'batch size')
fig.savefig(graph_dir + 'throughputlost.png')
plt.close(fig)

ax = sns.lineplot(x='throughput', y='meanlatency', data=stats, hue='batch_size', palette = color, linestyle='--')
# u = stats.where(stats['batch_size'] == 'u')
# ax.fill_between(['goodput'], stats['medianlatency'], stats['99latency'], alpha=0.2)
ax.set(xlabel = 'throughput (req/s)', ylabel = 'mean latency (ms)')
ax.legend(title = 'batch size')
fig = ax.get_figure()
fig.savefig(graph_dir + 'throughputlatency.png')
plt.close(fig)

ax = sns.lineplot(x='goodput', y='meanlatency', data=stats, hue='batch_size', palette = color, linestyle='--')
# u = stats.where(stats['batch_size'] == 'u')
# ax.fill_between(['goodput'], stats['medianlatency'], stats['99latency'], alpha=0.2)
ax.set(xlabel = 'goodput (req/s)', ylabel = 'mean latency (ms)')
ax.legend(title = 'batch size')
fig = ax.get_figure()
fig.savefig(graph_dir + 'goodputlatency.png')
plt.close(fig)

ax = sns.lineplot(x='throughputfiltered', y='meanlatency', data=stats, hue='batch_size', palette = color, linestyle='--')
# u = stats.where(stats['batch_size'] == 'u')
# ax.fill_between(['goodput'], stats['medianlatency'], stats['99latency'], alpha=0.2)
ax.set(xlabel = 'throughput (req/s)', ylabel = 'mean latency (ms)')
ax.legend(title = 'batch size')
fig = ax.get_figure()
fig.savefig(graph_dir + 'throughputflatency.png')
plt.close(fig)

ax = sns.lineplot(x='throughput', y='goodput', data=stats, hue='nodes', palette = color, linestyle='--')
ax.set(xlabel = 'throughput (req/s)', ylabel = 'goodput (req/s)')
fig = ax.get_figure()
fig.savefig(graph_dir + 'throughputgoodput_nodes.png')
plt.close(fig)

ax = sns.lineplot(x='throughput', y='lost', data=stats, hue='nodes', palette = color, linestyle='--')
ax.set(xlabel = 'throughput (req/s)', ylabel = 'requests lost (%)', xscale = 'log')
fig = ax.get_figure()
fig.savefig(graph_dir + 'throughputlost_nodes.png')
plt.close(fig)

ax = sns.lineplot(x='throughputfiltered', y='meanlatency', data=stats, hue='nodes', palette = color, linestyle='--')
ax.set(xlabel = 'throughput (req/s)', ylabel = 'mean latency (ms)')
fig = ax.get_figure()
fig.savefig(graph_dir + 'throughputflatency_nodes.png')
plt.close(fig)

ax = sns.lineplot(x='throughput', y='meanlatency', data=stats, hue='nodes', palette = color, linestyle='--')
ax.set(xlabel = 'throughput (req/s)', ylabel = 'mean latency (ms)')
fig = ax.get_figure()
fig.savefig(graph_dir + 'throughputlatency_nodes.png')
plt.close(fig)

ax = sns.lineplot(x='throughput', y='goodput', data=stats, hue='version', palette = color, linestyle='--')
ax.set(xlabel = 'throughput (req/s)', ylabel = 'goodput (req/s)')
fig = ax.get_figure()
ax.legend(title = 'version')
fig.savefig(graph_dir + 'throughputgoodput_ablation.png')
plt.close(fig)

ax = sns.lineplot(x='throughput', y='lost', data=stats, hue='version', palette = color, linestyle='--')
ax.set(xlabel = 'throughput (req/s)', ylabel = 'requests lost (%)', xscale = 'log')
fig = ax.get_figure()
ax.legend(title = 'version')
fig.savefig(graph_dir + 'throughputlost_ablation.png')
plt.close(fig)

ax = sns.lineplot(x='throughputfiltered', y='meanlatency', data=stats, hue='version', palette = color, linestyle='--')
ax.set(xlabel = 'throughput (req/s)', ylabel = 'mean latency (ms)')
ax.legend(title = 'version')
fig = ax.get_figure()
fig.savefig(graph_dir + 'throughputflatency_ablation.png')
plt.close(fig)

ax = sns.lineplot(x='throughput', y='meanlatency', data=stats, hue='version', palette = color, linestyle='--')
ax.set(xlabel = 'throughput (req/s)', ylabel = 'mean latency (ms)')
fig = ax.get_figure()
fig.savefig(graph_dir + 'throughputlatency_ablation.png')
plt.close(fig)