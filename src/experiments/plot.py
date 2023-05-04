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
graph_dir = f'./graphs/{test_name}/'
subtests_dir = f'./graphs/{test_name}/subtests/'
if not os.path.exists(graph_dir):
	os.mkdir(graph_dir)
if not os.path.exists(subtests_dir):
	os.mkdir(subtests_dir)

file_stats = []
for file in os.listdir(f'./data/{test_name}/subtests'):
	filename = os.fsdecode(file)
	times = pd.read_csv(f'./data/{test_name}/subtests/{filename}', skipinitialspace=True)
	filename = os.path.splitext(filename)[0]
	times['sent(s)'] = times['sent'] / 1_000_000_000
	times['latency'] = (times['rec'] - times['sent']) / 1_000_000_000
	file_stats.append({
		'name': filename,
		'medianlatency': times['latency'].median(),
		'95latency': times['latency'].quantile(0.95),
		'meanlatency': times['latency'].mean()
	})

	# # cumulative latency
	# ax = sns.ecdfplot(x='latency', data=times, palette = color)
	# ax.set(xlabel = 'latency (s)', ylabel = 'fraction of requests', xscale = 'log')
	# fig = ax.get_figure()
	# fig.savefig(f'{subtests_dir + filename}_cumlatency.png')
	# plt.close(fig)

	# # time / latency heatmap
	# fig, ax = plt.subplots(1, 1, figsize=(10, 5), constrained_layout=True)
	# # ax = sns.histplot(times, x="sent(s)", y="latency", binwidth=(0.05, 10), ax=ax)
	# ax = sns.histplot(times, x="sent(s)", y="latency", bins = 50, ax=ax, palette = color)
	# ax.set(xlabel="time (s)", xlim=(0,20), ylabel = "latency (s)")
	# fig.savefig(f'{subtests_dir + filename}_timelatencyheatmap.png')
	# plt.close(fig)

	# time / latency heatmap (log)
	# fig, ax = plt.subplots(1, 1, figsize=(10, 5), constrained_layout=True)
	# # ax = sns.histplot(times, x="sent(s)", y="latency", binwidth=(0.05, 10), ax=ax)
	# ax = sns.histplot(times, x="sent(s)", y="latency", bins = 50, ax=ax, palette = color)
	# ax.set(xlabel="time (s)", xlim=(0,20), ylabel = "latency (s)", yscale = 'log')
	# fig.savefig(f'{subtests_dir + filename}_timelatencyheatmap_log.png')
	# plt.close(fig)

file_stats_df = pd.DataFrame(file_stats)
print(file_stats_df)

# open stats file and add column
stats = pd.read_csv(f'./data/{test_name}/stats.csv', skipinitialspace=True)
stats['lost'] = 100. * (1. - (stats['rec'] / stats['sent']))
stats['diff'] = np.abs(stats['throughput'] - stats['goodput']) / stats['throughput']
stats['throughputfiltered'] = np.where(stats['diff'] <= 0.05, stats['throughput'], None)
stats['latencyfiltered'] = np.where(stats['mean'] <= 1., stats['mean'], None)
stats = stats.merge(file_stats_df, on='name')
# print(stats)

ax = sns.lineplot(x='throughput', y='goodput', data=stats, hue='batch_size', palette = color, linestyle='--')
ax.set(xlabel = 'throughput (req/s)', ylabel = 'goodput (req/s)', ylim = (0., 1000.))
fig = ax.get_figure()
ax.legend(title = 'batch size')
fig.savefig(graph_dir + 'throughputgoodput.png')
plt.close(fig)

ax = sns.scatterplot(x='goodput', y='medianlatency', data=stats, hue='batch_size', palette = color, linestyle='--')
ax.set(xlabel = 'goodput (req/s)', ylabel = 'median latency (s)', ylim = (0., 1.))
ax.legend(title = 'batch size')
fig = ax.get_figure()
fig.savefig(graph_dir + 'goodputlatency.png')
plt.close(fig)

ax = sns.scatterplot(x='goodput', y='95latency', data=stats, hue='batch_size', palette = color, linestyle='--')
ax.set(xlabel = 'goodput (req/s)', ylabel = '95%ile latency (s)', ylim = (0., 1.))
ax.legend(title = 'batch size')
fig = ax.get_figure()
fig.savefig(graph_dir + 'goodput95latency.png')
plt.close(fig)

ax = sns.lineplot(x='throughput', y='goodput', data=stats, hue='nodes', palette = color, linestyle='--')
ax.set(xlabel = 'throughput (req/s)', ylabel = 'goodput (req/s)')
fig = ax.get_figure()
fig.savefig(graph_dir + 'throughputgoodput_nodes.png')
plt.close(fig)

ax = sns.scatterplot(x='goodput', y='medianlatency', data=stats, hue='nodes', palette = color, linestyle='--')
ax.set(xlabel = 'goodput (req/s)', ylabel = 'median latency (s)', ylim = (0., 1.), xscale = 'log')
fig = ax.get_figure()
fig.savefig(graph_dir + 'goodputlatency_nodes.png')
plt.close(fig)

ax = sns.scatterplot(x='goodput', y='95latency', data=stats, hue='nodes', palette = color, linestyle='--')
ax.set(xlabel = 'goodput (req/s)', ylabel = '95%ile latency (s)', ylim = (0., 1.), xscale = 'log')
fig = ax.get_figure()
fig.savefig(graph_dir + 'goodput95latency_nodes.png')
plt.close(fig)

ax = sns.lineplot(x='throughput', y='goodput', data=stats, hue='version', palette = color, linestyle='--')
ax.set(xlabel = 'throughput (req/s)', ylabel = 'goodput (req/s)')
fig = ax.get_figure()
ax.legend(title = 'version')
fig.savefig(graph_dir + 'throughputgoodput_ablation.png')
plt.close(fig)

ax = sns.scatterplot(x='goodput', y='medianlatency', data=stats, hue='version', palette = color, linestyle='--')
ax.set(xlabel = 'goodput (req/s)', ylabel = 'median latency (s)', ylim = (0., 1.), xscale = 'log')
ax.legend(title = 'version')
fig = ax.get_figure()
fig.savefig(graph_dir + 'goodputlatency_ablation.png')
plt.close(fig)

ax = sns.scatterplot(x='goodput', y='95latency', data=stats, hue='version', palette = color, linestyle='--')
ax.set(xlabel = 'goodput (req/s)', ylabel = '95%ile latency (s)', ylim = (0., 1.), xscale = 'log')
ax.legend(title = 'version')
fig = ax.get_figure()
fig.savefig(graph_dir + 'goodput95latency_ablation.png')
plt.close(fig)

# ax = sns.pairplot(x='goodput', y='latencyfiltered', data=stats, hue='version', palette = color)
# ax = sns.pairplot(stats.where(stats['mean'] < 1.), vars=['mean', 'goodput'], hue='version', palette = color)
# ax.set(xlabel = 'goodput (req/s)', ylabel = 'mean latency (s)')
# ax.legend(title = 'version')
# ax.invert_yaxis()
# fig = ax.get_figure()
# fig.savefig(graph_dir + 'goodputlatency_ablation.png')
# plt.close(fig)
# plt.show()