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
	s = os.path.splitext(filename)
	if s[1] != '.csv':
		continue
	times = pd.read_csv(f'./data/{test_name}/subtests/{filename}', skipinitialspace=True, engine='python')
	filename = s[0]
	times['sent(s)'] = times['sent'] / 1_000_000_000
	times['latency'] = (times['rec'] - times['sent']) / 1_000_000_000
	file_stats.append({
		'name': filename,
		'medianlatency': times['latency'].median(),
		'95latency': times['latency'].quantile(0.95),
		'meanlatency': times['latency'].mean()
	})

	# cumulative latency
	# ax = sns.ecdfplot(x='latency', data=times, palette = color)
	# ax.set(xlabel = 'latency (s)', ylabel = 'fraction of requests')
	# fig = ax.get_figure()
	# fig.savefig(f'{subtests_dir + filename}_cumlatency.pgf')
	# fig.savefig(f'{subtests_dir + filename}_cumlatency.png')
	# plt.close(fig)
	# plt.clf()

	# # # time / latency heatmap
	# fig, ax = plt.subplots(1, 1, figsize=(10, 3), constrained_layout=True)
	# # ax = sns.histplot(times, x="sent(s)", y="latency", binwidth=(0.05, 10), ax=ax)
	# ax = sns.histplot(times, x="sent(s)", y="latency", bins = 50, ax=ax, palette = color)
	# ax.set(xlabel="time (s)", xlim=(0,10), ylabel = "latency (s)")
	# fig.savefig(f'{subtests_dir + filename}_timelatencyheatmap.pgf')
	# fig.savefig(f'{subtests_dir + filename}_timelatencyheatmap.png')
	# plt.close(fig)
	# plt.clf()

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
stats = pd.read_csv(f'./data/{test_name}/stats.csv', skipinitialspace=True, engine='python')
stats = stats.merge(file_stats_df, on='name')
stats['lost'] = 100. * (1. - (stats['rec'] / stats['sent']))
stats['diff'] = np.abs(stats['throughput'] - stats['goodput']) / stats['throughput']
stats['mininet_medianlatency'] = np.where(stats['medianlatency'] <= 4., stats['medianlatency'], None)
stats['mininet_95latency'] = np.where(stats['95latency'] <= 4., stats['95latency'], None)
stats['medianlatency'] = np.where(stats['medianlatency'] <= 1., stats['medianlatency'], None)
stats['95latency'] = np.where(stats['95latency'] <= 1., stats['95latency'], None)
# print(stats)

# voodoo to fix type error
stats[['goodput', 'medianlatency']] = stats[['goodput', 'medianlatency']].astype(float)
stats[['goodput', '95latency']] = stats[['goodput', '95latency']].astype(float)
stats[['goodput', 'mininet_medianlatency']] = stats[['goodput', 'mininet_medianlatency']].astype(float)
stats[['goodput', 'mininet_95latency']] = stats[['goodput', 'mininet_95latency']].astype(float)

ax = sns.lineplot(x='throughput', y='goodput', data=stats, hue='batch_size', palette = color, linestyle='--')
ax.set(xlabel = 'throughput (req/s)', ylabel = 'goodput (req/s)', xlim = (0, 2000), ylim = (0., 1000.))
fig = ax.get_figure()
ax.legend(title = 'batch size')
fig.savefig(graph_dir + 'throughputgoodput.pgf')
fig.savefig(graph_dir + 'throughputgoodput.png')
plt.close(fig)
plt.clf()

g = sns.lmplot(x='goodput', y='medianlatency', data=stats, hue='batch_size', palette = color, ci = None, order = 2, line_kws={'ls': '--'})
g.legend.remove()
ax = g.axes[0][0]
ax.set(xlabel = 'goodput (req/s)', ylabel = 'median latency (s)', xlim = (0., 1000.), ylim = (0., 1.0))
ax.legend(title = 'batch size')
fig = ax.get_figure()
fig.savefig(graph_dir + 'goodputlatency.pgf')
fig.savefig(graph_dir + 'goodputlatency.png')
plt.close(fig)
plt.clf()

g = sns.lmplot(x='goodput', y='95latency', data=stats, hue='batch_size', palette = color, ci = None, order = 4)
g.legend.remove()
ax.set(xlabel = 'goodput (req/s)', ylabel = '95%ile latency (s)', xlim = (0., 1000.), ylim = (0., 1.))
ax.legend(title = 'batch size')
fig = ax.get_figure()
fig.savefig(graph_dir + 'goodput95latency.pgf')
fig.savefig(graph_dir + 'goodput95latency.png')
plt.close(fig)
plt.clf()

ax = sns.lineplot(x='throughput', y='goodput', data=stats, hue='nodes', palette = color, linestyle='--')
ax.set(xlabel = 'throughput (req/s)', ylabel = 'goodput (req/s)', xlim = (0, 4000), ylim = (0., 4000.))
fig = ax.get_figure()
ax.legend(title = 'node count')
fig.savefig(graph_dir + 'throughputgoodput_nodes.pgf')
fig.savefig(graph_dir + 'throughputgoodput_nodes.png')
plt.close(fig)
plt.clf()

g = sns.lmplot(x='goodput', y='medianlatency', data=stats, hue='nodes', palette = color, ci = None, order = 3)
g.legend.remove()
ax = g.axes[0][0]
ax.set(xlabel = 'goodput (req/s)', ylabel = 'median latency (s)', ylim = (0., None), xscale='log')
ax.legend(title = 'node count')
fig = ax.get_figure()
fig.savefig(graph_dir + 'goodputlatency_nodes.pgf')
fig.savefig(graph_dir + 'goodputlatency_nodes.png')
plt.close(fig)
plt.clf()

g = sns.lmplot(x='goodput', y='95latency', data=stats, hue='nodes', palette = color, ci = None, order = 3)
g.legend.remove()
ax = g.axes[0][0]
ax.set(xlabel = 'goodput (req/s)', ylabel = '95%ile latency (s)', ylim = (0., None), xscale='log')
ax.legend(title = 'node count')
fig = ax.get_figure()
fig.savefig(graph_dir + 'goodput95latency_nodes.pgf')
fig.savefig(graph_dir + 'goodput95latency_nodes.png')
plt.close(fig)
plt.clf()

ax = sns.lineplot(x='throughput', y='goodput', data=stats, hue='version', palette = color, linestyle='--')
ax.set(xlabel = 'throughput (req/s)', ylabel = 'goodput (req/s)', xlim = (0, 2000), ylim = (0., 1200.))
fig = ax.get_figure()
ax.legend(title = 'version')
fig.savefig(graph_dir + 'throughputgoodput_ablation.pgf')
fig.savefig(graph_dir + 'throughputgoodput_ablation.png')
plt.close(fig)
plt.clf()

g = sns.lmplot(x='goodput', y='medianlatency', data=stats, hue='version', palette = color, ci = None, order = 3)
g.legend.remove()
ax = g.axes[0][0]
ax.set(xlabel = 'goodput (req/s)', ylabel = 'median latency (s)', ylim = (0., 1.), xscale = 'log')
ax.legend(title = 'version')
fig = ax.get_figure()
fig.savefig(graph_dir + 'goodputlatency_ablation.pgf')
fig.savefig(graph_dir + 'goodputlatency_ablation.png')
plt.close(fig)
plt.clf()

g = sns.lmplot(x='goodput', y='95latency', data=stats, hue='version', palette = color, ci = None, order = 3)
g.legend.remove()
ax = g.axes[0][0]
ax.set(xlabel = 'goodput (req/s)', ylabel = '95%ile latency (s)', ylim = (0., 1.), xscale = 'log')
ax.legend(title = 'version')
fig = ax.get_figure()
fig.savefig(graph_dir + 'goodput95latency_ablation.pgf')
fig.savefig(graph_dir + 'goodput95latency_ablation.png')
plt.close(fig)
plt.clf()

ax = sns.lineplot(x='throughput', y='goodput', data=stats, palette = color, linestyle='--')
ax.set(xlabel = 'throughput (req/s)', ylabel = 'goodput (req/s)', xlim = (0, None), ylim = (0., None))
fig = ax.get_figure()
fig.savefig(graph_dir + 'throughputgoodput_mininet.pgf')
fig.savefig(graph_dir + 'throughputgoodput_mininet.png')
plt.close(fig)
plt.clf()

g = sns.lmplot(x='goodput', y='mininet_medianlatency', data=stats, palette = color, ci = None, order = 5)
ax = g.axes[0][0]
ax.set(xlabel = 'goodput (req/s)', ylabel = 'median latency (s)', xlim = (0., None), ylim = (0., 4.))
fig = ax.get_figure()
fig.savefig(graph_dir + 'goodputlatency_mininet.pgf')
fig.savefig(graph_dir + 'goodputlatency_mininet.png')
plt.close(fig)
plt.clf()

g = sns.lmplot(x='goodput', y='mininet_95latency', data=stats, palette = color, ci = None, order = 5)
ax.set(xlabel = 'goodput (req/s)', ylabel = '95%ile latency (s)', xlim = (0., None), ylim = (0., None))
fig = ax.get_figure()
fig.savefig(graph_dir + 'goodput95latency_mininet.pgf')
fig.savefig(graph_dir + 'goodput95latency_mininet.png')
plt.close(fig)
plt.clf()