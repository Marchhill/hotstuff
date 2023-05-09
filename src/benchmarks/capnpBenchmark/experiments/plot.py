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
graph_dir = f'./graphs/{test_name}/'
if not os.path.exists(graph_dir):
	os.mkdir(graph_dir)

send_df = pd.DataFrame()
file_stats = []
for file in os.listdir(f'./data/{test_name}/subtests'):
	filename = os.fsdecode(file)
	times = pd.read_csv(f'./data/{test_name}/subtests/{filename}', skipinitialspace=True)
	filename = os.path.splitext(filename)[0]
	times['send_time'] = times['send_time'] * 1000.
	file_stats.append({
		'name': filename,
		'send(ms)': times['send_time'].median(),
		'send95(ms)': times['send_time'].quantile(0.95)
	})
	times['name'] = filename
	send_df = pd.concat([send_df, times])

file_stats_df = pd.DataFrame(file_stats)
# print(file_stats_df)

# open stats file and add column
stats = pd.read_csv(f'./data/{test_name}/stats.csv', skipinitialspace=True)
send_df = send_df.merge(stats, on='name')
stats = stats.merge(file_stats_df, on='name')
stats['goodput(mb/s)'] = stats['goodput'] / 1000000

ax = sns.lineplot(x='msg_size', y='goodput(mb/s)', data=stats)
ax.set(xlabel = 'msg size (bytes)', ylabel = 'max goodput (Mb/s)', xlim=(0,6000), ylim=(0.,14.))
fig = ax.get_figure()
fig.savefig(graph_dir + 'sizegoodput.pgf')
fig.savefig(graph_dir + 'sizegoodput.png')
plt.close(fig)

ax = sns.lineplot(x='msg_size', y='send(ms)', data=stats)
ax.set(xlabel = 'msg size (bytes)', ylabel = 'median send time (ms)', xlim=(0,6000), ylim=(0.,100.))
fig = ax.get_figure()
fig.savefig(graph_dir + 'sizemediansend.pgf')
fig.savefig(graph_dir + 'sizemediansend.png')
plt.close(fig)

ax = sns.lineplot(x='msg_size', y='send95(ms)', data=stats)
ax.set(xlabel = 'msg size (bytes)', ylabel = '95%ile send time (ms)', xlim=(0,6000), ylim=(0.,100.))
fig = ax.get_figure()
fig.savefig(graph_dir + 'size95send.pgf')
fig.savefig(graph_dir + 'size95send.png')
plt.close(fig)

# ax = sns.histplot(x='msg_size', y='send_time', data=send_df, binwidth=(500, 5.))
ax = sns.boxplot(x='msg_size', y='send_time', data=send_df, showfliers = False, whis=[5, 95])
ax.set(xlabel = 'msg size (bytes)', ylabel = 'send time (ms)', ylim = (0., 100.))
# ax.set_xticklabels(ax.get_xticklabels(), rotation=90)
for ind, label in enumerate(ax.get_xticklabels()):
	if ind % 4 == 0:
		label.set_visible(True)
		label.set_rotation(90)
	else:
		label.set_visible(False)
fig = ax.get_figure()
fig.savefig(graph_dir + 'box.pgf', bbox_inches="tight")
fig.savefig(graph_dir + 'box.png', bbox_inches="tight")
plt.close(fig)
plt.clf()