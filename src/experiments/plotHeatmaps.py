import sys
import os
import numpy as np
import pandas as pd
import matplotlib as mpl
import matplotlib.pyplot as plt
import seaborn as sns
import seaborn.objects as so

color = sns.color_palette("colorblind")

subtests_dir = f'./graphs/heatmaps/'

timesStable = pd.read_csv(f'./data/batch_sizes_7/subtests/batch_sizes_7_157_200_4_300.csv', skipinitialspace=True)
# timesStable = pd.read_csv(f'./data/batch_sizes_7/subtests/batch_sizes_7_22_600_4_300.csv', skipinitialspace=True)
# timesStable = pd.read_csv(f'./data/batch_sizes_7/subtests/batch_sizes_7_132_800_4_300.csv', skipinitialspace=True)
timesStable['sent(s)'] = timesStable['sent'] / 1_000_000_000
timesStable['latency'] = (timesStable['rec'] - timesStable['sent']) / 1_000_000_000
timesStable['latency(ms)'] = (timesStable['rec'] - timesStable['sent']) / 1_000_000

timesLinear = pd.read_csv(f'./data/batch_sizes_7/subtests/batch_sizes_7_279_2000_4_300.csv', skipinitialspace=True)
timesLinear['sent(s)'] = timesLinear['sent'] / 1_000_000_000
timesLinear['latency'] = (timesLinear['rec'] - timesLinear['sent']) / 1_000_000_000

timesExp = pd.read_csv(f'./data/batch_sizes_7/subtests/batch_sizes_7_290_2000_4_99999999.csv', skipinitialspace=True)
timesExp['sent(s)'] = timesExp['sent'] / 1_000_000_000
timesExp['latency'] = (timesExp['rec'] - timesExp['sent']) / 1_000_000_000

# cumulative latency
ax = sns.ecdfplot(x='latency(ms)', data=timesStable, palette = color)
ax.set(xlabel = 'latency (ms)', ylabel = 'proportion of requests')
fig = ax.get_figure()
fig.savefig(f'{subtests_dir}cumlatency.pgf')
fig.savefig(f'{subtests_dir}cumlatency.png')
plt.close(fig)
plt.clf()

# time / latency heatmap
fig, (ax1, ax2, ax3) = plt.subplots(3, 1, figsize=(10, 7), constrained_layout=True)

ax1 = sns.histplot(timesStable, x="sent(s)", y="latency", bins = 50, ax=ax1, palette = color)
ax1.set(title = "(a)", xlabel=None, xlim=(0,20), ylim=(0., None), ylabel = None)
ax1.xaxis.set_ticklabels([])

ax2 = sns.histplot(timesLinear, x="sent(s)", y="latency", bins = 50, ax=ax2, palette = color)
ax2.set(title = "(b)", xlabel=None, xlim=(0,20), ylim=(0., None), ylabel = None)
ax2.xaxis.set_ticklabels([])

ax3 = sns.histplot(timesExp, x="sent(s)", y="latency", bins = 50, ax=ax3, palette = color)
ax3.set(title = "(c)", xlabel=None, xlim=(0,20), ylim=(0., None), ylabel = None)

fig.supxlabel('time (s)')
fig.supylabel('latency (s)')
fig.savefig(f'{subtests_dir}timelatencyheatmap.pgf')
fig.savefig(f'{subtests_dir}timelatencyheatmap.png')
plt.close(fig)
plt.clf()