import numpy as np
import pandas as pd

import matplotlib as mpl
import matplotlib.pyplot as plt

import seaborn as sns
import seaborn.objects as so

# stats = pd.read_csv('./graphs/data/dummystats.csv', skipinitialspace=True)
stats = pd.read_csv('./graphs/data/stats.csv', skipinitialspace=True)
# times = pd.read_csv('./graphs/data/times-dummy_16-03-2023-16:25:00.csv', skipinitialspace=True)
# times = pd.read_csv('./graphs/data/times-dummy_27-03-2023-23:13:56.csv', skipinitialspace=True)
times = pd.read_csv('./graphs/data/times-28-03-2023-12:56:05.csv', skipinitialspace=True)
times['sent(s)'] = times['sent'] / 1_000_000_000
times['latency'] = (times['rec'] - times['sent']) / 1_000_000
stats['lost'] = 1. - (stats['rec'] / stats['sent'])

print(stats)

'''
sns.lineplot(x='throughput', y='goodput', data=stats, hue='nodes')
plt.savefig('graphs/throughputgoodput.png')
plt.show()

sns.lineplot(x='throughput', y='lost', data=stats, hue='nodes')
plt.savefig('graphs/throughputlost.png')
plt.show()

sns.lineplot(x='goodput', y='mean', data=stats, hue='nodes')
plt.savefig('graphs/goodputlatency.png')
plt.show()

sns.ecdfplot(x='latency', data=times)
plt.savefig('graphs/cumlatency.png')
plt.show()
'''

fig, ax = plt.subplots(1, 1, figsize=(10, 5), constrained_layout=True)
# ax = sns.histplot(times, x="sent(s)", y="latency", binwidth=(0.05, 10), ax=ax)
ax = sns.histplot(times, x="sent(s)", y="latency", bins = 50, ax=ax)
ax.set(xlabel="Time (s)", xlim = (0,10), ylabel = "Latency (ms)")
fig = ax
plt.savefig('graphs/timelatencyheatmap.png')
plt.show()