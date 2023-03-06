import numpy as np
import pandas as pd

import matplotlib as mpl
import matplotlib.pyplot as plt

import seaborn as sns
import seaborn.objects as so

stats = pd.read_csv('./graphs/data/stats.csv', skipinitialspace=True)
times = pd.read_csv('./graphs/data/times-05-03-2023-13:35:25.csv', skipinitialspace=True)
times['sent(s)'] = times['sent'] / 1_000_000_000
times['latency'] = (times['rec'] - times['sent']) / 1_000_000_000

print(stats)

sns.scatterplot(x='throughput', y='goodput', data=stats, hue='nodes')
plt.savefig('graphs/throughputgoodput.png')
plt.show()

sns.scatterplot(x='goodput', y='mean', data=stats, hue='nodes')
plt.savefig('graphs/goodputlatency.png')
plt.show()

sns.ecdfplot(x='latency', data=times)
plt.savefig('graphs/cumlatency.png')
plt.show()

sns.scatterplot(x='sent(s)', y='latency', data=times)
plt.savefig('graphs/timelatency.png')
plt.show()

piv = pd.pivot_table(times, values="freq", index=["send(s)"], columns=["latency"], fill_value=0)
sns.heatmap(data=piv, square=True)
plt.savefig('graphs/heatmap.png')
plt.show()