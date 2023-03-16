import subprocess
from datetime import datetime
import os
import time
import signal
import atexit
import itertools
import random

processes = []

def kill_processes():
	for p in processes:
		os.killpg(os.getpgid(p.pid), signal.SIGKILL)

atexit.register(kill_processes)

# create stats file if doesn't exist
if not os.path.isfile('./graphs/data/stats.csv'):
	with open('./graphs/data/stats.csv', 'w') as f:
		f.write('name, chained, nodes, throughput, goodput, mean, sd\n')

nodeCounts = [4, 8]
# nodeCounts = [1]
rates = [200, 400, 600, 800, 1000, 1200, 1400, 1600, 1800, 2000]
# throughput = [2000]
# rates = [10000]
experiment_time = 10
repeats = 3

# randomise testing order
test_iter = random.sample(list(itertools.product(rates, nodeCounts)) * repeats, len(rates) * len(nodeCounts) * repeats)

# randomise order of tests (generate permutations...)
for (rate, n) in test_iter:
	name = datetime.now().strftime("%d-%m-%Y-%H:%M:%S")
	for i in range(n):
		processes.append(subprocess.Popen('ulimit -n 65536; eval $(opam env) OCAML_LANDMARKS=format=json,output=profile' + str(i) + '.json dune exec --build-dir=_build' + str(i) + ' -- ./main.exe -i ' + str(i) + ' -n' + str(n), shell=True, preexec_fn=os.setsid))
	time.sleep(5)
	subprocess.run('eval $(opam env) OCAML_LANDMARKS=output=profile dune exec -- ./live_test.exe ' + str(n) + ' -t ' + str(experiment_time) + ' -r ' + str(rate) + ' --times "./graphs/data/times-' + name + '.csv" --stats "./graphs/data/stats.csv"', shell=True)
	time.sleep(1)
	kill_processes()
	processes = []