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
		try:
			os.killpg(os.getpgid(p.pid), signal.SIGKILL)
		except:
			print("already killed!")

atexit.register(kill_processes)

# create stats file if doesn't exist
if not os.path.isfile('./graphs/data/stats.csv'):
	with open('./graphs/data/stats.csv', 'w') as f:
		f.write('name, chained, nodes, throughput, goodput, mean, sd, rec, sent\n')

nodeCounts = [4]
# nodeCounts = [1]
# rates = [500, 1000, 2000, 3000, 4000, 5000]
rates = [1000]
# throughput = [2000]
# rates = [10000]
experiment_time = 10
repeats = 1

# randomise testing order
test_iter = random.sample(list(itertools.product(rates, nodeCounts)) * repeats, len(rates) * len(nodeCounts) * repeats)

# randomise order of tests (generate permutations...)
for (rate, n) in test_iter:
	name = datetime.now().strftime("%d-%m-%Y-%H:%M:%S")
	print(f"running test \"{name}\": rate = {rate}req/s, nodes = {n}")
	for i in range(n):
		processes.append(subprocess.Popen('ulimit -n 65536; eval $(opam env) OCAML_LANDMARKS=format=json,output=profile' + str(i) + '.json dune exec --build-dir=_build' + str(i) + ' -- ./main.exe -v -i ' + str(i) + ' -n' + str(n), shell=True, preexec_fn=os.setsid))
	time.sleep(5)
	subprocess.run('eval $(opam env) OCAML_LANDMARKS=output=profile dune exec -- ./live_test.exe ' + str(n) + ' -t ' + str(experiment_time) + ' -r ' + str(rate) + ' --times "./graphs/data/times-' + name + '.csv" --stats "./graphs/data/stats.csv"', shell=True)
	time.sleep(1)
	kill_processes()
	processes = []