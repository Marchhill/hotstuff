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
if not os.path.isfile('./graphs/data/dummystats.csv'):
	with open('./graphs/data/dummystats.csv', 'w') as f:
		f.write('name, chained, nodes, throughput, goodput, mean, sd, rec, sent\n')

# nodeCounts = [4, 8]
nodeCounts = [1]
# rates = [500, 1000, 2000, 3000, 4000, 5000]
rates = [1000]
experiment_time = 10
batch_size = 600
repeats = 1

# randomise testing order
test_iter = random.sample(list(itertools.product(rates, nodeCounts)) * repeats, len(rates) * len(nodeCounts) * repeats)

# randomise order of tests (generate permutations...)
for (rate, n) in test_iter:
	name = datetime.now().strftime("dummy_%d-%m-%Y-%H:%M:%S")
	for i in range(n):
		processes.append(subprocess.Popen('ulimit -n 65536; eval $(opam env) OCAML_LANDMARKS=format=json,output=profile' + str(i) + '.json dune exec --build-dir=_build' + str(i) + ' -- ./dummy.exe -i ' + str(i), shell=True, preexec_fn=os.setsid))
	time.sleep(5)
	subprocess.run('eval $(opam env) OCAML_LANDMARKS=output=profile dune exec -- ./live_test.exe ' + str(n) + ' -t ' + str(experiment_time) + ' -r ' + str(rate) + ' -s ' + str(batch_size) + ' --times "./graphs/data/times-' + name + '.csv" --stats "./graphs/data/dummystats.csv"', shell=True)
	time.sleep(1)
	kill_processes()
	processes = []