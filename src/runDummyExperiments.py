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

test_name = "dummytest_" + datetime.now().strftime("%d-%m-%Y-%H:%M:%S")
test_path = './experiments/data/' + test_name + '/'

# create folder for this test
os.mkdir(test_path)
os.mkdir(test_path + 'subtests')

# create stats file
with open(test_path + 'stats.csv', 'w') as f:
	f.write('name, version, nodes, throughput, goodput, mean, sd, rec, sent, batch_size, msg_size\n')

# nodeCounts = [4, 8]
nodeCounts = [1]
rates = [1, 10, 50, 200, 400, 600, 800, 1000, 2000, 4000, 8000, 12000]
experiment_time = 10
msg_sizes = [10, 100, 500, 1000, 2000, 3000, 4000, 5000, 6000]
repeats = 3

# randomise testing order
test_iter = random.sample(list(itertools.product(rates, nodeCounts, msg_sizes)) * repeats, len(rates) * len(nodeCounts) * len(msg_sizes) * repeats)

x = 0
# randomise order of tests (generate permutations...)
for (rate, n, s) in test_iter:
	name = f'{test_name}_{x}_{rate}_{n}_{s}'
	print(f'running "{name}"')
	for i in range(n):
		processes.append(subprocess.Popen(f'ulimit -n 65536; eval $(opam env) dune exec --build-dir=_build{str(i)} -- ./dummy.exe -i {str(i)}', shell=True, preexec_fn=os.setsid))
	time.sleep(5)
	subprocess.run(f'eval $(opam env) dune exec -- ./live_test.exe {str(n)} -t {str(experiment_time)} -r {str(rate)} -s {str(s)} --times "{test_path + "subtests/" + name}.csv" --stats "{test_path}stats.csv"', shell=True)
	time.sleep(1)
	kill_processes()
	processes = []
	x += 1

subprocess.run(f'python3 ./experiments/plotDummy.py {test_name}', shell = True)
