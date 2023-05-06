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
test_path = './data/' + test_name + '/'

# create folder for this test
os.mkdir(test_path)
os.mkdir(test_path + 'subtests')

# create stats file
with open(test_path + 'stats.csv', 'w') as f:
	# f.write('name, version, nodes, throughput, goodput, mean, sd, rec, sent, batch_size, msg_size\n')
	f.write('name, goodput, msg_size, mean_send, total_send\n')

# nodeCounts = [4, 8]
nodeCounts = [1]
# rates = [1, 10, 50, 200, 400, 600, 800, 1000, 2000, 4000, 8000, 12000]
rates = [1000] # not used
experiment_time = 10
msg_sizes = [1, 25, 50, 100, 200, 400, 600, 800, 1000, 1200, 1400, 1600, 1800, 2000, 2200, 2400, 2600, 2800, 3000, 3200, 3400, 3600, 3800, 4000, 4200, 4400, 4600, 4800, 5000, 5200, 5400, 5600, 5800, 6000]
repeats = 3

# randomise testing order
test_iter = random.sample(list(itertools.product(rates, nodeCounts, msg_sizes)) * repeats, len(rates) * len(nodeCounts) * len(msg_sizes) * repeats)

x = 0
# randomise order of tests (generate permutations...)
for (rate, n, s) in test_iter:
	name = f'{test_name}_{x}_{rate}_{n}_{s}'
	print(f'running "{name}"')
	for i in range(n):
		processes.append(subprocess.Popen(f'ulimit -n 65536; eval $(opam env) dune exec --build-dir=_build{str(i)} -- ../dummy.exe -i {str(i)}', shell=True, preexec_fn=os.setsid))
	time.sleep(5)
	subprocess.run(f'eval $(opam env) dune exec -- ../load_gen_dummy.exe {str(n)} -t {str(experiment_time)} -r {str(rate)} -s {str(s)} --times "{test_path + "subtests/" + name}.csv" --stats "{test_path}stats.csv"', shell=True)
	time.sleep(1)
	kill_processes()
	processes = []
	x += 1

# subprocess.run(f'python3 ./plotDummy.py {test_name}', shell = True)
