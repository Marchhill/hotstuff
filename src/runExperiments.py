import subprocess
from datetime import datetime
import os
import sys
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

test_name = "test_" + datetime.now().strftime("%d-%m-%Y-%H:%M:%S")
if len(sys.argv) >= 2:
    test_name = sys.argv[1]

test_path = './experiments/data/' + test_name + '/'

# create folder for this test
os.mkdir(test_path)
os.mkdir(test_path + 'subtests')

# create stats file
with open(test_path + 'stats.csv', 'w') as f:
    f.write('name, version, nodes, throughput, goodput, mean, sd, rec, sent, batch_size, msg_size\n')

#nodeCounts = [1, 2, 4, 7, 10, 13]
nodeCounts = [4]
rates = [1, 10, 50, 100, 200, 400, 600, 800, 1000, 1500, 2000, 4000]
# rates = [100, 200, 500, 1000, 2000, 4000]
#batch_sizes = [1, 50, 100, 300, 450, 600, 99999999]
batch_sizes = [2000]
experiment_time = 10
repeats = 3
version = "4"

# randomise testing order
test_iter = random.sample(list(itertools.product(rates, nodeCounts, batch_sizes)) * repeats, len(rates) * len(nodeCounts) * len(batch_sizes) * repeats)

x = 0
# randomise order of tests (generate permutations...)
for (rate, n, s) in test_iter:
    succ = False
    while not succ:
        name = f'{test_name}_{x}_{rate}_{n}_{s}'
        print(f'running "{name}"')
        for i in range(n):
            processes.append(subprocess.Popen(
                f'ulimit -n 65536; eval $(opam env) dune exec --build-dir=_build{str(i)} -- ./main.exe -i {str(i)} -n {str(n)} -b {str(s)}', shell=True, preexec_fn=os.setsid))
        time.sleep(5)
        cmd = f'eval $(opam env) dune exec -- ./live_test.exe {str(n)} -t {str(experiment_time)} --version "{version}" -r {str(rate)} -b {str(s)} --times "{test_path + "subtests/" + name}.csv" --stats "{test_path}stats.csv"'
        print(cmd)
        completed = subprocess.run(cmd, shell=True)
        if completed.returncode == 0:
            succ = True
        time.sleep(1)
        kill_processes()
        processes = []
    x += 1

subprocess.run(f'python3 ./experiments/plot.py {test_name}', shell=True)
