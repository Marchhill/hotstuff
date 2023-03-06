import subprocess
from datetime import datetime
import os
import time
import signal
import atexit


processes = []
def exit_handler():
	for p in processes:
		os.killpg(os.getpgid(p.pid), signal.SIGTERM)

atexit.register(exit_handler)

# create stats file if doesn't exist
if not os.path.isfile('./graphs/data/stats.csv'):
	with open('./graphs/data/stats.csv', 'w') as f:
		f.write('name, chained, nodes, throughput, goodput, mean, sd\n')

# nodeCounts = [4, 8]
nodeCounts = [4]
# throughput = [200, 400, 600, 800, 1000, 1200, 1400, 1600, 1800, 2000]
# throughput = [2000]
rates = [10000]
experiment_time = 20

for _ in range(3):
	for rate in rates:
		for n in nodeCounts:
			name = datetime.now().strftime("%d-%m-%Y-%H:%M:%S")
			for i in range(n):
				processes.append(subprocess.Popen('ulimit -n 65536; eval $(opam env) OCAML_LANDMARKS=format=json,output=profile' + str(i) + '.json dune exec --build-dir=_build' + str(i) + ' -- ./main.exe -i ' + str(i) + ' -n' + str(n), shell=True, preexec_fn=os.setsid))
			time.sleep(5)
			subprocess.run('eval $(opam env) OCAML_LANDMARKS=output=profile dune exec -- ./live_test.exe ' + str(n) + ' -t ' + str(experiment_time) + ' -r ' + str(rate) + ' --times "./graphs/data/times-' + name + '.csv" --stats "./graphs/data/stats.csv"', shell=True)
			time.sleep(1)
			processes = []