x=$(expr $1 - 1)

for id in $(seq 0 $x);
do
	xfce4-terminal --hold -e "bash -c 'eval $(opam env) ulimit -n 65536; dune exec --build-dir=_build${id} -- ./main.exe -i ${id} -n ${1} -v'"
done

sleep 3.0
dune exec -- ./live_test.exe ${1} -r 100 -t 10
