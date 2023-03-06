pids=()

x=$(expr $1 - 1)
for id in $(seq 0 $x);
do
	dune exec -- ./main.exe -i ${id} -n $1 &
	pids+=($!)
	sleep 0.5
done

read -p ""
for pid in "${pids[@]}"
do
	kill -TERM "$pid" 2>/dev/null
done