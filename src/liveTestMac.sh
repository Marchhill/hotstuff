x=$(expr $1 - 1)
dir=$(pwd)
rm _build/.lock

for id in $(seq 0 $x);
do
	osascript -e "tell app \"Terminal\" to do script \"cd ${dir}; dune exec --build-dir=_build${id} -- ./main.exe -i ${id} -n ${1}\""
done

sleep 3.0
dune exec -- ./live_test.exe ${1} -r 100 -t 10 --times "graphs/data/times.csv" --stats "graphs/data/stats.csv"