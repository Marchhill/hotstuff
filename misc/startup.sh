ulimit -n 65536; eval $(opam env) dune exec --build-dir=_build0 -- ./main.exe -i 0 -n 4 -b 99999999
ulimit -n 65536; eval $(opam env) dune exec --build-dir=_build1 -- ./main.exe -i 1 -n 4 -b 99999999
ulimit -n 65536; eval $(opam env) dune exec --build-dir=_build2 -- ./main.exe -i 2 -n 4 -b 99999999
ulimit -n 65536; eval $(opam env) dune exec --build-dir=_build3 -- ./main.exe -i 3 -n 4 -b 99999999
eval $(opam env) dune exec -- ./live_test.exe 4 -c -t 10 -r 200 -b ∞ --times "./experiments/data/mininet/subtests/200tpub.csv" --stats "./experiments/data/mininet/stats.csv"