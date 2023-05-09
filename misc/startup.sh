ulimit -n 65536; eval $(opam env) dune exec --build-dir=_build0 -- ./bin/run_node.exe -i 0 -n 4 -b 300
ulimit -n 65536; eval $(opam env) dune exec --build-dir=_build1 -- ./bin/run_node.exe -i 1 -n 4 -b 300
ulimit -n 65536; eval $(opam env) dune exec --build-dir=_build2 -- ./bin/run_node.exe -i 2 -n 4 -b 300
ulimit -n 65536; eval $(opam env) dune exec --build-dir=_build3 -- ./bin/run_node.exe -i 3 -n 4 -b 300
eval $(opam env) dune exec -- ./bin/load_gen.exe 4 -t 10 -b 300 --stats "./experiments/data/mininet3/stats.csv" --times "./experiments/data/mininet3/subtests/t1r300.csv" -r 300