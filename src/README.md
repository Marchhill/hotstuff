# HotStuff

## About
Implementation of the [HotStuff consensus algorithm](https://arxiv.org/abs/1803.05069).
## Dependencies
```
# install opam, libev and capnproto system deps
sudo apt-get install opam libev-dev capnproto libcapnp-dev libgmp-dev

# initialise opam
opam init
eval $(opam env)

# setup latest ocaml version (change version number)
opam switch create 4.14.1

# update & upgrade opam
opam update
opam upgrade

# installing dependencies
opam install dune ppx_expect conf-libev lwt capnp-rpc-unix tezos-crypto

# install python dependencies for plotting
pip3 install pandas matplotlib seaborn
```
