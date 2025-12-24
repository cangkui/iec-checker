#!/bin/bash

# build docker image for compiling first
# docker build -t iec:4.08 .

# run for only project build/compile
# docker run --rm \
#     -v "$(pwd)":/home/opam/src \
#     -u opam \
#     -w /home/opam/src \
#     iec:4.08 \
#     /bin/bash -c 'eval $(opam env) && make build'

# create and run for server running.
# docker create -it \
#     -v "$(pwd)":/home/opam/src \
#     -u opam \
#     --name iec-serve \
#     iec:4.08
# docker start iec-serve