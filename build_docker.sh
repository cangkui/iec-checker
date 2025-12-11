#!/bin/bash

# docker build -t iec:4.08 .

# docker run --rm \
#     -v "$(pwd)":/home/opam/src \
#     -u opam \
#     iec:4.08 \
#     make build

# docker create -it \
#     -v "$(pwd)":/home/opam/src \
#     -u opam \
#     iec:4.08