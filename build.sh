#!/bin/bash

IMAGE="iec:4.08"

if docker image inspect "$IMAGE" > /dev/null 2>&1; then
    docker run --rm \
        -v "$(pwd)":/home/opam/src \
        -u opam \
        -w /home/opam/src \
        $IMAGE \
        /bin/bash -c 'eval $(opam env) && make build'
else
    echo "image $IMAGE does not exist, please build it first"
    exit 1
fi
