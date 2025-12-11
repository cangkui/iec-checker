# Dockerfile.builder

# Opam Mirrors Based on OCaml 4.08.1
FROM ocaml/opam:ubuntu-22.04-ocaml-4.08

WORKDIR /home/opam/src

COPY . /home/opam/src

# Switch to root to modify apt sources to install python
USER root

# Configure Tsinghua apt mirror source and install python3
RUN sed -i 's/archive.ubuntu.com/mirrors.tuna.tsinghua.edu.cn/g' /etc/apt/sources.list && \
    apt-get update && \
    apt-get install -y python3 && \
    rm -rf /var/lib/apt/lists/*

USER opam

# Install project dependencies (dune-project defines dependencies)
# Note: opam install . By default installs both dependencies and the project itself.
RUN opam install . --deps-only -y && \
    opam clean

CMD ["python3", "server.py"]