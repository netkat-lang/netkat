# docker run -it --rm -v $(pwd):/netkat ubuntu:24.04 bash

# docker build -t netkat-server .
# docker run -it --rm -p 8080:8080 netkat-server

FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive
ENV TZ=Etc/UTC

# System dependencies
RUN apt update && \
    apt install -y \
      autoconf \
      build-essential \
      curl \
      git \
      m4 \
      opam \
      libgmp-dev \
      pkg-config && \
    rm -rf /var/lib/apt/lists/*

# Initialize opam
RUN opam init --disable-sandboxing -y

# Use bash for subsequent RUN commands so "eval $(opam env)" works
SHELL ["/bin/bash", "-c"]

# Create OCaml switch
RUN opam switch create 5.3.0

# Install OCaml dependencies
RUN eval $(opam env --switch=5.3.0) && \
    opam install -y dune && \
    opam install -y \
      sedlex \
      landmarks-ppx \
      menhir \
      yojson \
      alcotest \
      core \
      z3 \
      async && \
    opam install -y --assume-depexts ego

# Bring in local git working tree
COPY . /netkat

WORKDIR /netkat

# Build
RUN eval $(opam env --switch=5.3.0) && \
    make

# Expose server port
EXPOSE 8080

# Start server
CMD ["bash", "-c", "eval $(opam env --switch=5.3.0) && dune exec netkat-listen"]
