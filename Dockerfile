FROM ubuntu:18.04 AS builder

WORKDIR /builder

RUN apt-get update
RUN apt-get install --assume-yes curl
RUN apt-get install --assume-yes xz-utils
RUN apt-get install --assume-yes gcc
RUN apt-get install --assume-yes build-essential make
RUN curl https://gitlab.haskell.org/haskell/ghcup/raw/master/ghcup > ghcup
RUN sh ./ghcup install 8.6.5
RUN sh ./ghcup install-cabal 3.0.0.0
RUN ~/.ghcup/bin/cabal v2-update
RUN apt-get install --assume-yes git
RUN apt-get install --assume-yes libgmp-dev
RUN apt-get install --assume-yes libz-dev

COPY knossos-ksc /builder/knossos-ksc

WORKDIR /builder/knossos-ksc

RUN git checkout 41882f2
RUN ls ~/.ghcup/bin
RUN ~/.ghcup/bin/cabal v2-install --with-compiler=$HOME/.ghcup/bin/ghc-8.6.5 --installdir=.

FROM ubuntu:18.04

WORKDIR /root
COPY --from=builder /builder/knossos-ksc/ksc-rewrite-app .
COPY --from=builder /builder/knossos-ksc/test/ksc/ex0.ks test/ksc/ex0.ks
COPY --from=builder /builder/knossos-ksc/src/runtime/prelude.ks src/runtime/prelude.ks
CMD ./ksc-rewrite-app
