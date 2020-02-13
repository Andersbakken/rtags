FROM debian:stretch

RUN apt-get update \
        && apt-get install wget git libssl-dev llvm-dev \
          clang libclang-dev cmake emacs libcppunit-dev \
          python3 python3-pip zlib1g-dev pkg-config -y \
        && pip3 install pytest pytest-pudb

COPY scripts/docker-ci.sh  /usr/local/bin
RUN ln -s /usr/local/bin/docker-ci.sh /
WORKDIR /opt/

ENTRYPOINT [ "docker-ci.sh" ]
