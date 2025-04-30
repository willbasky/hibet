FROM buildpack-deps:oracular

RUN apt-get update && apt-get install -y --no-install-recommends \
    gnupg \
    ca-certificates \
    dirmngr \
    curl \
    git \
    software-properties-common \
    apt-utils \
    nodejs \
    && rm -rf /var/lib/apt/lists/*

RUN mkdir -p /root/.ghcup/bin \
    && curl -sL https://downloads.haskell.org/ghcup/0.1.50.1/x86_64-linux-ghcup-0.1.50.1 > /root/.ghcup/bin/ghcup \
    && chmod +x /root/.ghcup/bin/ghcup

RUN /root/.ghcup/bin/ghcup install cabal 3.14.2.0 \
    && /root/.ghcup/bin/cabal-3.14.2.0 --version

RUN /root/.ghcup/bin/ghcup install ghc 9.6.7 \
    && /root/.ghcup/bin/ghcup set ghc 9.6.7 \
    && /root/.ghcup/bin/ghc-9.6.7 --version

RUN mkdir -p /root/.cabal/bin \
    && curl -sL https://github.com/haskell-hvr/cabal-plan/releases/download/v0.7.3.0/cabal-plan-0.7.3.0-x86_64-linux.xz > cabal-plan.xz \
    && echo 'f62ccb2971567a5f638f2005ad3173dba14693a45154c1508645c52289714cb2  cabal-plan.xz' | sha256sum -c - \
    && xz -d < cabal-plan.xz > /root/.cabal/bin/cabal-plan \
    && rm -f cabal-plan.xz \
    && chmod +x /root/.cabal/bin/cabal-plan \
    && /root/.cabal/bin/cabal-plan --version

ENV PATH=/root/.cabal/bin:/root/.ghcup/bin:$PATH

ENV CABAL_DIR=/root/.cabal
ENV CABAL_CONFIG=/root/.cabal/config
ENV LANG=C.UTF-8

RUN mkdir -p /root/.cabal \
    && echo "remote-build-reporting: anonymous" >> /root/.cabal/config \
    && echo "write-ghc-environment-files: never" >> /root/.cabal/config \
    && echo "remote-repo-cache: /root/.cabal/packages" >> /root/.cabal/config \
    && echo "logs-dir: /root/.cabal/logs" >> /root/.cabal/config \
    && echo "world-file: /root/.cabal/world" >> /root/.cabal/config \
    && echo "extra-prog-path: /root/.cabal/bin" >> /root/.cabal/config \
    && echo "symlink-bindir: /root/.cabal/bin" >> /root/.cabal/config \
    && echo "installdir: /root/.cabal/bin" >> /root/.cabal/config \
    && echo "build-summary: /root/.cabal/logs/build.log" >> /root/.cabal/config \
    && echo "store-dir: /root/.cabal/store" >> /root/.cabal/config \
    && echo "install-dirs user" >> /root/.cabal/config \
    && echo "  prefix: /root/.cabal" >> /root/.cabal/config \
    && echo "repository hackage.haskell.org" >> /root/.cabal/config \
    && echo "  url: http://hackage.haskell.org/" >> /root/.cabal/config \
    && echo "program-default-options" >> /root/.cabal/config \
    && echo "  ghc-options: +RTS -M3G -RTS" >> /root/.cabal/config \
    && cat /root/.cabal/config

RUN cabal v2-update -v

WORKDIR /workspace

CMD ["tail", "-f", "/dev/null"]