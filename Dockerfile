FROM fpco/stack-build:lts-18.12
RUN mkdir /opt/build
COPY . /opt/build
WORKDIR /opt/build
RUN stack install --no-docker --system-ghc
RUN stack install --no-docker --system-ghc pandoc
RUN stack install --no-docker --system-ghc pandoc-plot
RUN ./test.sh
