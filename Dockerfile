FROM fpco/stack-build:lts-18.12
RUN mkdir /opt/build
COPY . /opt/build
WORKDIR /opt/build
RUN stack install --system-ghc
RUN stack install --system-ghc pandoc
RUN stack install --system-ghc pandoc-plot
RUN ./test.sh
