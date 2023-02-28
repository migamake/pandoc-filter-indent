FROM fpco/stack-build:lts-18.12
ENV DEBCONF=noninteractive
ENV TZ=Europe/Warsaw
RUN --mount=type=cache,target=/var/cache/apt \
    apt-get update && \
    apt-get install -y texlive-xetex texlive-latex-base
RUN mkdir /opt/build
COPY . /opt/build
WORKDIR /opt/build
RUN stack install --no-docker --system-ghc
RUN stack install --no-docker --system-ghc pandoc
RUN stack install --no-docker --system-ghc pandoc-plot
RUN ./test.sh
