FROM fpco/stack-build:lts-18.12
ENV DEBCONF=noninteractive
ENV TZ=Europe/Warsaw
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone
RUN --mount=type=cache,target=/var/cache/apt \
    apt-get update && \
    apt-get install -y texlive-xetex texlive-latex-base tzdata < /dev/null
RUN mkdir /opt/build
COPY . /opt/build
WORKDIR /opt/build
RUN stack install --no-docker --system-ghc
RUN stack install --no-docker --system-ghc pandoc
RUN stack install --no-docker --system-ghc pandoc-plot
RUN ./test.sh
