FROM docker.io/eclipse-temurin:21

RUN apt-get update && apt-get install -y --no-install-recommends \
    bash \
    curl \
    rlwrap \
    && rm -rf /var/lib/apt/lists/*

RUN curl -L -O https://github.com/clojure/brew-install/releases/latest/download/linux-install.sh \
    && bash ./linux-install.sh \
    && rm linux-install.sh

ENV PATH="/app/bin:$PATH"
ENV LIMABEAN_UBERJAR=/app/limabean-standalone.jar
ENV LIMABEAN_BEANFILE=main.beancount

VOLUME /data

ENTRYPOINT ["limabean"]
RUN mkdir -p /app/bin
WORKDIR /app

ARG VERSION
COPY clj/target/limabean-${VERSION}-standalone.jar limabean-standalone.jar
COPY rust/target/release/limabean rust/target/release/limabean-pod ./bin

WORKDIR /data
