FROM ubuntu:22.04 AS build

ENV PROJECT_NAME "idris2-node-postgres"

RUN apt-get update && apt-get install --yes gcc make chezscheme libgmp3-dev git gnupg && rm -rf /var/lib/apt/lists/*

ENV PATH "/root/.pack/bin:/root/.idris2/bin:$PATH"

WORKDIR /opt/idris2-pack

ADD idris2-pack/ ./
RUN true

ENV SCHEME=chezscheme

ARG db
RUN make micropack SCHEME=chezscheme DB=${db}

WORKDIR /opt/$PROJECT_NAME

COPY ./README.md .
ADD src/ ./src
COPY ./$PROJECT_NAME.ipkg .
COPY ./package.json .
COPY ./package-lock.json .

RUN /root/.pack/bin/pack --cg node build ./$PROJECT_NAME.ipkg

FROM node:16

ENV PROJECT_NAME "idris2-node-postgres"

WORKDIR /opt/$PROJECT_NAME

COPY --from=build /opt/$PROJECT_NAME/package.* /opt/$PROJECT_NAME
COPY --from=build /opt/$PROJECT_NAME/build/exec/$PROJECT_NAME /opt/$PROJECT_NAME

RUN npm install
