FROM haskell:8.4 as build

WORKDIR /usr/src/app/

COPY stack.yaml ./
RUN stack setup

COPY package.yaml ./
RUN stack build --only-dependencies

COPY . ./
RUN stack build --copy-bins --local-bin-path /usr/local/bin

FROM debian:9
RUN apt-get update && apt-get install libgmp10
COPY --from=build /usr/local/bin/aiplay-manager /usr/local/bin/
COPY --from=build /usr/local/bin/aiplay-referee-tron /usr/local/bin/
COPY --from=build /usr/local/bin/aiplay-dummy-player /usr/local/bin/
