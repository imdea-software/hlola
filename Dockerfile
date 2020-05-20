FROM haskell:8
RUN mkdir /HLola
COPY package.yaml /HLola
COPY stack.yaml /HLola
COPY empirical /HLola
COPY app /HLola/app
COPY src /HLola/src
WORKDIR /HLola
RUN stack setup
RUN stack install
ENTRYPOINT ["HLola"]
