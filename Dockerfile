FROM haskell:9.4.8

WORKDIR /app

RUN cabal update

COPY . /app/

RUN cabal install

CMD [ "cabal", "run" ]
