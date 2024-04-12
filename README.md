# Usage
## Run locally
```bash
## Server:
cabal run afp-project
# Alternatively, to not re-generate the stop and bus id's
cabal run afp-project -- skip-generate

## Subscriber:
# Once
pip install zmq
pip install asyncio
pip install websockets
# First:
python src/zmq_to_ws_relay.py
# Second, in a separate terminal:
cabal run afp-subscriber
```

When the server is ready, open `fronted/index.html` in your browser.

## Run with docker
```bash
# Build image (re-run this after adding dependencies)
docker build -t davvos11/afp-project .
# Run container as is (production)
docker run -it \
    -v ./database-prod.db:/app/database-prod.db \
    davvos11/afp-project \
    afp-project skip-generate
# Or, run container with volume mount to latest code changes and database
docker run -it \
    -v ./app:/app/app \
    -v ./database-prod.db:/app/database-prod.db \
    davvos11/afp-project \
    cabal run
```

If you run this using `docker-compose`, make sure to add `- tty: true`. For some reason it will not print logs without a shell.
