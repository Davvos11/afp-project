# Usage
## Run locally
```bash
cabal run
```

## Run with docker
```bash
# Build image (re-run this after adding dependencies)
docker build -t davvos11/afp-project .
# Run container with volume mount to latest code changes and database
docker run -it \
    -v ./app:/app/app \
    -v ./database-prod.db:/app/database-prod.db \
    davvos11/afp-project
```

If you run this using `docker-compose`, make sure to add `- tty: true`. For some reason it will not print logs run without a shell.
