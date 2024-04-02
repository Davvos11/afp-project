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
    -v ./database-prod:/app/database-prod \
    davvos11/afp-project
```
