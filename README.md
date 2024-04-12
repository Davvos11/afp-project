# Usage
## Run locally
```bash
cabal run
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
Optionally, replace `cabal run` with `cabal run exes -- skip-generate` to skip regenerating bus and line numbers.

If you run this using `docker-compose`, make sure to add `- tty: true`. For some reason it will not print logs without a shell.
