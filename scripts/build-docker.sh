#!/usr/bin/env bash
# Build and run the heteroTests Docker image.
set -euo pipefail

docker build -t hetero-tests .
echo "Image built. Launching interactive R session..."
docker run -it hetero-tests R
