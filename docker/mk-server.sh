#!/bin/sh -eu

cp ~/edgeNode/.stack-work/docker/_home/.local/bin/app server/app
docker-compose build server