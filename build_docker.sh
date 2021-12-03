#!/bin/bash
docker rm -f breathtestshiny
# docker build --no-cache --tag dmenne/breathtestshiny .
docker build --tag dmenne/breathtestshiny .
docker run -d -it  --name breathtestshiny  -p 3900:3838 dmenne/breathtestshiny
