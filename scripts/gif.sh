#!/bin/bash

if [ "$1" = "Gif" ]; then
  curl localhost:9000/gifs/gif -s \
    -H "Content-Type: application/json" \
    -X POST -d "{ \"tag\": \"Gif\", \"gif\": \"$2\" }" > /dev/null
fi

if [ "$1" = "Audio" ]; then
  curl localhost:9000/gifs/gif -s \
    -H "Content-Type: application/json" \
    -X POST -d "{ \"tag\": \"Audio\", \"audio\": \"$2\" }" > /dev/null
fi

if [ "$1" = "Scene" ]; then
  curl localhost:9000/gifs/gif -s \
    -H "Content-Type: application/json" \
    -X POST -d "{ \"tag\": \"Scene\", \"scene\": \"$2\" }" > /dev/null
fi
