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
  if [ "$2" = "main" ]; then
    curl localhost:9000/gifs/gif -s \
      -H "Content-Type: application/json" \
      -X POST -d "{ \"tag\": \"Scene\", \"scene\": \"Screen: Main\" }" > /dev/null
  fi

  if [ "$2" = "soon" ]; then
    curl localhost:9000/gifs/gif -s \
      -H "Content-Type: application/json" \
      -X POST -d "{ \"tag\": \"Scene\", \"scene\": \"Screen: Soon\" }" > /dev/null
  fi

  if [ "$2" = "break-screen" ]; then
    curl localhost:9000/gifs/gif -s \
      -H "Content-Type: application/json" \
      -X POST -d "{ \"tag\": \"Scene\", \"scene\": \"Screen: Break\" }" > /dev/null
  fi

  if [ "$2" = "private" ]; then
    curl localhost:9000/gifs/gif -s \
      -H "Content-Type: application/json" \
      -X POST -d "{ \"tag\": \"Scene\", \"scene\": \"Screen: Private\" }" > /dev/null
  fi

  if [ "$2" = "done_screen" ]; then
    curl localhost:9000/gifs/gif -s \
      -H "Content-Type: application/json" \
      -X POST -d "{ \"tag\": \"Scene\", \"scene\": \"Screen: Done\" }" > /dev/null
  fi

  if [ "$2" = "host-guest-call" ]; then
    curl localhost:9000/gifs/gif -s \
      -H "Content-Type: application/json" \
      -X POST -d "{ \"tag\": \"Scene\", \"scene\": \"Screen: Host & Guest\" }" > /dev/null
  fi

  if [ "$2" = "pairing-host" ]; then
    curl localhost:9000/gifs/gif -s \
      -H "Content-Type: application/json" \
      -X POST -d "{ \"tag\": \"Scene\", \"scene\": \"Screen: Pair programming (host screen)\" }" > /dev/null
  fi

  if [ "$2" = "pairing-guest" ]; then
    curl localhost:9000/gifs/gif -s \
      -H "Content-Type: application/json" \
      -X POST -d "{ \"tag\": \"Scene\", \"scene\": \"Screen: Pair programming (guest screen)\" }" > /dev/null
  fi
fi
