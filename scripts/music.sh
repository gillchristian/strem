#!/bin/sh

CURRENT=$(xdotool getwindowfocus)
PRETZEL=$(xdotool search --limit 1 --name "^Pretzel")
xdotool windowactivate --sync ${PRETZEL}
xdotool key --clearmodifiers "space"
xdotool windowactivate --sync ${CURRENT}
