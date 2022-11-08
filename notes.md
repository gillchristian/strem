## TODO

- [x] Pickup scene mapping from config file
- [x] Move obs-ws code out of Init ?
- [ ] Better name than `gifs` xD
- [ ] Update docs to include new commands & [obs-ws](https://github.com/Palakis/obs-websocket)
- [ ] Rename path: `/gifs` -> `/api`
- [ ] Serve client as well
- [ ] Load obs-ws from some secrets file
- [ ] Make obs-ws optional & configurable
- [ ] Update config to contain `Maybe Connection` and handle disconnect
- [ ] Logging: single line logs for HTTP
- [ ] Logging: parse obs-ws messages `Update JSON | Response JSON | ...`
- [ ] Send gif / audio path from server ?
- [ ] Remove duplication on `scripts/gif.sh`
- [ ] Add CLI app that runs the commands (instead of `scripts/gifs.sh`)
- [ ] Add `-Wall` and clean up warnings

[obs-ws Docs](https://github.com/Palakis/obs-websocket/blob/4.x-current/docs/generated/protocol.md)

## Stream Deck commands

```
/usr/bin/xdotool search --name '^Pretzel Rocks' windowfocus --sync key --delay 60 space
/usr/bin/xdotool search --name '^Pretzel Rocks' windowactivate --sync key --delay 60 space

/usr/bin/xdotool search --name '^Zoom' windowactivate --sync key --delay 60 alt+a
```

[Reference](https://github.com/timothycrosley/streamdeck-ui/issues/47#issuecomment-631695324)

```
/usr/bin/xdotool key --delay 60 ctrl+alt+m
```

```
/usr/bin/xdotool key --delay 60 ctrl+alt+1
/usr/bin/xdotool key --delay 60 ctrl+alt+2
/usr/bin/xdotool key --delay 60 ctrl+alt+3
/usr/bin/xdotool key --delay 60 ctrl+alt+4
/usr/bin/xdotool key --delay 60 ctrl+alt+5
```

```
/usr/bin/xdotool key --delay 60 ctrl+alt+f key --delay 3000 ctrl+alt+f
```

```
/usr/bin/xdotool key --delay 60 ctrl+alt+i
```
