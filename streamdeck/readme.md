# stream-deck

I use
[timothycrosley/streamdeck-ui](https://github.com/timothycrosley/streamdeck-ui)
to configure my [Stream Deck](https://www.elgato.com/en/stream-deck).

## Actions

I did not get the _Press Keys_ functionality of
[timothycrosley/streamdeck-ui](https://github.com/timothycrosley/streamdeck-ui)
to work so I use
[xdotool](https://manpages.ubuntu.com/manpages/focal/man1/xdotool.1.html) to
simulate key presses with the _Command_ option.

These are the shortcuts I have: 

## Page 1

At the moment this is the main page I use on stream.

### First line

These `ctrl+alt+N` are shortcuts I've set up in OBS for changing scenes.

**Main scene**

```
/usr/bin/xdotool key --delay 60 ctrl+alt+1
```

**Soon scene**

```
/usr/bin/xdotool key --delay 60 ctrl+alt+2
```

**Break scene**

```
/usr/bin/xdotool key --delay 60 ctrl+alt+3
```

**Private scene**

```
/usr/bin/xdotool key --delay 60 ctrl+alt+4
```

**Done scene**

```
/usr/bin/xdotool key --delay 60 ctrl+alt+5
```

### Second line

Here I have actions to play [sounds](/gifs) on an [overlay](/overlay).

I have [a script to post the messages](/scripts/gif.sh) to the [gifs](/gifs)
backend, which will then send a WebSocket message to the [overlay](/overlay) to
play the sound (or gif).

**My man** (Rick and Morty)

```
/path/to/the/respository/scripts/gif.sh Audio my-man
```

**FBI** (FBI open up!)

```
/path/to/the/respository/scripts/gif.sh Audio fbi
```

**Drums** (ba-dum-tss)

```
/path/to/the/respository/scripts/gif.sh Audio badumtss
```

**Hallelujah**

```
/path/to/the/respository/scripts/gif.sh Audio hallelujah
```

**Sad trombone**

```
/path/to/the/respository/scripts/gif.sh Audio sad-trombone
```

### Third line

**Mic**

This one is another OBS shortcut that toggles the microphone.

```
/usr/bin/xdotool key --delay 60 ctrl+alt+0
```

The rest are actions to play [gifs](/gifs) using the same approach as the sounds
described above.

**RollSafe**

```
/path/to/the/respository/scripts/gif.sh Gif rollsafe
```

**Omaba Mic Drop**

```
/path/to/the/respository/scripts/gif.sh Gif mic
```

**Focus!**

```
/path/to/the/respository/scripts/gif.sh Gif focus
```

**Boom**

```
/path/to/the/respository/scripts/gif.sh Gif boom
```

## Page 2

I'm experimenting with new actions here. Not used on stream yet.

### First line

**Music**

This is a
[xdotool](https://manpages.ubuntu.com/manpages/focal/man1/xdotool.1.html) script
to toggle the music on a [Pretzel Rocks](https://www.pretzel.rocks/) Chrome tab.
But is buggy at the moment.

```
/path/to/the/respository/scripts/music.sh
```

**Page 1**

A
[timothycrosley/streamdeck-ui](https://github.com/timothycrosley/streamdeck-ui)
action to switch to Page 1. It can also be done through the UI but _ain't
nobody got time for that_.

**Zoom Mic**

Another
[xdotool](https://manpages.ubuntu.com/manpages/focal/man1/xdotool.1.html) script
script. This time to toggle a Zoom call Mic.

```
/path/to/the/respository/scripts/zoom.mic.sh
```

**Zoom Cam**

And this one for the camera.

```
/path/to/the/respository/scripts/zoom.cam.sh
```
