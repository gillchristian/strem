import { pipe, flow } from "fp-ts/function";
import { sequenceS } from "fp-ts/Apply";
import * as A from "fp-ts/Array";
import * as E from "fp-ts/Either";
import * as O from "fp-ts/Option";
import * as Json from "fp-ts/Json";
import * as t from "io-ts";

// TODO: use IO
const getMain = () =>
  pipe(
    O.fromNullable(document.getElementById("main")),
    E.fromOption(() => new Error("Missing #main"))
  );

type Media = { tag: "Gif"; gif: Gif } | { tag: "Audio"; audio: Audio };

interface Audio {
  name: string;
  url: string;
}

interface Gif {
  name: string;
  url: string;
  duration: number;
}

const gifs: Gif[] = [
  { name: "boom", url: "/assets/gif/boom.gif", duration: 3000 },
  { name: "focus", url: "/assets/gif/focus.gif", duration: 4500 },
  { name: "mic", url: "/assets/gif/mic.gif", duration: 3250 },
  { name: "rollsafe", url: "/assets/gif/rollsafe.gif", duration: 4000 },
];

const audios: Audio[] = [
  { name: "badumtss", url: "/assets/audio/badumtss.mp3" },
  { name: "fbi", url: "/assets/audio/fbi-open-up.mp3" },
  { name: "hallelujah", url: "/assets/audio/hallelujah.mp3" },
  { name: "my-man", url: "/assets/audio/my-man.mp3" },
  { name: "sad-trombone", url: "/assets/audio/sad-trombone.mp3" },
];

const findMedia = (msg: Message): O.Option<Media> =>
  msg.tag === "Gif"
    ? pipe(
        gifs,
        A.findFirst((gif) => gif.name === msg.gif),
        O.map((gif) => ({ tag: "Gif", gif }))
      )
    : pipe(
        audios,
        A.findFirst((audio) => audio.name === msg.audio),
        O.map((audio) => ({ tag: "Audio", audio }))
      );

const Message = t.union([
  t.interface({ tag: t.literal("Gif"), gif: t.string }),
  t.interface({ tag: t.literal("Audio"), audio: t.string }),
]);
type Message = t.TypeOf<typeof Message>;

let timer: NodeJS.Timeout | undefined;

const showGif = ({ main, gif }: { main: HTMLElement; gif: Gif }) => {
  main.innerHTML = `<img src="${gif.url}">`;

  if (typeof timer !== "undefined") {
    clearTimeout(timer);
  }

  timer = setTimeout(() => {
    main.innerHTML = "";
    timer = undefined;
  }, gif.duration);
};

const playAudio = (audio: Audio) => {
  const audioEl = new Audio(audio.url);

  audioEl.play().catch((error) => {
    console.error(error);
    console.error(
      new Error(`Could not play audio: ${audio.name} (${audio.url})`)
    );
  });
};

const playMedia = ({ media, main }: { main: HTMLElement; media: Media }) => {
  if (media.tag === "Gif") {
    showGif({ gif: media.gif, main });
  }

  if (media.tag === "Audio") {
    playAudio(media.audio);
  }
};

const onMessage = ({ data }: MessageEvent) => {
  pipe(
    data,
    flow(
      Json.parse,
      E.mapLeft(() => new Error("Parsing JSON failed"))
    ),
    E.chain(
      flow(
        Message.decode,
        E.mapLeft(() => new Error("Decoding failed"))
      )
    ),
    E.chain((media) =>
      pipe(
        media,
        findMedia,
        E.fromOption(
          () =>
            new Error(
              `Media not found ${media.tag}/${
                media.tag === "Gif" ? media.gif : media.audio
              }`
            )
        )
      )
    ),
    (media) => sequenceS(E.Apply)({ media, main: getMain() }),
    E.fold(console.error, playMedia)
  );
};

const app = () => {
  const con = new WebSocket("ws://localhost:9000/gifs/ws");

  con.onmessage = onMessage;
};

app();
