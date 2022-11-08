const ws = new WebSocket("ws://localhost:4444");

ws.onmessage = ({ data }) => {
  console.log(data);
};

ws.onclose = (event) => {
  console.log("On close");
  console.error(event);
};

ws.onerror = (event) => {
  console.log("On error");
  console.error(event);
};

ws.onopen = () =>
  ws.send(
    JSON.stringify({
      "request-type": "GetAuthRequired",
      "message-id": "GetAuthRequired",
    })
  );

const response = {
  authRequired: true,
  challenge: "...",
  "message-id": "...",
  salt: "...",
  status: "ok",
};
