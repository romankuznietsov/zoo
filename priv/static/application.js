ws = new WebSocket("ws://localhost:4000/ws")

ws.onmessage = function(data) {
  console.log(data)
}

ws.onopen = function() {
  ws.send('test');
}
