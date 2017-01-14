(function() {
  var ws = new WebSocket("ws://localhost:4000/ws")

  ws.onmessage = function(data) {
    console.log(data)
  };

  ws.onopen = function() {
    ws.send('test');
  };

  var canvas = new fabric.Canvas('canvas');

  var rect = new fabric.Rect({
    left: 100,
    top: 100,
    fill: 'red',
    width: 20,
    height: 20
  });

  canvas.add(rect);
})();
