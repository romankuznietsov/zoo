(function() {
  var ws = new WebSocket("ws://localhost:4000/ws");
  var ctx = document.getElementById('canvas').getContext('2d');
  var width = 800, height = 800;

  ctx.canvas.width = width;
  ctx.canvas.height = height;
  ctx.translate(width / 2, height / 2);

  function clear() {
    ctx.clearRect(- width / 2, - height / 2, width, height);
  };

  function drawPlant(plant) {
    ctx.fillStyle = "rgb(0, 255, 0)";
    ctx.beginPath();
    ctx.arc(plant.position.x, plant.position.y, 2, 0, 2 * Math.PI);
    ctx.fill();
  };

  function drawCreature(creature) {
    ctx.translate(creature.position.x, creature.position.y);
    ctx.rotate(creature.direction);

    ctx.strokeStyle = "rgb(0,0,0)";
    ctx.beginPath();
    ctx.arc(0, 0, 10, 0, 2 * Math.PI);
    ctx.lineTo(0, 0);
    ctx.stroke();

    ctx.rotate(- creature.direction);
    ctx.translate(- creature.position.x, - creature.position.y);
  };

  ws.onmessage = function(event) {
    clear();
    data = JSON.parse(event.data);
    data.plants.map(drawPlant);
    data.creatures.map(drawCreature);
  };
})();
