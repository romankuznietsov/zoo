(function() {
  var ws = new WebSocket("ws://localhost:4000/ws")
  var creatures = {}, plants = {};
  var canvasWidth = 500, canvasHeight = 500;

  function buildCreature() {
    var circle = new fabric.Circle({
      fill: 'white',
      stroke: 'black',
      radius: 10,
      originX: 'center',
      originY: 'center'
    });
    var line = new fabric.Line(
      [0, 0, 10, 0],
      {stroke: 'black', width: 1}
    );
    return new fabric.Group([circle, line], {top: 0, left: 0});
  }

  function buildPlant() {
    return new fabric.Circle({
      fill: 'green',
      radius: 5,
      originX: 'center',
      originY: 'center'
    });
  }

  function updateCreature(creature) {
    if (creatures[creature.id] === undefined) {
      creatures[creature.id] = buildCreature();
      canvas.add(creatures[creature.id]);
    };
    creatures[creature.id].setTop(creature.position.y);
    creatures[creature.id].setLeft(creature.position.x);
    creatures[creature.id].setAngle(creature.direction / (2 * Math.PI) * 360);
  };

  function updatePlant(plant) {
    if (plants[plant.id] === undefined) {
      plants[plant.id] = buildPlant();
      canvas.add(plants[plant.id]);
    };
    plants[plant.id].setTop(plant.position.y);
    plants[plant.id].setLeft(plant.position.x);
  };

  ws.onmessage = function(event) {
    data = JSON.parse(event.data);
    data.plants.map(updatePlant);
    data.creatures.map(updateCreature);
    canvas.renderAll();
  };

  var canvas = new fabric.Canvas('canvas');
  canvas.setDimensions({width: canvasWidth, height: canvasHeight});
  canvas.absolutePan({x: -canvasWidth / 2, y: -canvasHeight / 2});
})();
