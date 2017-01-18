(function() {
  var ws = new WebSocket("ws://localhost:4000/ws")
  var creatures = {}, plants = {};
  var canvasWidth = 500, canvasHeight = 500;

  function colorFromEnergy(energy) {
    if (energy > 2000) {
      return 'rgb(255,0,0)'
    } else {
      intensity = 255 - Math.floor(energy / 2000 * 255);
      return 'rgb(255,' + intensity + ',' + intensity + ')'
    }
  };

  function buildCreature() {
    var circle = new fabric.Circle({
      fill: 'white',
      stroke: 'black',
      radius: 3,
      originX: 'center',
      originY: 'center'
    });
    var line = new fabric.Line(
      [0, 0, 3, 0],
      {stroke: 'black', width: 1}
    );
    return new fabric.Group([circle, line], {top: 0, left: 0});
  }

  function buildPlant() {
    return new fabric.Circle({
      fill: 'green',
      radius: 2,
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
    var color = colorFromEnergy(creature.energy);
    creatures[creature.id].getObjects()[0].setFill(color);
  };

  function updatePlant(plant) {
    if (plants[plant.id] === undefined) {
      plants[plant.id] = buildPlant();
      canvas.add(plants[plant.id]);
    };
    plants[plant.id].setTop(plant.position.y);
    plants[plant.id].setLeft(plant.position.x);
  };

  function clearPlants(dataPlants) {
    var remainingPlants = dataPlants.map(function(p) { return p.id });
    Object.keys(plants).map(function(plantId) {
      if (remainingPlants.indexOf(plantId) === -1) {
        canvas.remove(plants[plantId]);
        delete(plants[plantId]);
      }
    });
  };

  function clearCreatures(dataCreatures) {
    var remainingCreatures = dataCreatures.map(function(c) { return c.id });
    Object.keys(creatures).map(function(creatureId) {
      if (remainingCreatures.indexOf(creatureId) === -1) {
        canvas.remove(creatures[creatureId]);
        delete(creatures[creatureId]);
      }
    });
  };

  ws.onmessage = function(event) {
    data = JSON.parse(event.data);
    clearPlants(data.plants);
    clearCreatures(data.creatures);
    data.plants.map(updatePlant);
    data.creatures.map(updateCreature);
    canvas.renderAll();
  };

  var canvas = new fabric.Canvas('canvas');
  canvas.setDimensions({width: canvasWidth, height: canvasHeight});
  canvas.absolutePan({x: -canvasWidth / 2, y: -canvasHeight / 2});
})();
