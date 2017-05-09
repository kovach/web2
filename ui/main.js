var sock;

var initSock = function() {
  sock = new WebSocket("ws://0.0.0.0:8080");
  sock.onopen = function(event) {
    console.log('open');
    sock.send(resetCommand());
  };
  sock.onmessage = function(event) {
    var d = event.data;
    var obj = JSON.parse(event.data);

    var m = get('main');
    while (m.firstChild) {
      m.removeChild(m.firstChild);
    }
    _.each(obj, parseTuple);

    return;
  }
}

var parseNode = function(node) {
  switch (node.tag) {
    case "NTRef":
      break;
    case "NTInt":
      break;
    case "NTNamed":
      break;
  }
}

var clickCommand = function(id) {
  return JSON.stringify({
    "tag":"Click", "contents": {
      "tag": "NTRef", "contents": id
    }
  });
}
var hoverCommand = function(id) {
  return JSON.stringify({
    "tag":"Hover", "contents": {
      "tag": "NTRef", "contents": id
    }
  });
}
var unhoverCommand = function(id) {
  return JSON.stringify({
    "tag":"UnHover", "contents": {
      "tag": "NTRef", "contents": id
    }
  });
}
var resetCommand = function() {
  return JSON.stringify({
    "tag":"Reset"
  });
}

elements = {'main': get('main')};

// Handles UI interpretation; creates new objects/relations denoted by tuples
var parseTuple = function(t) {
  var nodes = t.t_nodes;
  var label = t.t_label;
  switch (label) {
    case "box":
      var id = nodes[0].contents;
      var obj = nodes[1].contents;
      var elem = mkBox(nodes[2].contents + " " + obj, id);
      elem.setAttribute('ref', obj);
      elements[id] = elem;
      elem.addEventListener('click', function(ev) {
        console.log('click: ', ev.button);
        ev.stopPropagation();
        ev.preventDefault();
        sock.send(clickCommand(id));
      });
      elem.addEventListener('contextmenu', function(ev) {
        console.log('click: ', ev.button);
        ev.stopPropagation();
        ev.preventDefault();
        sock.send(hoverCommand(id));
      });
      //elem.addEventListener('mouseenter', function(ev) {
      //  ev.stopPropagation();
      //  console.log('over');
      //  sock.send(hoverCommand(id));
      //});
      //elem.addEventListener('mouseleave', function(ev) {
      //  ev.stopPropagation();
      //  console.log('out', id);
      //  sock.send(unhoverCommand(id));
      //});
      break;
    case "child":
      var c = elements[nodes[0].contents];
      var p = elements[nodes[1].contents];
      append(c,p);
      break;
    case "color":
      var elem = nodes[0].contents;
      var color = nodes[1].contents;
      elements[nodes[0].contents].style.color = '#'+color;
      break;
    case "clear":
      var elem = elements[nodes[0].contents];
      elem.parentNode.removeChild(elem);
      break;
    case "hide":
      break;
  }

}

window.onload = function() {
  document.addEventListener('contextmenu', event => event.preventDefault());
  initSock();
}

