/* GLOBAL STATE */
var objects = {};
var sock;
var max_rank = {x: 0, y: 0};

/* PARAMETERS */
var radius = 12;
var boardMargin = 8;
var bdim = 9;

var initSock = function() {
  sock = new WebSocket("ws://0.0.0.0:8080");
  sock.onopen = function(event) {
    console.log('open');
    sock.send(resetCommand());
  };
  sock.onmessage = function(event) {
    var msgs = JSON.parse(event.data);

    var m = get('main');
    while (m.firstChild) {
      m.removeChild(m.firstChild);
    }

    //_.each(msgs, function(obj) {
    //  console.log("hey: ", obj);
    //});

    console.log("msgs received: ", msgs.length);

    //if (msgs.length < 10) {
    //  _.each(msgs, function(obj) {
    //    console.log(obj);
    //  });
    //}

    _.each(msgs, parseTuple);

    return;
  }
}

var sym = function(str) {
  return {
    "tag": "NTNamed",
    "contents": str
  };
}
var buttonVal = function(code) {
  switch (code) {
    case 0:
      return sym("left");
    case 2:
      return sym("right");
    default:
      return sym("other");
  }
}

var clickCommand = function(id, button) {
  return JSON.stringify({
    "tag": "Click",
    "ref": id,
    "button": buttonVal(button)
  });
}

var overCommand = function(id) {
  return JSON.stringify({
    "tag": "Hover",
    "ref": { "tag": "NTRef", "contents": id }
  });
}

var outCommand = function(id) {
  return JSON.stringify({
    "tag": "UnHover",
    "ref": { "tag": "NTRef", "contents": id }
  });
}

var resetCommand = function() {
  return JSON.stringify({
    "tag":"Reset"
  });
}

var hasKeys = function(ks, obj) {
  return _.every(ks, function(k) { return k in obj; });
}

var defined = function(obj) {
  return hasKeys(["elem", "x", "y", "color"], obj);
}

var get_svg = function() {
  return get("svg-main");
}

var setObjAttributes = function(obj) {
  if (defined(obj)) {
    var e = obj.elem;
    // TODO: ? use w = get_svg().width.baseVal;
    var w = 600;
    var margin = 3*2*radius;
    var xmax = max_rank["x"];
    var ymax = max_rank["y"];
    var xincr = xmax > 1 ? (w-2*margin) / (xmax - 1) : 0;
    var yincr = ymax > 1 ? (w-2*margin) / (ymax - 1) : 0 ;
    e.setAttribute("cx", (obj["x"]-1)*xincr + margin);
    e.setAttribute("cy", (obj["y"]-1)*yincr + margin);
    e.setAttribute("fill", obj["color"]);
    e.setAttribute("r", obj["size"]);
    e.setAttribute("visibility", "visible");
  }
}

var set = function(id, k, v) {
  id = JSON.stringify(id);
  if (!objects[id]) {
    objects[id] = {};
  }

  var obj = objects[id];
  obj[k] = v;
  setObjAttributes(obj);
}

var update_rank = function(r, v) {
  if (v > max_rank[r]) {
    max_rank[r] = v;
    _.each(objects, function(obj, id) {
      if (defined(obj)) {
        setObjAttributes(obj);
      }
    });
  }
}

var parseTuple = function(t) {

  var sign = t[0] === "Positive" ? true : false;
  var label = t[1];
  var nodes = t[2];

  switch (label) {
    case "elem":
      var id = nodes[0];
      if (sign) {
        var elem = mkToken(id, sock);
        set(id, "elem", elem);
        set(id, "size", 0);
      } else {
        var elem = objects[JSON.stringify(id)].elem;
        elem.parentNode.removeChild(elem);
      }
      break;
    case "x-rank":
      var id = nodes[0];
      var n = nodes[1].contents;
      update_rank("x", n);
      set(id, "x", n);
      break;
    case "y-rank":
      var id = nodes[0];
      var n = nodes[1].contents;
      update_rank("y", n);
      set(id, "y", n);
      break;
    case "line-to":
      var id1 = nodes[0].contents;
      var id2 = nodes[1].contents;
      var elem = mkLine();
      // TODO set endpoints
      break;
    case "color":
      var id = nodes[0];
      var color = nodes[1].contents;
      set(id, "color", color === "black" ? "#444" : color === "empty" ? "transparent" : "#ddd");
      break;
    case "size":
      var id = nodes[0];
      var r = nodes[1].contents;
      set(id, "size", r);
      break;
  }
}

window.onload = function() {
  document.addEventListener('contextmenu', event => event.preventDefault());
  initSock();
}

