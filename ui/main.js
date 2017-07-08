var sock;

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
var buttonVal = function(i) {
  return {0:sym("left"), 2:sym("right")}[i] || sym("other");
}

var clickCommand = function(id, button) {
  return JSON.stringify({
    "tag": "Click",
    "ref": { "tag": "NTRef", "contents": id },
    // TODO check
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

var objects = {};

var hasKeys = function(ks, obj) {
  return _.every(ks, function(k) { return k in obj; });
}

var defined = function(obj) {
  return hasKeys(["elem", "x", "y", "color"], obj);
}

var get_svg = function() {
  return get("svg-main");
}

var set = function(id, k, v) {
  if (!objects[id]) {
    objects[id] = {};
  }

  var obj = objects[id];
  obj[k] = v;

  var e = obj.elem;
  if (defined(obj)) {
    // TODO: ? use w = get_svg().width.baseVal;
    var w = 600;
    var off = 10*radius;
    var incr = (w-2*off) / (bdim - 1);
    e.setAttribute("cx", (obj["x"]-1)*incr + off);
    e.setAttribute("cy", (obj["y"]-1)*incr + off);
    e.setAttribute("fill", obj["color"]);
    e.setAttribute("r", obj["size"]);
    e.setAttribute("visibility", "visible");
  }
}
var parseTuple = function(t) {

  var sign = t[0] === "Positive" ? true : false;
  var label = t[1];
  var nodes = t[2];

  switch (label) {
    case "elem":
      var id = nodes[0].contents;
      if (sign) {
        //console.log("make elem");
        var elem = mkToken(id, sock);
        set(id, "elem", elem);
        set(id, "size", 0);
      } else {
        //console.log("remove elem");
        var elem = objects[id].elem;
        elem.parentNode.removeChild(elem);
      }
      break;
    case "x-rank":
      var id = nodes[0].contents;
      var n = nodes[1].contents;
      set(id, "x", n);
      break;
    case "y-rank":
      var id = nodes[0].contents;
      var n = nodes[1].contents;
      set(id, "y", n);
      break;
    case "line-to":
      var id1 = nodes[0].contents;
      var id2 = nodes[1].contents;
      break;
    case "color":
      var id = nodes[0].contents;
      var color = nodes[1].contents;
      set(id, "color", color === "black" ? "#444" : color === "empty" ? "#eb6" : "#ddd");
      break;
    case "size":
      var id = nodes[0].contents;
      var r = nodes[1].contents;
      set(id, "size", r);
      break;
  }
}

window.onload = function() {
  document.addEventListener('contextmenu', event => event.preventDefault());
  initSock();
}

