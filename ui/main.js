/* GLOBAL STATE */
var objects = {};
var max_rank = {x: 0, y: 0};

/* PARAMETERS */
var radius = 12;
var boardMargin = 8;
var bdim = 9;

var initSock = function() {
  var sock = new WebSocket("ws://0.0.0.0:8080");
  sock.onopen = function(event) {
    console.log('open');
    resetCommand();
  };
  sock.onmessage = function(event) {
    var msgs = JSON.parse(event.data);

    //_.each(msgs, function(obj) {
    //  console.log("msg: ", obj);
    //});

    console.log("msgs received: ", msgs.length);

    //if (msgs.length < 30) {
    //  _.each(msgs, function(obj) {
    //    console.log(JSON.stringify(obj));
    //  });
    //}

    _.each(msgs, parseTuple(sock));

    return;
  }

  return sock;
}

var symNode = function(str) {
  return {
    "tag": "NSymbol",
    "contents": str
  };
}
var strNode = function(str) {
  return {
    "tag": "NString",
    "contents": str,
  };
}
var intNode = function(n) {
  return {
    "tag": "NInt",
    "contents": n,
  };
}

var parseInput = function(str) {
  if (str.length == 0)
    return strNode("")
  if (str[0] == "'")
    return symNode(str.slice(1));
  var n = parseInt(str);
  if (!isNaN(n))
    return intNode(n);
  return strNode(str);
}

var showNode = function(node) {
  switch(node.tag) {
  }
}

var buttonVal = function(code) {
  switch (code) {
    case 0:
      return symNode("left");
    case 2:
      return symNode("right");
    default:
      return symNode("other");
  }
}

var shiftVal = function(code) {
  return code ? symNode("shift") : symNode("no-shift");
}

var mkTuple = function(label, nodes) {
  return JSON.stringify({
    "tag": "RawTuple",
    "rawLabel": {"tag": "L", "contents": label},
    "rawNodes": nodes,
  });
}

var clickCommand = function(id, tid, button, shiftDown) {
  sock.send(mkTuple("raw-click", [ buttonVal(button), shiftVal(shiftDown), id, tid, ]));
}

// TODO remove
var keyCommand = function(id, sock, label) {
  sock.send(mkTuple("key-press", [strNode(label), id]));
}

var keyCommand2 = function(sock, label) {
  sock.send(mkTuple("key-press", [strNode(label)]));
}

var textEntryCommand = function(id, sock, label, str) {
  sock.send(mkTuple("text-entry", [strNode(label), id, strNode(str)]));
}

var stringCommand = function(id, sock, str) {
  sock.send(mkTuple("string-contents", [id, strNode(str)]));
}

//TODO make Fact
//var overCommand = function(id) {
//}
//var outCommand = function(id) {
//}

var resetCommand = function() {
  var msg = JSON.stringify({
    "tag":"Reset"
  });
  sock.send(msg);
}

var connectionCommand = function() {
  var msg = JSON.stringify({
    "tag":"Connect"
  });
  sock.send(msg);
}

var hasKeys = function(ks, obj) {
  return _.every(ks, function(k) { return k in obj; });
}

var objTypes = {
  "token":["elem", "x-rank", "y-rank", "color", "size"],
  "js/edit":["elem", "parent"],
  "js/text":["elem"],
  "thing":["elem"]
}

var defined = function(obj) {
  if ("type" in obj) {
    if (obj.type in objTypes) {
      if (hasKeys(objTypes[obj.type], obj)) {
        return obj.type
      }
    } else {
      console.log("ERROR: undefined type: ", obj.type);
    }
  }
}

var get_svg = function() {
  return get("svg-main");
}

// TODO get rid of this
//      use mkAttr
var setObjAttributes = function(obj) {
  switch (defined(obj)) {
    case "token":
      var e = obj.elem;
      // TODO: ? use w = get_svg().width.baseVal;
      var w = 600;
      var margin = 3*2*radius;
      var xmax = max_rank["x"];
      var ymax = max_rank["y"];
      var xincr = xmax > 1 ? (w-2*margin) / (xmax - 1) : 0;
      var yincr = ymax > 1 ? (w-2*margin) / (ymax - 1) : 0 ;
      e.setAttribute("cx", (obj["x-rank"]-1)*xincr + margin);
      e.setAttribute("cy", (obj["y-rank"]-1)*yincr + margin);
      e.setAttribute("fill", obj["color"]);
      e.setAttribute("r", obj["size"]);
      e.setAttribute("visibility", "visible");
      if ("class" in obj) {
        e.className = obj["class"];
      }
      break;
    case "js/text":
      var e = obj.elem;
      if ("background-color" in obj) {
        obj.elem.style.backgroundColor = obj["background-color"];
      }
      if ("class" in obj) {
        e.className = obj["class"];
      }

      _.each(obj.queue, function(f) {
        checkFrame(f);
      });
      obj.queue = [];

      break;
    default:
      _.each(obj.queue, function(f) {
        checkFrame(f);
      });
      break;
  }
}

var getObj = function(id) {
  var id = JSON.stringify(id);
  if (!objects[id]) {
    objects[id] = {"queue":[]};
  }
  return objects[id];
}

var setObjAttr = function(id, k, v) {
  var obj = getObj(id);
  obj[k] = v;
  setObjAttributes(obj);
}

var getObjAttr = function(id, k) {
  return objects[JSON.stringify(id)][k];
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

var editCommands =
  { "Enter":["enter", true], ",":["comma", true], "Tab":["tab",true]
  , "Space":["space", true]
  , "Escape":["esc", true]
  };

var nodeCommands = {};
//  { "h":["h", false]
//  , "l":["l", false]
//  , "j":["j", false]
//  , "k":["k", false]
//  , "i":["i", false]
//  , "A":["A", false]
//  , "I":["I", false]
//  , "<":["<", false]
//  , ">":[">", false]
//  };

var editCommand = function(key) {
  return editCommands[key][0];
}

var nodeCommand = function(key) {
  return nodeCommands[key][0];
}

var editStopDefault = function(key) {
  return editCommands[key][1];
}

var nodeStopDefault = function(key) {
  return nodeCommands[key][1];
}

//var sel = function() {
//  if (window.getSelection)
//    return window.getSelection().getRangeAt(0);
//  else if(document.selection)
//    return document.selection.createRange();
//}

var bodyHandler = function(sock) {
  return function(ev) {
    ev.stopPropagation();
    var key = ev.key;
    if (key in nodeCommands) {
      if (nodeStopDefault(key)) {
        ev.preventDefault();
      }
      keyCommand2(sock, nodeCommand(key));
    } else {
      //console.log(ev);
    }
  };
}

var textNodeHandler = function(el, id, sock) {
  return function(ev) {
    ev.stopPropagation();
    var key = ev.key;
    if (key in nodeCommands) {
      if (nodeStopDefault(key)) {
        ev.preventDefault();
      }
      keyCommand(id, sock, nodeCommand(key));
    } else {
      //console.log(ev);
    }
  };
}

var textEditHandler = function(el, id, sock) {
  return function(ev) {
    ev.stopPropagation();
    var key = ev.key;
    if (key in editCommands) {
      if (editStopDefault(key)) {
        ev.preventDefault();
      }
      // Send key command along with current buffer contents
      textEntryCommand(id, sock, editCommand(key), el.innerHTML);
    } else {
      //console.log(ev);
    }
  };
}

var removeObject = function(id) {
  var elem = objects[JSON.stringify(id)].elem;
  if (elem.parentNode) {
    elem.parentNode.removeChild(elem);
  }
}

// TODO abstract the repeated structure of this switch
var parseTuple = function(sock) {
  return function(t) {

    var sign = t[0] === "Positive" ? true : false;
    var label = t[1].contents[0];
    var arity = t[1].contents[1]; // used sparingly
    var nodes = t[2];
    var tid = t[3];
    var tval = t[4];

    switch (label) {
      case "token":
        var id = nodes[0];
        if (sign) {
          var elem = mkToken(id, sock);
          setObjAttr(id, "elem", elem);
          setObjAttr(id, "type", "token");
        } else {
          mkDelete(id);
          //removeObject(id);
        }
        break;
      case "js/rect":
        var id = nodes[0];
        if (sign) {
          var elem = mkRect(id, tid, sock);
          setObjAttr(id, "elem", elem);
          setObjAttr(id, "type", "thing");
        } else {
          mkDelete(id);
          //removeObject(id);
        }
        break;
      case "js/element":
        var id = nodes[0];
        if (sign && tval) {
          var type = nodes[1].contents;
          var elem = mkElement(type, id, tid, sock);
          setObjAttr(id, "elem", elem);
        } else {
          mkDelete(id);
          //removeObject(id);
        }
        break;
      case "js/svg":
        var id = nodes[0];
        if (sign) {
          var elem = mkSVG(id, tid, sock);
          setObjAttr(id, "elem", elem);
        } else {
          mkDelete(id);
          //removeObject(id);
        }
        break;
      case "js/attr":
        var attr = nodes[0].contents;
        var val = nodes[1].contents;
        var id = nodes[2];
        if (sign && tval) {
          mkAttr(id, attr, val);
        }
        break;
      case "js/edit":
        var id = nodes[0];
        //var ruleid = nodes[1];
        var str = nodes[1].contents;
        if (sign && tval) {
          var cm = makeLineCM(id, str, sock);
          console.log(cm);
          cm.focus();
        } else if (tval) {
          mkDelete(id);
          //removeObject(id);
        }
        break;
      case "js/text":
        var id = nodes[0];
        if (sign && tval) {
          var body = toString(nodes[1]); // string
          var el = mkNode(body, id, tid, sock);
          setObjAttr(id, "type", "js/text");
          setObjAttr(id, "elem", el);
          setObjAttr(id, "tid", tid);
        } else if (tval) {
          mkDelete(id);
          //removeObject(id);
        }
        break;
      case "refresh-code-mirror":
        var id = nodes[0];
        var par = nodes[1];
        var handler = function() {
          var cm = getObjAttr(id, "code-mirror");
          cm.refresh();
          cm.focus();
        }
        mkFrame([id, par], handler);
      case "x-rank":
        var id = nodes[0];
        var n = nodes[1].contents;
        update_rank("x", n);
        setObjAttr(id, "x-rank", n);
        break;
      case "y-rank":
        var id = nodes[0];
        var n = nodes[1].contents;
        update_rank("y", n);
        setObjAttr(id, "y-rank", n);
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
        setObjAttr(id, "color", color);
        break;
      case "font-color":
        var id = nodes[0];
        var color = nodes[1].contents;
        setObjAttr(id, "color", color);
        break;
      case "size":
        var id = nodes[0];
        var r = nodes[1].contents;
        setObjAttr(id, "size", r);
        break;
      case "child":
        // child, parent
        var id1 = nodes[0];
        var id2 = nodes[1];
        if (sign && tval) {
          if (arity == 2) {
            mkParent(id1, id2);
          } else if (arity == 3) {
            var rank = nodes[2].contents;
            mkPositionalParent(id1, id2, rank);
            mkAttr(id1, "rank", rank);
          }
        } // TODO otherwise remove child?
        break;
      case "background-color":
        var id = nodes[0];
        if (sign) {
          var c = nodes[1].contents;
          mkStyle(id, "backgroundColor", c);
        } else if (tval) {
          mkStyle(id, "backgroundColor", "transparent");
        }
        break;
      // TODO: check this
      case "js/style":
        var attr = nodes[0].contents;
        var value = nodes[1].contents;
        var id = nodes[2];
        if (sign && tval) {
          mkStyle(id, attr, value);
        }
        break;
      case "class":
        var id = nodes[0];
        var c = nodes[1].contents;
        setObjAttr(id, "class", c);
        break;
      case "log":
        var c = _.map(nodes, function(n) { return n.contents });
        console.log(c);
        break;
    }
    // TODO: ??
    var lg = get("log");
    if (lg) {
      lg.scrollTop = lg.scrollHeight;
    }
    var rp = get("repl-log");
    if (rp) {
      console.log('ok');
      rp.scrollTop = rp.scrollHeight;
    }
  }
}

var makeMainCM = function() {
  var mainCM = CodeMirror(get("edit"), {
    //keyMap: "vim",
    value: "hi\nthere\n",
    readOnly: true,
    cursorBlinkRate: 0,
    gutters: ["gutter"],
  });
}

window.onload = function() {
  // TODO bad
  //var rootElems = ["rules", "app", "log"];
  var rootElems = [];
  setObjAttr(strNode("body"), "elem", document.body);
  rootElems.forEach(function(str) {
    var rulesId = strNode(str);
    setObjAttr(rulesId, "elem", get(str));
  });

  // Some components will use right-click inputs; best to disable it everywhere?
  //document.addEventListener('contextmenu', event => event.preventDefault());

  sock = initSock();
}


var parseURL = function(sock) {
  var u = new URL(window.location.href)
  var dir = u.searchParams.get("file")

  sock.send(mkTuple("request-app", [strNode(dir)]));
}
