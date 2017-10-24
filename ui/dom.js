var append = function(a, b) {
  b.appendChild(a);
  return a;
}

// TODO remove?
var setFontSize = function(size) {
  var fix = function(el) {
    el.style.fontSize = size;
  }
  document.getElementsByClassName("editor").each(fix);
  document.getElementsByClassName("node").each(fix);
}

var mkText = function(str, elem) {
  var t = document.createTextNode(str);
  if (elem)
    append(t, elem);
  return t;
}

var mkElement = function(type, id, tid, sock) {
  var el = document.createElement(type);
  el.setAttribute("id", JSON.stringify(id));
  el.setAttribute("tabindex", 0);
  addClickHandlers(el, id, tid, sock);
  return el;
}

var mkSVG = function(id, tid, sock) {
  var el = document.createElementNS(svgurl, "svg");
  el.setAttribute("id", JSON.stringify(id));
  el.setAttribute("tabindex", 0);
  addClickHandlers(el, id, tid, sock);
  return el;
}

var mkNode = function(str, id, tid, sock) {
  var el = document.createElement("div");
  el.setAttribute("id", JSON.stringify(id));
  el.setAttribute("tabindex", 0);
  el.className = "node";
  el.innerHTML = str;
  addClickHandlers(el, id, tid, sock);
  return el;
}

var makeLineCM = function(id, str, sock) {
  // nb: doesn't attach anything
  var attacher = function(el) {
    setObjAttr(id, "elem", el);
  }
  var lineCM = CodeMirror(attacher, {
    smartIndent: false,
    keyMap: "emacs",
    cursorBlinkRate: 0,
    lineWrapping: true,
  });

  setObjAttr(id, "code-mirror", lineCM);

  lineCM.setOption("extraKeys", {
    Enter: function() {
      var v = lineCM.getValue();
      sock.send(mkTuple("js/text-entry", [id, strNode(v)]));
      //lineCM.setValue("");
      return;
      // TODO:
      if (v.length == 0) {
        return;
      }
      if (v[v.length-1] == ".") {
        console.log('SEND');
        var value = v.slice(0,v.length-1);
        sock.send(mkTuple("update-rule", [ruleid, strNode(value)]));
        lineCM.setValue("");
      } else if (v[v.length-1] == "!") {
        console.log('cancel');
        lineCM.setValue("");
        // update id
      } else {
        console.log(v);
      }
    }
  });

  lineCM.on("change", function() {
    console.log('change');
  });

  lineCM.setValue(str);

  return lineCM;
}

var svgurl = 'http://www.w3.org/2000/svg';
var mkCircle = function() {
  var el = document.createElementNS(svgurl, "circle");
  el.setAttribute("stroke", "#000");
  append(el, get("svg-main"));
  return el;
}

var mkRect = function(id, tid, sock) {
  var el = document.createElementNS(svgurl, "rect");
  el.setAttribute("id", JSON.stringify(id));
  el.setAttribute("tabindex", 0);
  addClickHandlers(el, id, tid, sock);
  return el;
}

// TODO
var mkLine = function() {
  var el = document.createElementNS(svgurl, "line");
  append(el, get("svg-main"));
  return el;
}

var mkToken = function(id, sock) {
  var el = mkCircle();
  el.setAttribute("visibility", "hidden");
  addClickHandlers(el, id, undefined, sock);
  el.addEventListener("mouseover", function() {
    //TODO
    //overCommand(id);
  });
  el.addEventListener("mouseout", function() {
    //TODO
    //outCommand(id);
  });

  return el;
}

var mkBox = function(str, id, other) {
  var el = document.createElement('div');
  el.classList.add('box');
  el.id = id;
  mkText(str, el);
  if (other)
    append(el, other);
  el.addEventListener("mouseover", function() {
  });
  el.addEventListener("mouseleave", function() {
  });
  return el;
}

var addClickHandlers = function(el, id, tid, sock) {
  el.addEventListener("click", function(ev) {
    clickCommand(id, tid, ev.button, ev.shiftKey);
    ev.stopPropagation();
  });
  el.addEventListener("contextmenu", function(ev) {
    ev.stopPropagation();
    ev.preventDefault();
    clickCommand(id, tid, ev.button, ev.shiftKey);
  });
}

var insertPositional = function(childID, child, par, rank) {
  var done = _.find(par.children, function(c) {
    if (!c.hasAttribute("rank")) {
      return false;
    }
    if (c.getAttribute("rank") > rank) {
      par.insertBefore(child, c)
      return true
    }
  });
  if (!done) {
    par.appendChild(child);
  }
}

var get = function(id) {
  return document.getElementById(id);
}
