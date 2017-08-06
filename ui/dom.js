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

// TODO codemirror
//var mkEditor = function(str, id, sock, special_key_handler, other) {
//  var el = document.createElement("textarea");
//  el.setAttribute("id", JSON.stringify(id));
//  el.setAttribute("tabindex", 0);
//  el.className = "editor";
//  el.innerHTML= str;
//  el.addEventListener("keydown", special_key_handler(el, id, sock));
//  addClickHandlers(el, id, sock);
//  if (other)
//    append(el, other);
//  return el;
//}

var mkNode = function(str, id, tid, sock, special_key_handler, other) {
  var el = document.createElement("div");
  el.setAttribute("id", JSON.stringify(id));
  el.setAttribute("tabindex", 0);
  el.className = "node";
  el.innerHTML = str;
  addClickHandlers(el, id, tid, sock);
  if (other)
    append(el, other);
  return el;
}

var svgurl = 'http://www.w3.org/2000/svg';
var mkCircle = function() {
  var el = document.createElementNS(svgurl, "circle");
  el.setAttribute("stroke", "#000");
  append(el, get("svg-main"));
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
    console.log('lclick');
    clickCommand(id, tid, ev.button);
    ev.stopPropagation();
  });
  el.addEventListener("contextmenu", function(ev) {
    console.log('rclick');
    ev.stopPropagation();
    ev.preventDefault();
    clickCommand(id, tid, ev.button);
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
