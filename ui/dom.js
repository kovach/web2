var append = function(a, b) {
  b.appendChild(a);
  return a;
}

var mkText = function(str, elem) {
  var t = document.createTextNode(str);
  if (elem)
    append(t, elem);
  return t;
}

var mkEditor = function(str, id, other) {
  var el = document.createElement('textarea');
  el.placeholder= str;
  //TODO move
  //el.addEventListener('keydown', editorHandler(sock, id));

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
  el.addEventListener("click", function(ev) {
    sock.send(clickCommand(id, ev.button));
  });
  el.addEventListener("contextmenu", function(ev) {
    ev.stopPropagation();
    ev.preventDefault();
    sock.send(clickCommand(id, ev.button));
  });
  el.addEventListener("mouseover", function() {
    //TODO
    //sock.send(overCommand(id));
  });
  el.addEventListener("mouseout", function() {
    //TODO
    //sock.send(outCommand(id));
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
  el.addEventListener('onmouseenter', function() {
  });
  el.addEventListener('onmouseleave', function() {
  });
  return el;
}

var create = function(type, id) {
  var el = document.createElement(type);
  el.id = id;
  return el;
}

var get = function(id) {
  return document.getElementById(id);
}
