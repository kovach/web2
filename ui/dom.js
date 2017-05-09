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

var mkEditor = function(str, id, sock, other) {
  var el = document.createElement('textarea');
  el.placeholder= str;
  el.addEventListener('keydown', editorHandler(sock, id));

  if (other)
    append(el, other);
  return el;
}

var mkLine = function(other) {
  var el = document.createElement('hr');
  if (other)
    append(el, other);
}

var mkBox = function(str, id, socket, other) {
  //console.log('boxing', str);
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
