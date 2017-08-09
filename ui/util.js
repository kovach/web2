var checkFrame = function(f) {
  var hook = _.find(f.ids, function(id) {
    return !("elem" in getObj(id));
  });
  if (hook) {
    getObj(hook).queue.push(f);
  } else {
    f.handler(f);
  }
}

// when we set id.elem, call checkFrame on each childFrame
// this will evaluate the ones that are ready, and attach others to a different
// watch list

var mkFrame = function(ids, f) {
  var f = {
    ids: ids,
    handler: f,
  }

  checkFrame(f);
}

var mkParent = function(child, par) {
  var handler = function() {
    append(getObjAttr(child, "elem"), getObjAttr(par, "elem"));
  }
  mkFrame([child, par], handler);
}

var mkAttr = function(id, name, val) {
  var handler = function() {
    getObjAttr(id, "elem").setAttribute(name, val);
  }
  mkFrame([id], handler);
}

var mkStyle = function(id, name, val) {
  var handler = function() {
    var obj = getObjAttr(id, "elem");
    obj.style[name] = val;
  }
  mkFrame([id], handler);
}

var mkPositionalParent = function(child, par, rank) {
  var handler = function() {
    insertPositional(child, getObjAttr(child, "elem"), getObjAttr(par, "elem"), rank);
  }
  mkFrame([child, par], handler);
}

var toString = function(node) {
  switch(node.tag) {
    case "NString":
      return node.contents;
      break;
    case "NInt":
      return node.contents;
      break;
    case "NSymbol":
      return "'"+node.contents;
      break;
    case "NNode":
      return "#"+node.contents;
    default:
      console.log('wtf: ', node);
      break;
  }
}
