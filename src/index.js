require('./main.css');

var Elm = require('./Main.elm');
var root = document.getElementById('root');
var app = Elm.Main.embed(root);

window.onscroll = function() {
  var newScroll = window.pageYOffset || document.body.scrollTop;
  app.ports.scroll.send([0, newScroll]);
};
