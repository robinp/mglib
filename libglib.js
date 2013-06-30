glib = {
  loadImage: function(src, onloadCb, onerrorCb) {
    var img = new Image();
    img.onload = function(){ onloadCb(img); }
    img.onerror = function(){ onerrorCb(src); }
    img.src = src;
  }
}
