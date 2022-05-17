// app/js/index.js

// Use rhino::build_js() after changes

function clearUpload(id) {
  var app = '#' + id + '-upload';
  $(app).parent().parent().next()[0].value = '';
}

function addTooltip(id, type, opts) {
  var $id = shinyBS.getTooltipTarget(id);
  var dopts = {html: true};
  opts = $.extend(opts, dopts);
  if(type == "tooltip") {
    $id.tooltip("destroy");
    setTimeout(function() {$id.tooltip(opts);},200);
  } else if(type == "popover") {
    $id.popover("destroy");
    setTimeout(function() {$id.popover(opts);},200);
  }
}

export const extensions = {clearUpload, addTooltip};
