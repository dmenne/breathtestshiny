Shiny.addCustomMessageHandler("replace_url",
  function(url) {
    location.replace(url);
  }
);

(function($) {
    $.QueryString = (function(a) {
        if (a === "") return {};
        var b = {};
        for (var i = 0; i < a.length; ++i)
        {
            var p=a[i].split('=', 2);
            if (p.length != 2) continue;
            b[p[0]] = decodeURIComponent(p[1].replace(/\+/g, " "));
        }
        return b;
    })(window.location.search.substr(1).split('&'));
})(jQuery);


shinyBS.addTooltip = function(id, type, opts) {
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
};


