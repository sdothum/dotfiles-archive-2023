/* lifted from http://njw.me.uk/simplyread/ */

if(window.content && window.content.document && window.content.document.simplyread_original === undefined) window.content.document.simplyread_original = false;

function simplyread(nostyle, nolinks)
{
  /* count the number of <p> tags that are direct children of parenttag */
  function count_p(parenttag)
  {
    var n = 0;
    var c = parenttag.childNodes;
    for (var i = 0; i < c.length; i++) {
      if (c[i].tagName == "p" || c[i].tagName == "P")
        n++;
    }
    return n;
  }

  var doc;
  doc = (document.body === undefined)
        ? window.content.document : document;

  /* if simplyread_original is set, then the simplyread version is currently active,
   * so switch to the simplyread_original html */
  if (doc.simplyread_original) {
    doc.body.innerHTML = doc.simplyread_original;
    for (var i = 0; i < doc.styleSheets.length; i++)
      doc.styleSheets[i].disabled = false;
    doc.simplyread_original = false
    return 0;
  }

  doc.simplyread_original = doc.body.innerHTML;
  doc.body.innerHTML = doc.body.innerHTML.replace(/<br[^>]*>\s*<br[^>]*>/g, "<p>");

  var biggest_num = 0;
  var biggest_tag;

  /* search for tag with most direct children <p> tags */
  var t = doc.getElementsByTagName("*");
  for (var i = 0; i < t.length; i++) {
    var p_num = count_p(t[i]);
    if (p_num > biggest_num) {
      biggest_num = p_num;
      biggest_tag = t[i];
    }
  }

  if (biggest_num == 0) return 1;

  /* save and sanitise content of chosen tag */
  var fresh = doc.createElement("div");
  fresh.innerHTML = biggest_tag.innerHTML;
  fresh.innerHTML = fresh.innerHTML.replace(/<h1>.*<\/h1>/g, "");
  fresh.innerHTML = fresh.innerHTML.replace(/<\/?font[^>]*>/g, "");
  fresh.innerHTML = fresh.innerHTML.replace(/style="[^"]*"/g, "");
  if(nolinks)
    fresh.innerHTML = fresh.innerHTML.replace(/<\/?a[^>]*>/g, "");
  fresh.innerHTML = fresh.innerHTML.replace(/<\/?span[^>]*>/g, "");
  fresh.innerHTML = fresh.innerHTML.replace(/<style[^>]*>/g, "<style media=\"aural\">"); /* ensures contents of style tag are ignored */

  for (var i = 0; i < doc.styleSheets.length; i++)
    doc.styleSheets[i].disabled = true;

  srstyle =
    "* {" +
    "  color: #000;" +
    "  font-family: Arial, sans-serif;" +
    "}" +
    "body {" +
    "  background: #fdf6e3 none;" +
    "  font-size: 0.85em;" +
    "}" +
    "h1 {" +
    "  font-size: 1.85em;" +
    "  font-weight: normal;" +
    "  text-align: center;" +
    "  text-transform: lowercase;" +
    "}" +
    "h2,h3,h4 {" +
    "  font-weight: normal;" +
    "  text-align: right;" +
    "  text-transform: lowercase;" +
    "}" +
    "p {" +
    "  color: #444;" +
    "  font-family: Verdana, sans-serif;" +
    "  line-height: 1.65em;" +
    "  margin: 0 auto;" +
    "  padding-bottom: 1.35em;" +
    "  text-indent: 1.6em;" +
    "}" +
    "h1+p,h2+p,h3+p,h4+p {" +
    "  text-indent: 0;" +
    "}" +
    "p a {" +
    "  color: #B3431E;" +
    "  font-family: Verdana, sans-serif;" +
    "}" +
    "li {" +
    "  padding-bottom: 0.55em;" +
    "}" +
    "code {" +
    "  color: #4B4D4E;" +
    "  display: block;" +
    "  font-family: Inconsolata, monospace;" +
    "  font-size: 0.85em;" +
    "  line-height: 1.45em; " +
    "  text-indent: 0em;" +
    "}" +
    "img, iframe {" +
    "  display: block;" +
    "  max-width: 32em;" +
    "  height: auto;" +
    "}" +
    ".caption,.hidden,a img,.image,.photo,.share-help,.sharetools,.story-header,.video,.visually-hidden {" +
    "  display: none;" +
    "}" +
    "div#sr {" +
    "  hyphens: auto;" +
    "  margin: auto;" +
    "  padding: 8em;" +
    "  padding-top: 2em;" +
    "  text-align: normal;" +
    "  text-rendering: optimizeLegibility;" +
    "  width: 32em;" +
    "}";

  doc.body.innerHTML =
    "<style type=\"text/css\">" + (nostyle ? "" : srstyle) + "</style>" +
    "<div id=\"sr\">" + "<h1>"+doc.title+"</h1>" + fresh.innerHTML + "</div>";

  return 0;
}

/* lifted from http://readable.tastefulwords.com/ */

function readable()
{
  _readableOptions = {
    'base':                'blueprint',
    'box_width':           '32em',
    'color_background':    '#FDF6E3',
    'color_links':         '#99CCFF',
    'color_text':          '#1B1D1E',
    'custom_css':          'h1 { font-weight: normal; text-align: center !important; text-transform: lowercase; } h2,h3,h4 { font-weight: normal; text-align: right !important; text-transform: lowercase; } p { color: #444; padding-bottom: 0.35em ; text-indent: 1.6em; } p a { color: #B3431E; } code { display: block; line-height: 1.45em; margin-left: 1.65em; text-indent: -1.65em; } img { display: block; max-width: 32em; height: auto; }',
    'text_align':          'normal',
    'text_font_header':    'Helvetica, quote(Helvetica Neuve), Arial, Tahoma, sans-serif',
    'text_font':           'Verdana, Geneva, Helvetica Neue, Arial, DejaVu Sans, sans-serif',
    'text_font_monospace': 'Inconsolata',
    'text_line_height':    '1.65em',
    'text_size':           '0.85em',
  };

  if (document.getElementsByTagName('body').length>0)
    ;
  else
    return;

  if (window.$readable) {
    if (window.$readable.bookmarkletTimer)
      return;
  }
  else
    window.$readable={};

  window.$readable.bookmarkletTimer=true;
  window.$readable.options=_readableOptions;
  if (window.$readable.bookmarkletClicked) {
    window.$readable.bookmarkletClicked();
    return;
  }

  _readableScript=document.createElement('script');
  _readableScript.setAttribute('src','http://readable-static.tastefulwords.com/target.js?rand='+encodeURIComponent(Math.random()));
  document.getElementsByTagName('body')[0].appendChild(_readableScript);
}

/* google translate */

function translate()
{
  var t = ((window.getSelection&&window.getSelection()) || (document.getSelection&&document.getSelection()) || (document.selection&&document.selection.createRange&&document.selection.createRange().text));
  var e = (document.charset || document.characterSet);
  if (t != '') {
    location.href='http://translate.google.com/translate_t?text='+ t +'&hl=en&langpair=auto|en&tbb=1&ie='+e;
  }
  else {
    location.href='http://translate.google.com/translate?u='+escape(location.href)+'&hl=en&langpair=auto|en&tbb=1&ie=' + e;
  };
}
