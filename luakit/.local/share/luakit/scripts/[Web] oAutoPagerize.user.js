// ==UserScript==
// @name           [Web] oAutoPagerize
// @namespace      http://ss-o.net/
// @description    loading next page and inserting into current page. oAutoPagerize supports for Opera 9.5+, GreaseKit(Safari/WebKit)
// @version        1.5.2
// @include        http://*
// @include        https://*
// @exclude        https://mail.google.com/*
// @exclude        http://b.hatena.ne.jp/*
// ==/UserScript==
//
// author: os0x( http://d.hatena.ne.jp/os0x/ )
//
// this script based on
// AutoPagerize_opera ( http://d.hatena.ne.jp/gnarl/20070603/1180820465 id:gnarl) and
// AutoPagerize ( http://userscripts.org/scripts/show/8551 id:swdyh) and
// GoogleAutoPager ( http://la.ma.la/blog/diary_200506231749.htm ) and
// estseek autopager ( http://la.ma.la/blog/diary_200601100209.htm ).
// thanks to ma.la.
//
// Released under the GPLv3
// http://www.gnu.org/copyleft/gpl.html
//
// UPDATE INFO (Only Japanese) http://d.hatena.ne.jp/os0x/searchdiary?word=%2a%5boAutoPagerize%5d
//

(function _oAutoPagerize(window,loaded){
	var oAutoPagerize = _oAutoPagerize;
	var Set = window.AutoPagerizeSettings || {
		TOP_SITEINFO:[]
		,BOTTOM_SITEINFO:[]
	};
	var HTML_NAMESPACE = 'http://www.w3.org/1999/xhtml';
	var SITEINFO_SERVER = Set.SITEINFO_SERVER ?
		Set.SITEINFO_SERVER.replace('%url',encodeURIComponent(location.href)) :
		'http://ss-o.net/json/wedataAutoPagerizeSITEINFO.js';
	Set.DISABLE_SITE = Set.DISABLE_SITE || [];
	if (window.AutoPagerizeWedataSiteinfo) {
		var SITEINFO = window.AutoPagerizeWedataSiteinfo;
	} else if (window.name !== 'oAutoPagerizeRequest') {
		if (Set.DISABLE_IFRAME && window.parent !== window) return;
		insertSITEINFO();
	}

	var isSafari3 = window.getMatchedCSSRules && !window.postMessage;
	window.AutoPagerizeSettings = null;
	window.AutoPagerizeWedataSiteinfo = null;

	// disable site set
	var disable = Set.DISABLE_SITE.concat([
		'pagead\\d\\.googlesyndication\\.com/pagead/ads'
		//,''
	]);
	var naviType = Set.naviType || 'number'; // 'link' or 'number';
	var DebugMode = Set.DebugMode || false;
	var URL = 'http://d.hatena.ne.jp/os0x/searchdiary?word=%2a%5boAutoPagerize%5d';
	var UPDATE_URL = window.opera ? 'http://ss-o.net/userjs/0AutoPagerize.SITEINFO.js' : 'http://ss-o.net/userjs/0AutoPagerize.SITEINFO.user.js';
	var VERSION = '1.5.2';
	var AUTO_START = Set.AUTO_START !== false;
	//var CACHE_EXPIRE = 24 * 60 * 60 * 1000;
	var BASE_REMAIN_HEIGHT = Set.BASE_REMAIN_HEIGHT || 400;
	var FORCE_TARGET_WINDOW = Set.FORCE_TARGET_WINDOW !== false;
	var TARGET_WINDOW_NAME = Set.TARGET_WINDOW_NAME || '_blank';
	var DISABLE_IFRAME = Set.DISABLE_IFRAME || false;
	var HISTORY_MODE_FAST = Set.HISTORY_MODE_FAST || false;

	var COLOR = {
		on         : '#00ff00',
		off        : '#cccccc',
		loading    : '#00ffff',
		terminated : '#0000ff',
		error      : '#ff00ff'
	};
	var ICON_SIZE = Set.ICON_SIZE || 10;
	if (Set.COLOR) (function(c){
		for (var i in c) {
			COLOR[i] = c[i];
		}
	})(Set.COLOR);
	var MICROFORMATs = [
		{
			 url          : '^https?://.'
			,nextLink     : '//a[@rel="next"] | //link[@rel="next"]'
			,insertBefore : '//*[contains(concat(" ",@class," "), " autopagerize_insert_before ")]'
			,pageElement  : '//*[contains(concat(" ",@class," "), " autopagerize_page_element ")]'
		}
		,{
			 url          : '^https?://.'
			,nextLink     : '//link[@rel="next"] | //a[contains(concat(" ",@rel," "), " next ")] | //a[contains(concat(" ",@class," "), " next ")]'
			,pageElement  : '//*[contains(concat(" ",@class," "), " hfeed ") or contains(concat(" ",@class," "), " xfolkentry ")]'
		}
	];

	var useIframe = false;
	var locationHref = location.href;
	// check disable site
	if (disable.some(function(ads) {
		return new RegExp(ads).test(locationHref);
	})) return;

	var TOP_SITEINFO = Set.TOP_SITEINFO.concat([
		/*
		{
			 url:         'https?://(?:explore\\.)?twitter\\.com/'
			,nextLink:    '//div[@class="pagination"]/a[last()]'
			,incremental: {
				nextMatch: '\\?page=(\\d+)'
				,nextLink: '?page=#'
				,step:     1
			}
			,pageElement: '//table[@class="doing"]'
			,exampleUrl:  'http://twitter.com/os0x'
		},
		*/
		{
			 url:         '^http://m\\.twitter\\.com/'
			,nextLink:    '//a[@accesskey="6"]'
			,pageElement: '//ul/li'
			,exampleUrl:  'http://m.twitter.com/os0x'
		}
		,{
			url:           '^http://eow\\.alc\\.co\\.jp/[^/]+'
			,nextLink:     'id("AutoPagerizeNextLink")'
			,pageElement:  'id("resultsList")/ul'
			,exampleUrl:   'http://eow.alc.co.jp/%E3%81%82%E3%82%8C/UTF-8/ http://eow.alc.co.jp/are'
		}
		,{
			url:           '^http://matome\\.naver\\.jp/\w+'
			,nextLink:     '//a[@class="next"]'
			,pageElement:  'id("itemDetail")/li'
			,exampleUrl:   'http://matome.naver.jp/odai/2127476492987286301'
		}
	]);
	var BOTTOM_SITEINFO = Set.BOTTOM_SITEINFO.concat([
		/* template
		,{
			 url:          ''
			,nextLink:     ''
			//,insertBefore: ''
			,pageElement:  ''
			,exampleUrl:   ''
		}
		*/
	]);
	if (window != window.parent) { //if frame
		if (window.opera && window.name == 'oAutoPagerizeRequest' && window.parent.AutoPagerize && window.parent.AutoPagerize.AutoPagerObject) {
			try {
				var node = document.body || document.getElementsByTagName('head')[0];
				if (node) {
					var style = document.createElementNS(HTML_NAMESPACE, 'style');
					style.type = 'text/css';
					style.textContent = 'body{display:none ! important;}';
					node.appendChild(style);
				} else {
					document.write('<style type="text/css">body{display:none ! important;}</style>');
				}
				window.opera.addEventListener( 'BeforeEventListener', function (e) {
					if (e.event.type != 'DOMContentLoaded') e.preventDefault();
				}, false );
				document.addEventListener('DOMContentLoaded',function(){
					var _AutoPager = window.parent.AutoPagerize.AutoPagerObject;
					_AutoPager._frameLoad.call(_AutoPager);
					setTimeout(function(){
						_AutoPager.removeIframe();
					},100);
				},false);
			} catch (e) {
				log(e);
			}
			return;
		}
		if (DISABLE_IFRAME)
			return;
	}
	var miscellaneous = [];
	if (/^http:\/\/(www|images)\.google\.(?:[^.]+\.)?[^.\/]+\/images\?./.test(locationHref)) {
		miscellaneous.push(function(utils){
			//via http://furyu.tea-nifty.com/annex/2008/04/autopagerizeaut_c163.html
			var info, href = location.href.replace(/^http:\/\/www/,'http://images') + '&gbv=1';
			SITEINFO.some(function(site){
				var r = new RegExp(site.url);
				var m = !r.test('http://a')  && r.test(href);
				if (m) {
					info = site;
					site.url = 'http://..';
					return r;
				}
			});
			if (!info) return;
			var a = utils.getFirstElementByXPath(info.nextLink);
			if (a) {
				a.href = a.href.replace(/(\?)gbv=2&|&gbv=2(&?)/,'$1$2') + '&gbv=1';
			}
			setTimeout(function(){
				new Image().src = 'http://www.google.com/images?gbv=2&hl=ja&q=AutoPagerize?update='+(new Date()).getTime();
				// for delete cookie(PREF= .. GBV=1 ..)
			},100);
		});
	} else if (
		/^http:\/\/\w+\.google\.(?:[^.]+\.)?[^.\/]+/.test(locationHref) && (/^\/videosearch\?/.test(location.pathname) || /tbs=vid(%3A|:)1/.test(location.search))
		) {
		useIframe = true;
	} else if (location.host === 'eow.alc.co.jp') {
		var alc = function(doc){
			var a,r = doc.evaluate('id("paging")/a[last()]',doc,null,XPathResult.FIRST_ORDERED_NODE_TYPE,null);
			if (r.singleNodeValue) a = r.singleNodeValue;
			else return;
			a.id = 'AutoPagerizeNextLink';
			a.href = a.href.replace(/javascript:goPage\("(\d+)"\)/,'./?pg=$1');
		};
		miscellaneous.push(function(){
			alc(document);
			setTimeout(function(){if (window.AutoPagerize) AutoPagerize.addDocumentFilter(alc);},500);
		});
	} else if (location.host==='matome.naver.jp') {
		var info, href = location.href;
		SITEINFO.some(function(site){
			var r = new RegExp(site.url);
			var m = !r.test('http://a')  && r.test(href);
			if (m) {
				info = site;
				return true;
			}
		});
		if (info) {
			miscellaneous.push(function(utils){
				var X = utils.getFirstElementByXPath;
				var naver = function(doc){
						var next = X(info.nextLink, doc);
						if (next){
							if (next.getAttribute('onclick')) {
								var nextpage = next.getAttribute('onclick').match(/(\d+)/)[1];
								var form=document.getElementsByName('missionViewForm')[0];
								var param=[].slice.call(form).map(function(i){return i.name+'='+(i.name==='page'?nextpage:i.value);}).join('&');
								next.href = location.pathname+'?'+param;
							} else {
								next.parentNode.removeChild(next);
							}
						}
				};
				naver(document);
				setTimeout(function(){if (window.AutoPagerize) AutoPagerize.addDocumentFilter(naver);},500);
			});
		}
	}
	if (HISTORY_MODE_FAST && window.opera) {
		opera.addEventListener('AfterEvent.GM_AutoPagerizeNextPageLoaded', function(){
			if (window.AutoPagerize)
				window.opera.setOverrideHistoryNavigationMode('fast');
			opera.removeEventListener('AfterEvent.GM_AutoPagerizeNextPageLoaded',arguments.callee,false);
		}, false);
	}

	var autopager = function() {
		if (!SITEINFO) return;
		miscellaneous.forEach(function(f){f({getFirstElementByXPath:getFirstElementByXPath});});
		log('oAutoPagerize loading');
		if (!document.body) return;
		var docRoot = /BackCompat/.test(document.compatMode) ? document.body : document.documentElement;

		var AutoPager = function() {this.initialize.apply(this, arguments);};
		AutoPager.prototype = {
			initialize:function(info){
				log('new AutoPager('+info+')');
				this.pageNum = 1;
				this.info = info;
				this.state = AUTO_START;
				var self = this;
				var url = this.requestURL = this.getNextURL(info.nextLink, document);
				if (info.insertBefore) {
					this.insertPoint = getFirstElementByXPath(info.insertBefore);
				}
				if (!this.insertPoint) {
					var lastPageElement = getElementsByXPath(info.pageElement).pop() || {};
					this.insertPoint = lastPageElement.nextSibling || lastPageElement.parentNode.appendChild(document.createTextNode(' '));
				}
				log('this.insertPoint.parentNode:'+this.insertPoint);
				if (!url || !this.insertPoint) return;
				this.loadedURLs = {};
				this.loadedURLs[location.href] = true;
				this.toggle = function() {self.stateToggle();};
				this.scroll = function() {self.onScroll();};
				window.addEventListener('scroll', this.scroll, false);
				this.initHelp();
				var scrollHeight = docRoot.scrollHeight;
				var bottom = getElementPosition(this.insertPoint).top || this.getPageElementsBottom() || (Math.round(scrollHeight * 0.8));
				this.remainHeight = scrollHeight - bottom + BASE_REMAIN_HEIGHT;
				// append space to show scrollbar surely.
				var pageHeight = window.opera ? document.documentElement.clientHeight : document.body.offsetHeight;
				if (window.innerHeight >= pageHeight) {
					document.body.appendChild(document.createElementNS(HTML_NAMESPACE, 'div')).setAttribute('style','position:absolute;bottom:-1px;height:1px;width:1px;');
				}
			}
			,getPageElementsBottom:function() {
				try {
					var elem = getElementsByXPath(this.info.pageElement).pop();
					return getElementBottom(elem);
				} catch(e) {}
			}
			,initHelp:function() {
				var icon = document.createElementNS(HTML_NAMESPACE, 'div');
				icon.setAttribute('id', 'autopagerize_icon');
				var css = [
					'#autopagerize_help {'
						,'margin:0 0 0 15px;'
						,'padding:0 5px;'
						,'font-size:10px;'
						,'background:#ffffff;'
						,'border:none;'
						,'color:#000000;'
						,'text-align:left;'
						,'font-weight:normal;'
						,'line-height:120%;'
						,'font-family:verdana;'
						,'display:none;'
					,'}'
					,'#autopagerize_help p{'
						,'margin:0;'
						,'padding:0 0 3px;'
						,'line-height:1.2;'
					,'}'
					,'#autopagerize_help .toggle{'
						,'float:right;padding:8px;'
					,'}'
					,'#autopagerize_help .autopagerize_status {'
						,'min-width:' + (140 + ICON_SIZE) + 'px;margin:0;padding:3px 0;list-style:none;text-align:left;'
					,'}'
					,'#autopagerize_help .autopagerize_status li{'
						,'margin:3px 0;padding:0;border:none;'
					,'}'
					,'#autopagerize_help .autopagerize_status li span{'
						,'display:inline-block;'
						,'vertical-align:middle;'
						,'margin:0 3px 0 0;'
					,'}'
					,'#autopagerize_help .autopagerize_status li span.autopagerize_icons{'
						,'width:12px;'
						,'height:12px;'
					,'}'
					,'#autopagerize_icon {'
						,'font-size:12px;'
						,'position:fixed;'
						,'top:3px;'
						,'right:3px;'
						,'background:',(this.state ? COLOR['on'] : COLOR['off']),';'
						,'color:#fff;'
						,'width:',ICON_SIZE,'px;'
						,'height:',ICON_SIZE,'px;'
						,'z-index:4294967295;'
						,'opacity:0.8;'
						,'overflow:hidden;'
						,'margin:0;'
						,'padding:0;'
					,'}'
					,'#autopagerize_icon:hover {'
						,'overflow:visible;'
						,'width:auto;'
						,'height:auto;'
						,'border:1px solid #cccccc;'
					,'}'
					,'#autopagerize_icon:hover #autopagerize_help {'
						,'display:block;'
					,'}'
				].join('');
				addCSS(css);
				document.body.appendChild(icon);
				this.icon = icon;
				var helpDiv = document.createElementNS(HTML_NAMESPACE, 'div');
				helpDiv.setAttribute('id', 'autopagerize_help');

				var toggleDiv = document.createElementNS(HTML_NAMESPACE, 'div');
				toggleDiv.className = 'toggle';
				var a = document.createElementNS(HTML_NAMESPACE, 'a');
				a.className = 'autopagerize_link';
				a.textContent = 'on/off';
				a.href = 'javascript:void 0';
				var self = this;
				a.addEventListener('click', function() {self.stateToggle();}, false);
				toggleDiv.appendChild(a);

				var ul = document.createElementNS(HTML_NAMESPACE, 'ul');
				ul.className = 'autopagerize_status';
				for (var i in COLOR) {
					var li = document.createElementNS(HTML_NAMESPACE, 'li');
					var span = document.createElementNS(HTML_NAMESPACE, 'span');
					var text = document.createElementNS(HTML_NAMESPACE, 'span');
					span.className = 'autopagerize_icons';
					span.style.background = COLOR[i];
					text.textContent = i;
					li.appendChild(span);
					li.appendChild(text);
					ul.appendChild(li);
				}
				helpDiv.appendChild(toggleDiv);
				helpDiv.appendChild(ul);

				var versionDiv = document.createElementNS(HTML_NAMESPACE,'div');
				var range = document.createRange();
				range.selectNodeContents(document.body);
				var df = range.createContextualFragment([
					 '<p><a href="' + URL + '">oAutoPagerize</a> ver. ' + VERSION + '</p>'
					,'<p><a href="' + UPDATE_URL + '">UPDATE SITEINFO</a></p>'
				].join('\n'));
				versionDiv.appendChild(df);
				helpDiv.appendChild(versionDiv);
				icon.appendChild(helpDiv);
				this.helpLayer = helpDiv;
			}
			,onScroll:function() {
				if (!this.wait && !this.checkRemain()) {
					this.wait = true;
					var self = this;
					setTimeout(function(){
						self.checkRemain();
						self.wait = false;
					},500);
				}
			}
			,checkRemain:function() {
				var remain = docRoot.scrollHeight - window.innerHeight - window.pageYOffset;
				if (this.state && remain < this.remainHeight) {
					this.request();
					return true;
				}
				return false;
			}
			,stateToggle:function() {
				if (this.state) {
					this.disable();
				} else {
					this.enable();
				}
			}
			,enable:function() {
				this.state = true;
				this.icon.style.background = COLOR['on'];
				this.icon.style.opacity = 0.8;
			}
			,disable:function() {
				this.state = false;
				this.icon.style.background = COLOR['off'];
				this.icon.style.opacity = 0.5;
			}
			,request:function() {
				if (!this.requestURL || this.lastRequestURL == this.requestURL) return;
				if (this.requestURL.indexOf('http') != 0) {
					this.requestURL = pathToURL(this.requestURL);
				}
				this.lastRequestURL = this.requestURL;
				this.showLoading(true);
				if (useIframe || window.opera && !window.localStorage && ( !window.postMessage || 'utf-8' != document.characterSet ) ) {
					this.frameRequest();
				} else {
					this.httpRequest();
				}
			}
			,httpRequest:function() {
				var self = this;
				try {
					var x = new XMLHttpRequest();
					x.open('GET',this.requestURL,true);
					x.onload = function() {
						self.requestLoad.call(self, x);
					};
					x.onerror = function(){
						if (window.opera) {
							self.frameRequest();
						} else {
							self.error(x);
						}
					};
					log('XMLHttpRequest: url='+this.requestURL);
					x.overrideMimeType('text/html; charset=' + document.characterSet);
					x.send(null);
				} catch (e) {
					if (window.opera) {
						self.frameRequest();
					} else {
						log('message: '+e.message);
						this.error();
						return;
					}
				}
			}
			,frameRequest:function() {
				var iframe = document.createElementNS(HTML_NAMESPACE, 'iframe');
				var self = this;
				this.removeIframe = function(){
					document.body.removeChild(iframe);
				};
				this._frameLoad = function(){
					self.frameLoad.call(self,iframe);
				};
				iframe.width = iframe.height = 1;
				iframe.style.visibility = 'hidden';
				iframe.name = 'oAutoPagerizeRequest';
				iframe.src = this.requestURL;
				document.body.appendChild(iframe);
			}
			,showLoading:function(sw) {
				if (sw) {
					this.icon.style.background = COLOR['loading'];
				} else {
					this.icon.style.background = COLOR['on'];
				}
			}
			,frameLoad:function(frame) {
				var htmlDoc = frame.contentDocument || frame.contentWindow.document;
				this.loaded(htmlDoc);
			}
			,requestLoad:function(res) {
				log('requestLoad\n'+'status: '+res.statusText);
				var t = res.responseText;
				var htmlDoc = createHTMLDocumentByString(t);
				this.createHTMLDocumentMode = true;
				this.loaded(htmlDoc);
			}
			,loaded:function(htmlDoc) {
				AutoPager.documentFilters.forEach(function(i) {
					i(htmlDoc, this.requestURL, this.info);
				}, this);
				log('response document: '+htmlDoc);
				try {
					var pages = getElementsByXPath(this.info.pageElement, htmlDoc);
					var url = this.getNextURL(this.info.nextLink, htmlDoc);
					if (!!this.info.incremental) {
						var exp = new RegExp(this.info.incremental.nextMatch,'i');
						var _m = this.info.incremental.nextLink;
						var step = this.info.incremental.step || 1;
						url = this.requestURL.replace(exp,function(m0,m1){
							var n = parseInt(m1,10) + step;
							return _m.split('#').join(n);
						});
					}
				} catch(e) {
					log('error at AutoPager:requestLoad() : '+e);
					log('code: '+e.code);
					this.error();
					return;
				}
				if (!pages.length) {
					log('requestLoad abort: pages:'+pages);
					this.terminate();
					return;
				}
				if (this.loadedURLs[this.requestURL]) {
					log('requestLoad abort: already loadpage:' + this.requestURL);
					this.terminate();
					return;
				}
				pages.forEach(function(page, i) {
					pages[i] = document.importNode(page, true);
				});
				this.loadedURLs[this.requestURL] = true;
				this.addPage(htmlDoc, pages);
				AutoPager.filters.forEach(function(i) {
					i(pages);
				});
				this.requestURL = url;
				this.showLoading(false);
				if (!url) {
					log('requestLoad abort: next URL is empty');
					this.terminate();
				}
				var ev = document.createEvent('Event');
				ev.initEvent('GM_AutoPagerizeNextPageLoaded', true, false);
				document.dispatchEvent(ev);
			}
			,addPage:function(htmlDoc, pages) {
				var insertParentNode = this.insertPoint.parentNode;
				var root,node;
				if (/^tbody$/i.test(insertParentNode.localName)) {
					var colNodes = getElementsByXPath('child::tr[1]/child::*[self::td or self::th]',insertParentNode);
					var colums = 0;
					for (var i = 0, l = colNodes.length;i<l;i++) {
						var col = colNodes[i].getAttribute('colspan');
						colums += parseInt(col,10) || 1;
					}
					node = document.createElementNS(HTML_NAMESPACE, 'td');
					root = document.createElementNS(HTML_NAMESPACE, 'tr');
					node.setAttribute('colspan',colums);
					root.appendChild(node);
				} else {
					root = node = document.createElementNS(HTML_NAMESPACE, 'div');
				}
				node.className = 'autopagerize_page_separator_blocks';
				var tmpl = {
					number:'<hr style="clear:both;" class="autopagerize_page_separator"/><p class="autopagerize_page_info">page: <a class="autopagerize_link" href="%s">%n</a></p>'
					,link:'<hr style="clear:both;" class="autopagerize_page_separator"/><p class="autopagerize_page_info">AutoPagerized: <a class="autopagerize_link" href="%s">%s</a></p>'
				};
				var range = document.createRange();
				range.selectNodeContents(document.body);
				var df = range.createContextualFragment((tmpl[naviType]||tmpl['number']).replace(/%s/g, this.requestURL).replace(/%n/g,++this.pageNum));
				node.appendChild(df);
				insertParentNode.insertBefore(root, this.insertPoint);
				var self = this;
				if (!this.nodeInsert) (function(){
					if (window.opera) {
						var dummy = document.createEvent('MutationEvents');
						dummy.initMutationEvent('hogehoge', true, false, insertParentNode, null, null, null, null);
						if (!dummy.relatedNode) { // MutationEvents��䎚�����𨰻�卝���硔�����烐���蝵�
							self.nodeInsert = function(node) {
								var insertNode = insertParentNode.insertBefore(node, self.insertPoint);
								var ev = document.createEvent('Event');
								ev.initEvent('AutoPagerize_DOMNodeInserted', true, false);
								ev.relatedNode = insertParentNode;
								ev.newValue = self.requestURL;
								insertNode.dispatchEvent(ev);
							};
							return;
						}
					}
					self.nodeInsert = function(node) {
						var insertNode = insertParentNode.insertBefore(node, self.insertPoint);
						var ev = document.createEvent('MutationEvents');
						ev.initMutationEvent('AutoPagerize_DOMNodeInserted', true, false, insertParentNode, null, self.requestURL, null, null);
						insertNode.dispatchEvent(ev);
					};
				})();
				pages.forEach(this.nodeInsert);
			}
			,getNextURL:function(xpath, doc) {
				var next = getFirstElementByXPath(xpath, doc);
				if (next) {
					return next.getAttribute('href') || next.getAttribute('action') || next.getAttribute('value');
				}
			}
			,terminate:function() {
				log('terminating');
				this.icon.style.background = COLOR['terminated'];
				window.removeEventListener('scroll', this.scroll, true);
				var self = this;
				setTimeout(function() {
					self.icon.parentNode.removeChild(self.icon);
				}, 1500);
			}
			,error:function() {
				this.icon.style.background = COLOR['error'];
				window.removeEventListener('scroll', this.scroll, true);
			}
		};
		AutoPager.documentFilters = [];
		AutoPager.filters = [];

		if (FORCE_TARGET_WINDOW) {
			var setTargetBlank = function(doc) {
				var anchers = getElementsByXPath('descendant-or-self::a[@href and not(contains(@class, "autopagerize_link") or starts-with(@href, "javascript:") or starts-with(@href, "#"))]', doc);
				anchers.forEach(function(i) {
					i.target = TARGET_WINDOW_NAME;
				});
			};
			AutoPager.documentFilters.push(setTargetBlank);
		}

		var launchAutoPager = function(list) {
			log('launchAutoPager(...)');
			var s = (new Date).getTime();
			list.some(function(info) {
				try {
					var exp = new RegExp(info.url);
					if (ap) {
					} else if ( ! exp.test(locationHref) ) {
					} else if (!getFirstElementByXPath(info.nextLink)) {
						// ignore microformats case.
						if (!exp.test('http://a')) {
							debug('nextLink not found.', info);
						}
					} else if (!getFirstElementByXPath(info.pageElement)) {
						if (!exp.test('http://a')) {
							debug('pageElement not found.', info);
						}
					} else {
						ap = new AutoPager(info);
						window.AutoPagerize.AutoPagerObject = ap;
						return true;
					}
				} catch(e) {
					log('error at launchAutoPager() : ' + e);
				}
			});
			log('launchAutoPager... fin:'+((new Date).getTime()-s));
			MICROFORMATs.forEach(function(format){
				if (!ap && getFirstElementByXPath(format.nextLink) && getFirstElementByXPath(format.pageElement)) {
					ap = new AutoPager(format);
					window.AutoPagerize.AutoPagerObject = ap;
				}
			});
		};

		// initialize
		if (!window.AutoPagerize) {
			window.AutoPagerize = {
				addFilter:function(f) {
					AutoPager.filters.push(f);
				}
				,addDocumentFilter:function(f) {
					AutoPager.documentFilters.push(f);
				}
			};
		}
		var ev = document.createEvent('Event');
		ev.initEvent('GM_AutoPagerizeLoaded', true, false);
		document.dispatchEvent(ev);

		var ap = null;
		launchAutoPager(TOP_SITEINFO.concat(SITEINFO,BOTTOM_SITEINFO));

		// utility functions.
		window.AutoPagerize.getElementsByXPath = getElementsByXPath;
		window.AutoPagerize.getFirstElementByXPath = getFirstElementByXPath;

		function createHTMLDocumentByString(str) {
			if (document.documentElement.nodeName != 'HTML') {
				return new DOMParser().parseFromString(str, 'application/xhtml+xml');
			}
			var html = String(str);// Thx! jAutoPagerize#HTMLResource.createDocumentFromString http://svn.coderepos.org/share/lang/javascript/userscripts/jautopagerize.user.js
			html = html.replace(/<script(?:[ \t\r\n][^>]*)?>[\S\s]*?<\/script[ \t\r\n]*>|<\/?(?:i?frame|html|script|object)(?:[ \t\r\n][^<>]*)?>/gi, ' ');
			if (isSafari3) return createDocumentFromString(html);
			return createDocumentFromString(html);
		}
		function createDocumentFromString(str) {
			getXPathResult.forceRelative = true;
			var d = document.createElement('div');
			d.appendChild(createDocumentFragmentByString(str));
			return d;
		}

		// via http://github.com/hatena/hatena-bookmark-xul/blob/master/chrome/content/common/05-HTMLDocumentCreator.js
		function createDocumentFromString(source){
			var doc;
			if (document.implementation.createHTMLDocument) {
				doc = document.implementation.createHTMLDocument('title');
			} else {
				doc = document.cloneNode(false);
				if (doc) {
					doc.appendChild(doc.importNode(document.documentElement, false));
				} else {
					doc = document.implementation.createDocument(null, 'title', null);
				}
			}
			var range = document.createRange();
			range.selectNodeContents(document.documentElement);
			var fragment = range.createContextualFragment(source);
			var headChildNames = {title: true, meta: true, link: true, script: true, style: true, /*object: true,*/ base: true/*, isindex: true,*/};
			var child, head = doc.getElementsByTagName('head')[0] || doc.createElement('head'),
			           body = doc.getElementsByTagName('body')[0] || doc.createElement('body');
			while ((child = fragment.firstChild)) {
				if (
					(child.nodeType === doc.ELEMENT_NODE && !(child.nodeName.toLowerCase() in headChildNames)) ||
					(child.nodeType === doc.TEXT_NODE &&/\S/.test(child.nodeValue))
				   )
					break;
				head.appendChild(child);
			}
			body.appendChild(fragment);
			doc.documentElement.appendChild(head);
			doc.documentElement.appendChild(body);
			return doc;
		}

		function debug(message, siteinfo) {
			if (!DebugMode) return;
			var params = (function(site){
				var p = [];
				for (var k in site) {
					if (k == 'data') {
						var data = site[k];
						for (var j in data) {
							p.push(j+':'+data[j]);
						}
					} else {
						p.push(k+':'+site[k]);
					}
				}
				return p.join(',');
			})(siteinfo);
			log(message,params);
		}

		function log() {
			if (!DebugMode) return;
			if (window.console) {
				console.log(Array.prototype.slice.call(arguments));
			} else if (window.opera && window.opera.postError) {
				window.opera.postError(arguments);
			}
		}

		function getElementPosition(elem) {
			var offsetTrail = elem;
			var offsetLeft  = 0;
			var offsetTop   = 0;
			while (offsetTrail) {
				offsetLeft += offsetTrail.offsetLeft;
				offsetTop  += offsetTrail.offsetTop;
				offsetTrail = offsetTrail.offsetParent;
			}
			offsetTop = offsetTop || null;
			offsetLeft = offsetLeft || null;
			return {left: offsetLeft, top: offsetTop};
		}

		function getElementBottom(elem) {
			var c_style = document.defaultView.getComputedStyle(elem, '');
			var height  = 0;
			var prop    = [
				'height', 'borderTopWidth', 'borderBottomWidth',
				'paddingTop', 'paddingBottom',
				'marginTop', 'marginBottom'];
			prop.forEach(function(i) {
				var h = parseInt(c_style[i]);
				if (typeof h == 'number') {
					height += h;
				}
			});
			var top = getElementPosition(elem).top;
			return top ? (top + height) : null;
		}
		function pathToURL(path) {
			var link = document.createElement('a');
			link.href = path;
			return link.href;
		}
		function getElementsByXPath(xpath, node) {
			var nodesSnapshot = getXPathResult(xpath, node, XPathResult.ORDERED_NODE_SNAPSHOT_TYPE);
			var data = [];
			for (var i = 0; i < nodesSnapshot.snapshotLength; i++) {
				data.push(nodesSnapshot.snapshotItem(i));
			}
			return data;
		}
		function getFirstElementByXPath(xpath, node) {
			var result = getXPathResult(xpath, node, XPathResult.FIRST_ORDERED_NODE_TYPE);
			return result.singleNodeValue;
		}
		function getXPathResult(xpath, node, resultType) {
			if (getXPathResult.forceRelative) {
				xpath = xpath.replace(/id\(\s*(["'])([^"']+)\1\s*\)/g, './/*[@id="$2"]');
				xpath = xpath.indexOf('(//') == 0
					? '(.//' + xpath.substring(3)
					: (xpath[0] == '/' ? '.' : './') + xpath;
			}
			var node = node || document;
			var doc = node.ownerDocument || node;
			var resolver = doc.createNSResolver(node.documentElement || node);
			// Use |node.lookupNamespaceURI('')| for Opera 9.5
			var isXHTML = document.documentElement.tagName !== 'HTML';
			var defaultNS = node.lookupNamespaceURI(window.opera ? '' : null);
			if (isXHTML) {
				var defaultPrefix = '__default__';
				xpath = addDefaultPrefix(xpath, defaultPrefix);
				var defaultResolver = resolver;
				resolver = function (prefix) {
					return (prefix == defaultPrefix) ? defaultNS : defaultResolver.lookupNamespaceURI(prefix);
				}
			}
			return doc.evaluate(xpath, node, resolver, resultType, null);
		}
		function addDefaultPrefix(xpath, prefix) {
			var tokenPattern = /([A-Za-z_\u00c0-\ufffd][\w\-.\u00b7-\ufffd]*|\*)\s*(::?|\()?|(".*?"|'.*?'|\d+(?:\.\d*)?|\.(?:\.|\d+)?|[\)\]])|(\/\/?|!=|[<>]=?|[\(\[|,=+-])|([@$])/g;
			var TERM = 1, OPERATOR = 2, MODIFIER = 3;
			var tokenType = OPERATOR;
			prefix += ':';
			function replacer(token, identifier, suffix, term, operator, modifier) {
				if (suffix) {
					tokenType =
						(suffix == ':' || (suffix == '::' && (identifier == 'attribute' || identifier == 'namespace')))
						? MODIFIER : OPERATOR;
				} else if (identifier) {
					if (tokenType == OPERATOR && identifier != '*') {
						token = prefix + token;
					}
					tokenType = (tokenType == TERM) ? OPERATOR : TERM;
				} else {
					tokenType = term ? TERM : operator ? OPERATOR : MODIFIER;
				}
				return token;
			}
			return xpath.replace(tokenPattern, replacer);
		}
		function addCSS (css){
			var style = document.createElementNS(HTML_NAMESPACE, 'style');
			style.id = 'autopagerize_style';
			style.type = 'text/css';
			var root = document.getElementsByTagName('head')[0] || document.body;
			root.appendChild(style);
			addCSS = function(_css){
				style.appendChild(document.createTextNode(_css+'\n'));
			};
			addCSS(css);
		}
	};
	function insertSITEINFO(){
		var sc = document.createElementNS(HTML_NAMESPACE, 'script');
		sc.type = 'text/javascript';
		window.AutoPagerizeCallbackSiteinfo = function(res){
			window.AutoPagerizeCallbackSiteinfo = null;
			SITEINFO = res;
			var ev = document.createEvent('Event');
			ev.initEvent('AutoPagerize_SiteinfoLoaded', true, false);
			document.dispatchEvent(ev);
			sc.parentNode.removeChild(sc);
		};
		sc.src = SITEINFO_SERVER;
		if (window.opera && (document.readyState === 'loading' || document.readyState === 'interactive')) {
			document.addEventListener('DOMContentLoaded',insert,false);
		} else {
			insert();
		}
		function insert(){
			document.body.appendChild(sc);
		}
	}
	if (window.opera && (document.readyState === 'loading' || document.readyState === 'interactive')) {
		document.addEventListener('DOMContentLoaded', autopager, false);
	} else {
		autopager();
	}
	document.addEventListener('AutoPagerize_SiteinfoLoaded', autopager, false);
})(window);