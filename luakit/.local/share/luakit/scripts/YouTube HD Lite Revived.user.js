// ==UserScript==
// @name		YouTube HD Lite Revived
// @namespace	http://userscripts.org/users/124528
// @description	Automatically plays the best quality YouTube video format and removes ads, plus several optional features
// @grant		GM_log
// @include		http://www.youtube.com/watch*
// @include		https://www.youtube.com/watch*
// @include		http://www.youtube.com/user/*
// @include		https://www.youtube.com/user/*
// @version		2013-10-12
// ==/UserScript==

function main() {

	// CONFIGURABLE OPTIONS (see http://userscripts.org/scripts/show/124528 for more info)
	var use1080p = true,
	use720p = true,
	wideVideo = true,
	wideSize = "medium",
	hideAnnotations = false,
	scrollToVideo = true,
	autoPlay = true,
	autoPlayChan = false,
	skipAds = true,
	hideControls = true;

	// DO NOT EDIT BELOW THIS LINE
	
	var debug = false;

	// ensure proper Youtube URL on normal watch pages
	if ( location.href.search( "watch#!" ) != -1 ) {
		var url = location.href.split( "watch#!" );
		url = url[0] + "watch?" + url[1];
		window.open( url, "_self" );
	}
	
	if ($("watch-headline-title") || $("vt")) {
		var pageType = "watch";
	}
	else if ($("gh-banner")) {
		var pageType = "chan";
	}

	if (pageType == "watch") {
		if ( scrollToVideo ) {
			if ($("page")) $("page").scrollIntoView( true );
			else if ($("vt")) $("vt").scrollIntoView( true );
		}
		if ( wideVideo ) {
			if ($("watch7-container")) {
				var ele = $("watch7-container");
				if ( ele.className.indexOf( "watch-wide" ) == -1 ) {
					ele.className += " watch-wide";
				}

				// expand the video container forcefully and constantly
				if ($("player-legacy")) var ele = $("player-legacy");
				else if ($("player")) var ele = $("player");
				var wideClass = "watch-" + wideSize;
				var wait=window.setInterval(function (){forceSize(ele, wideClass, wideSize)}, 250);
			}
		}
	}
	else if (pageType == "chan") {
		 if ($("gh-banner")) $("gh-banner").scrollIntoView( true );
	}
	
	if ( debug ) GM_log( "page type: " + pageType );

	if ($("movie_player")) var player = $("movie_player");
	else if ($("movie_player-flash")) var player = $("movie_player-flash");
	else if ($c("html5-main-video")) var player = alert($c("html5-main-video"));

	var	myPlayer = player.cloneNode( true ),
		flashvars = myPlayer.getAttribute( "flashvars" );
		
	if ( debug ) GM_log( "flashvars unmodified: " + flashvars );


	function setFlashvar( field, newVal ) 
	{
		var delimited = "&" + field;
		if ( flashvars.indexOf( delimited ) == -1 ) {
			// field not found, so append it
			flashvars += delimited + "=" + newVal;
		}
		else {
			// modify existing field
			var tmp = flashvars.split( delimited );
			var tmp2 = tmp[1].indexOf( "&" );
			if ( tmp2 != -1 ) {
				flashvars = tmp[0] + delimited + "=" + newVal + tmp[1].substr( tmp2 );
			}
			else {
				flashvars = tmp[0] + delimited + "=" + newVal;
			}
		}
	}
		

	/**********************************************
	//	 2012.03.27 list of video formats 
	// fmt=5	240p		  vq=small	 flv  mp3
	// fmt=18   360p		  vq=medium	mp4  aac
	// fmt=34   360p		  vq=medium	flv  aac
	// fmt=43   360p		  vq=medium	vp8  vorbis
	// fmt=35   480p		  vq=large	 flv  aac
	// fmt=44   480p		  vq=large	 vp8  vorbis
	// fmt=22   720p		  vq=hd720	 mp4  aac
	// fmt=45   720p		  vq=hd720	 vp8  vorbis
	// fmt=37  1080p		  vq=hd1080	mp4  aac
	// fmt=46  1080p		  vq=hd1080	vp8  vorbis
	***********************************************/

	//indexOf is faster than Regex in one off use
	var start = flashvars.search("fmt_list=");
	var end = flashvars.indexOf("&", start);
	var len = ((end != -1)?end:flashVars.length) - start; // Az: 12.05.14: If fmt_list was at the end of flashVars, this would fail. Now Fixed!
	var fmt_list = flashvars.substr(start, len);

	if ( debug ) GM_log( " fmt_list: " + fmt_list ); 

	// now must set the "vq" var so that the proper format is loaded
	if ( use1080p && (  fmt_list.search("46%2F") != -1 || fmt_list.search("37%2F") != -1 )) {
		setFlashvar( "vq", "hd1080" );
	}
	else if ( use720p && ( fmt_list.search("45%2F") != -1 || fmt_list.search("22%2F") != -1 )) {
		setFlashvar( "vq", "hd720" );
	}
	else if ( fmt_list.search("44%2F") != -1 || fmt_list.search("35%2F") != -1 ) {
		setFlashvar( "vq", "large" ); 
	}
	else {
		setFlashvar( "vq", "medium" );
	}

	setFlashvar( "enablejsapi", "1" );
	
	if (pageType == "watch") {
		if ( wideVideo ) {
			setFlashvar( "player_wide", "1")
		}
	}

	if ((pageType == "watch" && autoPlay) || (pageType == "chan" && autoPlayChan)) { 
		setFlashvar( "autoplay", "1" );
	}
	else {
		setFlashvar( "autoplay", "0" );
	}	

	if ( skipAds ) {
		var patt = /&(ad_|infringe|watermark)[^=]*=[^&]*/g;
		flashvars = flashvars.replace(patt,"");
		setFlashvar("invideo","false");
	}

	if ( hideAnnotations ) {
		setFlashvar( "iv_load_policy", "3" );
	}
	else {
		setFlashvar( "iv_load_policy", "1" );
	}

	if ( hideControls ) {
		setFlashvar( "autohide", "1" );
	}

	if ( debug ) GM_log( "flashvars final: " + flashvars );

	myPlayer.setAttribute( "flashvars", flashvars );
	
	player.parentNode.replaceChild( myPlayer, player );
	
}

//force the player to stay one size... constantly... no goingbackses also use this to check if the ID has changed :|
function forceSize(ele, wideClass, wideSize) {

	if ( ele.className.indexOf( wideClass ) == -1 ) {
		if ($("player-api-legacy")) ele = $("player-api-legacy");
		else if	($("player-api")) ele = $("player-api");
		if (wideSize == "medium") {
			if ($c("watch7-playlist-bar-left").length) {
				var ele = document.getElementById("player");
				if (ele.className.indexOf( "watch-medium" ) == -1) {
					ele.className = ele.className.replace("watch-large", "");
					ele.className = ele.className + " watch-medium";
				}
				var elems = $c("watch7-playlist-bar-left")
				for(var i = 0; i < elems.length; i++) {
					elems[i].style.width = "554px";
				}
			}
			ele.style.height = "545px";
			ele.style.width = "800px";
			$("watch7-sidebar").style.marginTop = "10px";
		}
		else if (wideSize == "large") {
			if ($c("watch7-playlist-bar-left").length) {
				var ele = document.getElementById("player");
				if (ele.className.indexOf( "watch-large" ) == -1) {
					ele.className = ele.className.replace("watch-medium", "");
					ele.className = ele.className + " watch-large";
				}
				var elems = $c("watch7-playlist-bar-left");
				for(var i = 0; i < elems.length; i++) {
					elems[i].style.width = "980px";
				}
			}
			ele.style.height = "750px";
			ele.style.width = "1280px";
			$("watch7-sidebar").style.marginTop = "10px";
		}
	}
}

function checkChanged(pageTitle, oldTitle) {
	if (pageTitle != oldTitle) {
		main();
		var oldTitle = pageTitle;
	}
	var check=window.setInterval(checkChanged(oldTitle), 250); 
}

// simplify get ID
function $(ID) {
	if (document.getElementById(ID)) return document.getElementById(ID);
}

//simplify getClass
function $c(className) {
	if (document.getElementsByClassName(className)) return document.getElementsByClassName(className);
}

//make sure page is ready before firing script
function readyCheck() {
	if($("movie_player") && $("footer-container") || $("ft")) {
		window.clearInterval(wait);
		main();
		checkChanged();
	}
	i++;
	if(i == 40) window.clearInterval(wait);
	
}
var wait=window.setInterval(readyCheck, 250, main), i=0;
