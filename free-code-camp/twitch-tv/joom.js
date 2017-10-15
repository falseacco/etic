Ttv = {};

function clog(message = "") {
    console.log(message);
}

Ttv.exampleJSON = [
    {
	"stream": {
	    "mature": false,
	    "status": "Greg working on Electron-Vue boilerplate w/ Akira #programming #vuejs #electron",
	    "broadcaster_language": "en",
	    "display_name": "FreeCodeCamp",
	    "game": "Creative",
	    "language": "en",
	    "_id": 79776140,
	    "name": "freecodecamp",
	    "created_at": "2015-01-14T03:36:47Z",
	    "updated_at": "2016-09-17T05:00:52Z",
	    "delay": null,
	    "logo": "https://static-cdn.jtvnw.net/jtv_user_pictures/freecodecamp-profile_image-d9514f2df0962329-300x300.png",
	    "banner": null,
	    "video_banner": "https://static-cdn.jtvnw.net/jtv_user_pictures/freecodecamp-channel_offline_image-b8e133c78cd51cb0-1920x1080.png",
	    "background": null,
	    "profile_banner": "https://static-cdn.jtvnw.net/jtv_user_pictures/freecodecamp-profile_banner-6f5e3445ff474aec-480.png",
	    "profile_banner_background_color": null,
	    "partner": false,
	    "url": "https://www.twitch.tv/freecodecamp",
	    "views": 161989,
	    "followers": 10048,
	    "_links": {
		"self": "https://api.twitch.tv/kraken/channels/freecodecamp",
		"follows": "https://api.twitch.tv/kraken/channels/freecodecamp/follows",
		"commercial": "https://api.twitch.tv/kraken/channels/freecodecamp/commercial",
		"stream_key": "https://api.twitch.tv/kraken/channels/freecodecamp/stream_key",
		"chat": "https://api.twitch.tv/kraken/chat/freecodecamp",
		"subscriptions": "https://api.twitch.tv/kraken/channels/freecodecamp/subscriptions",
		"editors": "https://api.twitch.tv/kraken/channels/freecodecamp/editors",
		"teams": "https://api.twitch.tv/kraken/channels/freecodecamp/teams",
		"videos": "https://api.twitch.tv/kraken/channels/freecodecamp/videos"
	    }
	},
	"_links": {
	    "self": "https://api.twitch.tv/kraken/streams/freecodecamp",
	    "channel": "https://api.twitch.tv/kraken/channels/freecodecamp"
	}
    },
    {
	"stream": null,
	"display_name": "OgamingSC2",
	"_links": {
	    "self": "https://api.twitch.tv/kraken/streams/ogamingsc2",
	    "channel": "https://api.twitch.tv/kraken/channels/ogamingsc2"
	}
    },
    {
	"stream": {
	    "mature": false,
	    "status": "RERUN: StarCraft 2 - Kane vs. HuK (ZvP) - WCS Season 3 Challenger AM - Match 4",
	    "broadcaster_language": "en",
	    "display_name": "ESL_SC2",
	    "game": "StarCraft II",
	    "language": "en",
	    "_id": 30220059,
	    "name": "esl_sc2",
	    "created_at": "2012-05-02T09:59:20Z",
	    "updated_at": "2016-09-17T06:02:57Z",
	    "delay": null,
	    "logo": "https://static-cdn.jtvnw.net/jtv_user_pictures/esl_sc2-profile_image-d6db9488cec97125-300x300.jpeg",
	    "banner": null,
	    "video_banner": "https://static-cdn.jtvnw.net/jtv_user_pictures/esl_sc2-channel_offline_image-5a8657f8393c9d85-1920x1080.jpeg",
	    "background": null,
	    "profile_banner": "https://static-cdn.jtvnw.net/jtv_user_pictures/esl_sc2-profile_banner-f8295b33d1846e75-480.jpeg",
	    "profile_banner_background_color": "#050506",
	    "partner": true,
	    "url": "https://www.twitch.tv/esl_sc2",
	    "views": 60843789,
	    "followers": 135275,
	    "_links": {
		"self": "https://api.twitch.tv/kraken/channels/esl_sc2",
		"follows": "https://api.twitch.tv/kraken/channels/esl_sc2/follows",
		"commercial": "https://api.twitch.tv/kraken/channels/esl_sc2/commercial",
		"stream_key": "https://api.twitch.tv/kraken/channels/esl_sc2/stream_key",
		"chat": "https://api.twitch.tv/kraken/chat/esl_sc2",
		"subscriptions": "https://api.twitch.tv/kraken/channels/esl_sc2/subscriptions",
		"editors": "https://api.twitch.tv/kraken/channels/esl_sc2/editors",
		"teams": "https://api.twitch.tv/kraken/channels/esl_sc2/teams",
		"videos": "https://api.twitch.tv/kraken/channels/esl_sc2/videos"
	    }
	},
	"_links": {
	    "self": "https://api.twitch.tv/kraken/streams/esl_sc2",
	    "channel": "https://api.twitch.tv/kraken/channels/esl_sc2"
	}
    },
    {
	"stream": null,
	"display_name": "noobs2ninjas",
	"_links": {
	    "self": "https://api.twitch.tv/kraken/streams/esl_sc2",
	    "channel": "https://api.twitch.tv/kraken/channels/esl_sc2"
	}
    },
    {
	"error": "Not Found",
	"status": 404,
	"message": "Channel 'not-a-valid-account' does not exist"
    }
]

// Various accessors for above JSON.
function getStream(stream) {
    return stream.stream;
}

function streamCase(stream, ifUndefined, ifNull, ifElse) {
    let innerStream = getStream(stream);

    if (innerStream === undefined) {
	return ifUndefined(innerStream);
    }
    else if (innerStream === null) {
	return ifNull(innerStream);
    }
    else {
	return ifElse(innerStream);
    }
}

function getName(stream) {
    return streamCase(stream,
		      function() {return "";},
		      function() {return stream.display_name;},
		      function(inner) {return inner.display_name;});
}

function getStatus(stream) {
    return streamCase(stream,
		      function() {return "";},
		      function() {return "Offline";},
		      function(inner) {return inner.status;});
}

function getLogo(stream) {
    let dummy = 'https://dummyimage.com/50x50/ecf0e7/5c5457.jpg&text=0x3F';
    
    return streamCase(stream,
		      function() {return "";},
		      function () {return stream.logo || dummy;},
		      function(inner) {return inner.logo || dummy;});
}

// Modify the page's content.
// _BUILDs just return some HTML as a string.
// _HTMLs modify the document itself.

function tag(tag, properties, content) {
    if (properties) {
	properties = ' ' + properties;
    }

    return '<' + tag + properties + '>' + content + '</' + tag + '>';
}

function name_BUILD(name) {
    return name === ''?'':tag('a', 'class="stream-title" href="https://www.twitch.tv/' + name + '"', name);
}

function status_BUILD(status) {
    return status === ''?'':tag('text', 'class="stream-status"', status);
}

function logo_BUILD(logo) {
    return logo === ''?'':tag('img', 'class="stream-logo" src=' + logo, '');
}

function stream_BUILD(stream) {
    let divClass = getStatus(stream) === 'Offline'?'dead-stream':'live-stream';
    
    return tag('div', 'class="stream ' + divClass + '"', logo_BUILD(getLogo(stream)) + name_BUILD(getName(stream)) + status_BUILD(getStatus(stream)));
}

function streamers_HTML(data) {
    for (let index in data) {
	let stream = data[index];

	$("#streamers").append(stream_BUILD(stream)); // .data?
    }
}

//

$(document).ready(function() {

    streamers_HTML(Ttv.exampleJSON);

    clog(0);
});
