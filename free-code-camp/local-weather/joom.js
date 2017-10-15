// how can I store data from an async function? / not have to stuff everything in one place

var wp = {};

function clog(message) {
    console.log(message);
}

// Convert degrees celsius to fahrenheit.
function c2f(celsius) {
    return parseFloat(((celsius * (9 / 5)) + 32).toFixed(2));
}

function tempSwitchHTML(form) {// having html here makes me nervous
    return ' <a id="temp-switch" href="#">' + form + '</a>';
}

// Get the user's position, then see what their weather's like.
function getWeather(form="C") {
    // Accessors for the JSON
    function lat(lData) {
	return lData.lat;
    }
    function lon(lData) {
	return lData.lon;
    }

    function location(lData) {
	return lData.city + ", " + lData.regionName;
    }

    function temperature(wData) {
	if (form == "C") {
	    return wData.main.temp + tempSwitchHTML("C");
	}
	else {
	    return c2f(wData.main.temp) + tempSwitchHTML("F");
	}
	//return wData.main.temp + tempSwitchHTML("C");
    }

    function weather(wData) {
	return wData.weather[0].description;
    }

    function weatherIcon(wData) {
	return wData.weather[0].icon;
    }

    // Change the page's content, based on data from geo/weather apis.
    (function main() {
	let locationAPI = "http://ip-api.com/json/?callback=?";

	$.getJSON(locationAPI, function(lData) {
	    let weatherAPI = "http://fcc-weather-api.glitch.me/api/current?lat=" + lat(lData) + "&lon=" + lon(lData);

	    $.getJSON(weatherAPI, function(wData) {
		$('#location').html(location(lData));
		$('#temperature').html(temperature(wData));
		$('#weather').html(weather(wData));
		// The FCC API doesn't seem to have icons for every weather condition.
		$('#weather-icon').attr('src', weatherIcon(wData));
	    });
	});
    })();
}

$(document).ready(function() {
    getWeather();

    let isC = true;

    $(document).on('click', '#temp-switch', function() {
	if (isC === true) {
	    getWeather("F");
	    isC = false;
	}
	else {
	    getWeather();
	    isC = true;
	}
	
	// Don't jump to the top of the page on click.
	return false;
    });
});
