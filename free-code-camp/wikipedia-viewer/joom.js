var wv = {};

function clog(message) {
    console.log(message);
}

function searchResults(data) {
    return data.query.search;
}

function resultTitle(searchResult) {
    return searchResult.title;
}

function resultSnip(searchResult) {
    return searchResult.snippet;
}

// Generates HTML to display the user's search results.
function buildResults(data) {
    let results = searchResults(data);
    $("#search-results").html('');

    for (i = 0; i < 10; i += 1) {
	let result = results[i],
	    title = resultTitle(result),
	    snip = resultSnip(result);
	$("#search-results").append('<div class="result">' +
				    '<a href="https://en.wikipedia.org/wiki/' + title + '">' +
				    '<div class="search-title">' + title + '</div>' +
				    '<div class="search-snip">' + snip + '...</div' +
				    '</a>' +
				    '</div>');
    }
}

$(document).ready(function() {
    
    $("#search").on("click", function() {
	let input = $("#search-form").val(),
	    wikipediaAPI = 'http://en.wikipedia.org/w/api.php?action=query&format=json&list=search&origin=*&srsearch=' + input;

	if (input !== "") {
	    $.getJSON(wikipediaAPI, function(data) {
		buildResults(data);
	    });
	}

	// Don't jump to the top of the page on-click.
	return false;
    });
});
