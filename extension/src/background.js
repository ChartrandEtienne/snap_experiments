var request = require('superagent');

function getCurrentTabUrl(callback) {
	var queryInfo = {
		active: true,
		currentWindow: true
	};
	chrome.tabs.getSelected(null, function(tab) {
		var url = tab.url;
		console.assert(typeof url == 'string', 'tab.url should be a string');
		callback(url);
	});
}

/*
getCurrentTabUrl(function(url) {
	console.log("URGH: " + url);
	request
		.post('http://127.0.0.1:8000/visit/add')
		.withCredentials()
		.send({url: url})
		.set('Userid', 'varg')
		.end(function(err, res) {
		});
});
*/
