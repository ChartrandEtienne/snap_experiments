var React = require('react');
var request = require('superagent');

var UrlList = React.createClass({
	render: function() {
		return (<ul>
			{ this.props.urls.map(function(url) {
				return (<ul><a href={url.url}>{url.url}</a></ul>);
			})};
		</ul>);
	}
});

var DisplayStuff = React.createClass({
	render: function() {
		return (
			<p><UrlList urls={this.props.okay} /></p>
		)
	}
});



document.addEventListener('DOMContentLoaded', function() {
	chrome.tabs.getSelected(null,function(tab) {
	    var tablink = tab.url;
		var hook = document.getElementById('uh');
		request
			.get('http://127.0.0.1:8000/visit/search')
			.withCredentials()
			.query({url: tablink})
			.set('Userid', 'varg')
			.end(function(err, res) {
				React.render(<DisplayStuff okay={res.body} />, hook);
			});
	});

});

/*
chrome.runtime.onMessage.addListener(function(request, sender, sendResponse) {
	var div = document.createElement('div');
	div.innerText = 'okay';
	document.getElementsByTagName('body')[0].appendChild(div);
	sendResponse({farewell: "goodbye"});
});
*/
