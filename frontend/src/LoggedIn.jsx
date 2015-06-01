var React = require('react');
var request = require('superagent');

var LoggedIn = React.createClass({
	trySomething: function() {
		request.post('/pins/add')
			.send({"url": "http://example.com", "title": "Example dot com"})
			.end(function(res) {
				console.log("res", res);
			});
		console.log('yes');
	},
	render: function() {
		return (<div>
			<div>You are {this.props.user}</div>
			<a onClick={this.trySomething}>Try something?</a>
		</div>);
	}
});

module.exports = LoggedIn;

