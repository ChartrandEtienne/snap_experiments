var React = require('react');

var LoggedIn = React.createClass({
	render: function() {
		return (<div>You are {this.props.user}</div>);
	}
});

module.exports = LoggedIn;

