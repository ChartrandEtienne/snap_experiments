var React = require('react');
var LoggedOut = require('./LoggedOut.jsx');
var LoggedIn = require('./LoggedIn.jsx');

var BaseComponent = React.createClass({
	render: function() {
		if ("" === this.props.user) {
			return (<LoggedOut />);
		} else {
			return (<LoggedIn user={this.props.user} />);
		}
	}
});

module.exports = BaseComponent;
