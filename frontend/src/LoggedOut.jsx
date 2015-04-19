var React = require('react');

var LoggedOut = React.createClass({
	render: function() {
		return (<form method='post' action='/login'>Who is you<br/><input type='text' name='user'/><input type='submit' value='Submit'/></form>);
	}
});

module.exports = LoggedOut;
