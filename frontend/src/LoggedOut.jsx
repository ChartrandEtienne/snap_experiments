var React = require('react');

var LoggedOut = React.createClass({
	render: function() {
		return (<div>
			<form method='post' action='/login'>
				login<br/>
				<input type='text' name='login'/>
				<input type='password' name='password'/>
				<input type='submit' value='Submit' />
			</form>

			<form method='post' action='/register'>
				register<br/>
				<input type='text' name='login'/>
				<input type='password' name='password'/>
				<input type='submit' value='Submit' />
			</form>
		</div>);
	}
});

module.exports = LoggedOut;
