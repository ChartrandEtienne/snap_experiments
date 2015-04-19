var React = require('react');
var BaseComponent = require('./BaseComponent.jsx');

// <form method='post' action='/login'>Who is you<br><input type='text' name='user'><input type='submit' value='Submit'></form>

React.render(<BaseComponent user={window.user}/>, document.getElementById("react_hook"));
