var React = require('react');

var PostDisplay = React.createClass({
	render: function() {
		return (<div>
			<div><a href={this.props.post.url}>{this.props.post.title}</a></div>
		</div>);
	}
});

module.exports = PostDisplay;

