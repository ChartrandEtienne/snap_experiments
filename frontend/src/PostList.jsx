var React = require('react');
var PostDisplay = require('./PostDisplay.jsx');

var PostList = React.createClass({
	render: function() {
		return (<div>
			{this.props.posts.map(function(post) {
				return (<PostDisplay post={post} />)
			})}
		</div>);
	}
});

module.exports = PostList;

