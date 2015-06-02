var React = require('react');

var CreatePost = React.createClass({
	getInitialState: function() {
		return { "url": "", "title": "" };
	},
	makePost: function() {
		this.props.makePost(this.state.url, this.state.title);
	},

	setUrl: function(event) {
		this.setState({"url": event.target.value});
	},

	setTitle: function(event) {
		this.setState({"title": event.target.value});
	},

	render: function() {
		return (<div>
			<input type="url" onChange={this.setUrl} />
			<input type="text" onChange={this.setTitle} />
			<a onClick={this.makePost}>Send</a>
		</div>);
	}
});

module.exports = CreatePost;


