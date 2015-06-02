var React = require('react');
var request = require('superagent');
var PostList = require('./PostList.jsx');
var CreatePost = require('./CreatePost.jsx');

var LoggedIn = React.createClass({

	getInitialState: function() {
		return {
			posts: [],
			create: false
		}
	},

	makePost: function(url, title) {
		request.post('/pins/add')
			.send({"url": url, "title": title})
			.end(function(res) {
				this.getPosts();
			}.bind(this));
	},

	getPosts: function() {
		request.get('/pins')
			.set('Accept', 'application/json')
			.end(function(err, res) {
				console.log("res", res);
				this.setState({posts: res.body});
			}.bind(this));
	},

	componentDidMount: function() {
		this.getPosts();
	},

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
			<PostList posts={this.state.posts} />
			<CreatePost makePost={this.makePost} />
		</div>);
	}
});

module.exports = LoggedIn;

