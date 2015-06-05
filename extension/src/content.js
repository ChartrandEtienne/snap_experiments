console.log("hey! ");

function myOnclick(event) {
}

// window.addEventListener('load', function() {
// document.addEventListener('DOMContentLoaded', function() {
	for (var element in document.getElementsByTagName('a')) {
		element.onclick = myOnclick;
	}
// });
//


var request = require('superagent');

var url = window.location.href;

request
	.post('http://127.0.0.1:8000/visit/add')
	.withCredentials()
	.send({url: url})
	.set('Userid', 'varg')
	.end(function(err, res) {
	});


