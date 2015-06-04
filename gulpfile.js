var browserify = require('browserify');
var gulp = require('gulp');
var reactify = require('reactify');
var flow = require('gulp-flowtype');
var react = require('gulp-react');
var watch = require('gulp-watch');
var watchify = require('watchify');
var source = require('vinyl-source-stream');
var gutil = require('gulp-util');

gulp.task('browserify', function() {

	var dashboard_bundler = browserify({
		entries: ["./frontend/src/Main.jsx"],
		debug: true, // Gives us sourcemapping
		cache: {}, packageCache: {}, fullPaths: true // Requirement of watchify
	}).transform('reactify', {stripTypes: true});
	var dashboard_watcher = watchify(dashboard_bundler);

	var dashboard = dashboard_watcher
	.on('update', function () {
		var updateStart = Date.now();
		console.log('Updating!');
		dashboard_watcher.bundle()
		.on('error', function(e) {
			gutil.log(gutil.colors.red('Bundle error:', e.message));
		})
// Create new bundle that uses the cache for high performance
		.pipe(source('app.js'))
		// This is where you add uglifying etc.
		.pipe(gulp.dest("./frontend/dist"));
		console.log('Updated!', (Date.now() - updateStart) + 'ms');
	})
	// Create the initial bundle when starting the task
	.bundle()
	.on('error', function(e) {
		gutil.log(gutil.colors.red('Bundle error:', e.message));
	})
	.pipe(source('app.js'))
	.pipe(gulp.dest("./frontend/dist"));


	var background_bundler = browserify({
		entries: ["./extension/src/background.js"],
		debug: true, // Gives us sourcemapping
		cache: {}, packageCache: {}, fullPaths: true // Requirement of watchify
	}).transform('reactify', {stripTypes: true});
	var background_watcher = watchify(background_bundler);

	var background = background_watcher
	.on('update', function () {
		var updateStart = Date.now();
		console.log('Updating!');
		background_watcher.bundle()
		.on('error', function(e) {
			gutil.log(gutil.colors.red('Bundle error:', e.message));
		})
// Create new bundle that uses the cache for high performance
		.pipe(source('background.js'))
		// This is where you add uglifying etc.
		.pipe(gulp.dest("./extension"));
		console.log('Updated!', (Date.now() - updateStart) + 'ms');
	})
	// Create the initial bundle when starting the task
	.bundle()
	.on('error', function(e) {
		gutil.log(gutil.colors.red('Bundle error:', e.message));
	})
	.pipe(source('background.js'))
	.pipe(gulp.dest("./extension"));

	var content_bundler = browserify({
		entries: ["./extension/src/content.js"],
		debug: true, // Gives us sourcemapping
		cache: {}, packageCache: {}, fullPaths: true // Requirement of watchify
	}).transform('reactify', {stripTypes: true});
	var content_watcher = watchify(content_bundler);

	var content = content_watcher
	.on('update', function () {
		var updateStart = Date.now();
		console.log('Updating!');
		content_watcher.bundle()
		.on('error', function(e) {
			gutil.log(gutil.colors.red('Bundle error:', e.message));
		})
// Create new bundle that uses the cache for high performance
		.pipe(source('content.js'))
		// This is where you add uglifying etc.
		.pipe(gulp.dest("./extension"));
		console.log('Updated!', (Date.now() - updateStart) + 'ms');
	})
	// Create the initial bundle when starting the task
	.bundle()
	.on('error', function(e) {
		gutil.log(gutil.colors.red('Bundle error:', e.message));
	})
	.pipe(source('content.js'))
	.pipe(gulp.dest("./extension"));


});

gulp.task('default', ['browserify'], function() {
});
