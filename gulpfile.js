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
    var bundler = browserify({
        entries: ["./frontend/src/Main.jsx"],
        debug: true, // Gives us sourcemapping
        cache: {}, packageCache: {}, fullPaths: true // Requirement of watchify
    }).transform('reactify', {stripTypes: true});
    var watcher  = watchify(bundler);

    var dashboard = watcher
	// When any files update
    .on('update', function () {
        var updateStart = Date.now();
        console.log('Updating!');
        watcher.bundle()
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


});

gulp.task('default', ['browserify'], function() {
});
