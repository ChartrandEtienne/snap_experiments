#!/bin/sh

DIST='../dist_haskelboard'

cp migration*.sql $DIST
cp -r snaplets $DIST
cp dist/build/snapexample/snapexample $DIST/exec
cp frontend/dist/app.js $DIST/frontend/dist/app.js
