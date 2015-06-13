#!/bin/bash
HOME=/afs/hcoop.net/user/u/us/usr

# GHOSTROOT=$HOME/haskelboard/dist_haskelboard
HASKELROOT=$HOME/haskelboard/dist_haskelboard

# NODEROOT=$HOME/public_web/ghost/nodejs

# NODE=$NODEROOT/bin/node

EXEC=$HASKELROOT"/exec -p 50040"

PIDFILE=$GHOSTROOT/ghost.pid
HASKELPIDFILE=$HASKELROOT/haskel.pid

# K5START="k5start -qtUf /etc/keytabs/user.daemon/mcarberry"
K5START="k5start -qtUf /etc/keytabs/user.daemon/usr"

# cd $GHOSTROOT
cd $HASKELROOT

# NODE_CONFIG_DIR=$GHOSTROOT
HASKEL_CONFIG_DIR=$HASKELROOT

# if (test -f $PIDFILE); then
if (test -f $HASKELPIDFILE); then
	echo "PID File exists..."
	# PID=`cat $PIDFILE 2> /dev/null`
	PID=`cat $HASKELPIDFILE 2> /dev/null`
	echo "Is previous server still running"
	kill -0 $PID 2> /dev/null
	if (test $? -ne 0); then
		echo "Nope, starting server"
		# $K5START -b -c $PIDFILE -- $NODE $GHOSTROOT/index.js
		$K5START -b -c $HASKELPIDFILE -- $EXEC
	else
		echo "Yes, no need to start server"
		exit 0
	fi  
else
	echo "Starting server"
	# $K5START -b -c $PIDFILE -- $NODE $GHOSTROOT/index.js
	$K5START -b -c $HASKELPIDFILE -- $EXEC
fi
