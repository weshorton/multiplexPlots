#!/bin/sh

### Function to run shiny app
function runApp() {
    cmd="R --slave --no-restore -e 'shiny::runApp(\"$1\", launch.browser=T)'"
    echo $cmd
    eval $cmd
}

### Run
if [ -z $MULTIPLEX_DIR ]; then
	## Check for server.R
	if [ -a ./server.R ]; then
		echo "Running in current directory"
		runApp ./server.R
	else
		a="server.R does not exist in current directory."
		b="Either move run.command to the same directory as server.R or"
		c='set $MULTIPLEX_DIR environment variable (see README.md)'
		printf '%s\n\n%s\n%s\n' "$a" "$b" "$c"
	fi
else
	printf 'Running in $MULTIPLEX_DIR: %s\n' $MULTIPLEX_DIR
	runApp $MULTIPLEX_DIR/server.R
fi
