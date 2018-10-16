### Function to run shiny app
function runApp() {
    cmd="R --slave --no-restore -e 'shiny::runApp(\"$1\", launch.browser=T)'"
    echo $cmd
    eval $cmd
}

### Run
runApp ./server.R
