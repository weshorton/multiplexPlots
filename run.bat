REM Run
REM R --slave --no-restore -e shiny::runApp('./server.R', launch.browser=T)

REM ### Haven't tried this yet - requires a run.r script
REM "C:\Program Files\R\R-3.5.1\bin\Rscript.exe" "C:\Users/wrhor/multiplexPlots/run.R"

"C:\Program Files\R\R-3.5.1\bin\Rscript.exe" -e "shiny::runApp('C:\Users/wrhor/multiplexPlots/run.R', launch.browser = T)"

SLEEP 30
