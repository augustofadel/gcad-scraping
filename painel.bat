"C:\Program Files (x86)\R\R-4.0.0\bin\R.exe" -e "pckg <- c('shiny');for (p in pckg) {if(all(p != installed.packages()[,'Package'])) install.packages(p, repos = 'http://cran.rstudio.com')};shiny::runApp('//WARQPRD14V/cempre/GCAD/REGISTRO_ADMINISTRATIVO/gcad-scraping/ui/app.R', launch.browser = TRUE)"
