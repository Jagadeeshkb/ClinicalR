library(plotPK)

head(Theoph)
 if (!require(pharmavis))    {               
devtools::install_github('asancpt/pharmavis')}
library(pharmavis)
 plot_nca(concData = Theoph, colSubj = "Subject", colTime = "Time", colConc = "conc")
 
