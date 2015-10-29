##############################################################
##                                                          ##
## Use this to convert comma separated values files (*.csv) ##
##   in the data-raw directory of the NCStats package to    ##
##   R data files (.rdata) in the data directory of the     ##
##   NCStats package.  Note, there should be no .txt        ##
##   in data-raw ... run aaaConvertTab2CSV.R if there is.   ##
##                                                          ##
##############################################################

library(tools)

## Get all .csv files in the data-raw directory
setwd("C:/aaaWork/Programs/GitHub/NCStats/data-raw")
raw <- list.files(pattern="*.csv")

#### Convert to .rdata files ####
## Cycle through each file to make it an RDA file
for (f in raw) {
  print(f)
  ## Read file
  nm <- file_path_sans_ext(f)
  assign(nm,read.csv(f))
  ## Save as an .rdata file
  save(list=nm,file=paste0("../data/",nm,".rdata"))
}
