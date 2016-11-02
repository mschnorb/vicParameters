#!/usr/bin/env Rscript

#####################################################################################################
#USAGE: Rcsript [options] wrapper_make_VIC_param.r [ARGUMENTS]

#DESCRIPTION: Build VICGL vegetation and band parameters

#ARGUMENTS:
# -u, --hrudf -   HRU data frame object
# -r, --rootdf -  Rooting depth data frame object
# -d, --dfile -   RData source file 
# -f, --fncfile - Name of R source file for function make_VIC_param()
# -g, --glacid -  ID of glacier land cover class
# -b, --maxb -    Maximum number of elevation bands in band file
# -n, --nullg -   If TRUE, add NULL glaciers ot vegetation parameter file and extra bottom band to band file
# -v, --vpfile -  Name of vegetation parameter file
# -s, --sbfile -  Name of elevation band file
# -S, --save -    Save function output to *.RData file
# -h, --help -    print help message

#DETAILS:
# Script uses tryCatch() to print 'result', which will either be TRUE (if successful), or an error/
# warning (if not successful). A side effect of the script is to write various text files.
#####################################################################################################

#Parse arguments
library('optparse')
option_list <- list(
  make_option(c("-u", "--hrudf"),   action="store", type="character", help="HRU data frame object [required]"),
  make_option(c("-r", "--rootdf"),  action="store", type="character", help="Rooting depth data frame object [required]"),
  make_option(c("-d", "--dfile"),   action="store", type="character", help="Name of RData file [required]"),
  make_option(c("-v", "--vpfile"),  action="store", type="character", help="Name of output vegetation parameter file [required]"),
  make_option(c("-s", "--sbfile"),  action="store", type="character", help="Name of output elevation band parameter file [required]"),
  make_option(c("-f", "--fncfile"), action="store", type="character", help="R function(s) source code file [required]"),
  make_option(c("-g", "--glacid"),  action="store", type="integer", default=22, help="ID of glacioer landcover class [default is 22]"),
  make_option(c("-b", "--maxb"),    action="store", type="integer", default=20, help="Maximum number of bands for band file [default is 20]"),
  make_option(c("-n", "--nullg"),   action="store_true", default=FALSE, help="Add null glaciers to elevation bands missing glacier HRUs and add blank bottom elevation band for each cell"),
  make_option(c("-S", "--save"),    action="store_true", default=FALSE, help="Save results to *.RData file")
)
opt <- parse_args(OptionParser(option_list=option_list))
if(is.null(opt$hrudf))   stop("Missing argument for 'hrudf'. Use -h or --help flag for usage.")
if(is.null(opt$rootdf))  stop("Missing argument for 'rootdf'. Use -h or --help flag for usage.")
if(is.null(opt$dfile))   stop("Missing argument for 'dfile'. Use -h or --help flag for usage.")
if(is.null(opt$fncfile)) stop("Missing argument for 'fncfile'. Use -h or --help flag for usage.")
if(is.null(opt$vpfile))  stop("Missing argument for 'vpfile'. Use -h or --help flag for usage.")
if(is.null(opt$sbfile))  stop("Missing argument for 'sbfile'. Use -h or --help flag for usage.")

#Load source file(s)
load(opt$dfile)
source(opt$fncfile)
e <- environment()

#Construct VICGL parameters
result <- tryCatch({
  rslt <- make_VIC_param(e[[opt$hrudf]], e[[opt$rootdf]], vpf_filename=opt$vpfile, snb_filename=opt$sbfile,
                         null_glaciers=opt$nullg, glacierID=opt$glacid, max_bands=opt$maxb)
  if(opt$save) save(rslt, file="param.RData")
  rslt <- TRUE
}, warning = function(war){
  return(paste("make_VIC_param_WARNING: ", war))
}, error = function(err){
  return(paste("make_VIC_param_ERROR: ", err))
}, finally = {
  #do nothing
}) #End tryCatch

#Print 'result' - potentially used by calling scripts to test for succesful completion.
cat(result)
