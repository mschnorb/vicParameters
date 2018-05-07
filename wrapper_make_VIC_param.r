#!/usr/bin/env Rscript

#####################################################################################################
#USAGE: Rcsript [options] wrapper_make_VIC_param.r [ARGUMENTS]

#DESCRIPTION: Write VICGL vegetation and band parameters to file

#ARGUMENTS:
# -d, --dfile -   RData source file [required]
# -u, --hrudf -   HRU data frame object [required]
# -r, --rootdf -  Rooting depth data frame object [required]
# -b, --basin -   Sub-basin name
# -c, --celldf -  Cell map data frame object
# -v, --vpfile -  Name of vegetation parameter file [required]
# -s, --sbfile -  Name of elevation band file [required]
# -f, --fncfile - Name of R source file for function make_VIC_param() [required]
# -g, --glacid -  ID of glacier land cover class [default = 22]
# -z, --maxz -    Maximum number of elevation bands in band file [default = 20]
# -m, --minb -    BAND_ID of lowest band (i.e. that which includes sea level)
# -n, --nullg -   If TRUE, add NULL glaciers to vegetation parameter file and extra bottom band to band file [default = FALSE]
# -S, --save -    Save function output to *.RData file [default = FALSE]
# -h, --help -    print help message

#DETAILS:
# Script uses side-effect of function make_VIC_param() to generate VICGL parameters and write the
# vegetation and band parameter files. The -S or --save flags can also be used to save the return
# value of the function make_VIC_param() to an *.RData file. See the documentation for make_VIC_param()
# for a detailed desription of the function return value. Script uses tryCatch() to print 'result',
# which will either be TRUE (if successful), or an error/warning (if not successful).
#####################################################################################################

#Parse arguments
library('optparse')
option_list <- list(
  make_option(c("-d", "--dfile"),   action="store", type="character", help="Name of RData file containing hrudf, rootdf and, optionally, celldf [required]"),
  make_option(c("-u", "--hrudf"),   action="store", type="character", help="HRU data frame object [required]"),
  make_option(c("-r", "--rootdf"),  action="store", type="character", help="Rooting depth data frame object [required]"),
  make_option(c("-b", "--basin"),   action="store", type="character", help="Sub-basin name [optional]"),
  make_option(c("-c", "--celldf"),  action="store", type="character", help="Cell map data frame object [required if --basin set]"),
  make_option(c("-v", "--vpfile"),  action="store", type="character", help="Name of output vegetation parameter file [required]"),
  make_option(c("-s", "--sbfile"),  action="store", type="character", help="Name of output elevation band parameter file [required]"),
  make_option(c("-f", "--fncfile"), action="store", type="character", help="R function(s) source code file [required]"),
  make_option(c("-g", "--glacid"),  action="store", type="integer", default=22, help="ID of glacier landcover class [default is 22]"),
  make_option(c("-z", "--maxz"),    action="store", type="integer", default=20, help="Maximum number of bands for band file [default is 20]"),
  make_option(c("-m", "--minb"),    action="store", type="integer", help="Band ID of lowest elevation band"),
  make_option(c("-n", "--nullg"),   action="store_true", default=FALSE, help="Add null glaciers to elevation bands missing glacier HRUs and add blank bottom elevation band for each cell"),
  make_option(c("-S", "--save"),    action="store_true", default=FALSE, help="Save results to *.RData file")
)
opt <- parse_args(OptionParser(option_list=option_list))
if(is.null(opt$dfile))   stop("Missing argument for 'dfile'. Use -h or --help flag for usage.")
if(is.null(opt$hrudf))   stop("Missing argument for 'hrudf'. Use -h or --help flag for usage.")
if(is.null(opt$rootdf))  stop("Missing argument for 'rootdf'. Use -h or --help flag for usage.")
if(is.null(opt$fncfile)) stop("Missing argument for 'fncfile'. Use -h or --help flag for usage.")
if(is.null(opt$vpfile))  stop("Missing argument for 'vpfile'. Use -h or --help flag for usage.")
if(is.null(opt$sbfile))  stop("Missing argument for 'sbfile'. Use -h or --help flag for usage.")
if(!is.null(opt$basin)){
  if(is.null(opt$celldf)) stop("Must specifiy cell map data frame object if providing sub-basin name. Use -h or --help flag for usage.")
}
if(opt$nullg & is.null(opt$minb)) stop("Must specifiy minimum band ID if nullg set. Use -h or --help flag for usage.")

#Load/source file(s)
load(opt$dfile)
source(opt$fncfile)
e <- environment()

#Check that all required R objects are present
if(is.na(match(opt$hrudf, ls(e)))) stop(cat("Specified hru data frame object '", opt$hrudf, "' is missing.\n", sep=""))
if(is.na(match(opt$rootdf, ls(e)))) stop(cat("Specified root depth data frame object '", opt$rootdf, "' is missing.\n", sep=""))
if(!is.null(opt$basin)){
  if(is.na(match(opt$celldf, ls(e)))) stop(cat("Specified cell map data frame object '", opt$celldf, "' is missing.\n", sep=""))
}

#Subset main data frame if required
if(!is.null(opt$basin)){
  cell_map <- e[[opt$celldf]]
  ind <- which(cell_map$NAME==opt$basin)
  if(length(ind)==0) stop(paste("Sub-basin '", opt$basin, "' could not be found in supplied data frame.", sep=""))
  cells <- cell_map$CELL_ID[ind]
  inFrame <- do.call(rbind, lapply(cells, function(x, dfy){dfy[which(dfy$CELL_ID==x),]}, e[[opt$hrudf]]))
} else {
  inFrame <- e[[opt$hrudf]]
}

#Construct VICGL parameters
result <- tryCatch({
  rslt <- make_VIC_param(inFrame, e[[opt$rootdf]], vpf_filename=opt$vpfile, snb_filename=opt$sbfile,
                         null_glaciers=opt$nullg, glacierID=opt$glacid, max_bands=opt$maxz, min_band_id=opt$minb)
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
cat(result, "\n")
