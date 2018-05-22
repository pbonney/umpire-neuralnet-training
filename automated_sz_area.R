library(RMySQL)
library(data.table)
library(neuralnet)

save_dir <- "./umpire_uzr"
area_prefix <- "sz_area_"
file_type <- ".txt"

year.c <- 2018

message(paste(Sys.time(),"Loading sz_area.R"))
source("sz_area.R")
message(paste(Sys.time(),"Calculating NN-based area"))
dt.me <- area.all.years.f(min.year=year.c, max.year=year.c)
message(paste(Sys.time(),"Done with NN-based area"))

message(paste(Sys.time(),"Calculating Roegele area"))
dt.roeg <- roegele.all.years.f(min.year=year.c, max.year=year.c)
message(paste(Sys.time(),"Done with Roegele area"))

dt.area <- data.table(year=dt.me$year, stand=dt.me$stand, my_area=dt.me$area, roegele_area=dt.roeg$area, diff=dt.me$area-dt.roeg$area)

message(paste(Sys.time(),"Saving area results"))
area_filename <- paste(area_prefix, Sys.Date(), file_type, sep="")
area_savestring <- paste(save_dir, area_filename, sep="/")
write.table(dt.area, file=area_savestring, quote=FALSE, row.names=FALSE)
