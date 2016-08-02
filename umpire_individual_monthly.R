#!/opt/local/bin/Rscript

library(neuralnet)
library(parallel)
library(data.table)
library(RMySQL)

source("gameday_date_functions.R")
source("umpire_nn_tools.R")

sqlStringUmpList <- sqlImport("./umpire_eval_list_monthly.sql")
mydb            <- dbConnect(dbDriver("MySQL"),user="bbos",password="bbos",host="localhost",dbname="gameday")
rs.umplist      <- dbSendQuery(mydb,sqlStringUmpList)
df.umplist      <- fetch(rs.umplist,-1)
dbDisconnect(mydb)

dt.umplist      <- data.table(df.umplist)
rm(df.umplist)

wrapper.individual.monthly.f <- function(myid,year,month,mystand) {
  path <- ump.train.and.save.f(id=myid,d.s=as.Date(paste(year,1,1,sep="-")),
    d.e=as.Date(paste(year,month,month.last.day.f(year),sep="-")),stand=mystand,
    dir=paste(getwd(),"models.umpire/monthly",sep="/"))

  return(path)
}

dt.umplist$path <- mcmapply(wrapper.individual.monthly.f, dt.umplist$id, dt.umplist$year,
  dt.umplist$month, dt.umplist$stand,
  mc.cores=getOption("mc.cores",20L))
