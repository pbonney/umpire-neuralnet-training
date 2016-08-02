library(data.table)
library(parallel)

source("gameday_date_functions.R")
source("umpire_nn_tools.R")

wrapper.generic.monthly.f <- function(year,min.date,max.date,has.mar.games,has.oct.games) {
    start.l <- month.first.day.list.f(year,min.date,has.mar.games,has.oct.games)
    end.l <- month.last.day.list.f(year,max.date,has.mar.games,has.oct.games)

    dt.r <- data.table(d.s=start.l,d.e=end.l)
    dt.r$stand <- "R"
    dt.l <- data.table(d.s=start.l,d.e=end.l)
    dt.l$stand <- "L"

    dt.t <- rbind(dt.r,dt.l)

#    dt.t$path <- mapply(ump.train.and.save.f,d.s=as.Date(dt.t$d.s),d.e=as.Date(dt.t$d.e),
#        stand=dt.t$stand,pitch.limit=99999)

    dt.t$path <- mcmapply(ump.train.and.save.f,d.s=as.Date(dt.t$d.s),d.e=as.Date(dt.t$d.e),
        stand=dt.t$stand,pitch.limit=99999,dir=paste(getwd(),"models.umpire/monthly",sep="/"),
        mc.cores=getOption("mc.cores",10L))

    return(dt.t$path)
}

# Test that it works
# m.list <- wrapper.generic.monthly.f(2015,as.Date("2015-04-05"),as.Date("2015-10-04"),FALSE,TRUE)
m.list <- mcmapply(wrapper.generic.monthly.f, year=dt.y$year, min.date=as.Date(dt.y$min),
  max.date=dt.y$max, has.mar.games=dt.y$has.mar.games, has.oct.games=dt.y$has.oct.games,
  mc.cores=getOption("mc.cores",2L))
