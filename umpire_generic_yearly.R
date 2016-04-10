library(data.table)
library(parallel)

source("umpire_nn_tools.R")

min.y <- 2007
max.y <- 2016

y.l <- min.y:max.y
m.l <- mapply(get.date.range.f,y.l)
dt.y <- data.table(year=y.l,min=m.l[1,],max=m.l[2,])

year.first.day.list.f <- function(year) {
    s1.l
    s1.l <- ifelse(has.mar.games,
                   c(paste(year,3,as.POSIXlt(min.date)$mday,sep="-"),paste(year,4,1,sep="-")),
                   c(paste(year,4,as.POSIXlt(min.date)$mday,sep="-")))
    s2.l <- c(paste(year,5,1,sep="-"),paste(year,6,1,sep="-"),paste(year,7,1,sep="-"),paste(year,8,1,sep="-"))
    s3.l <- ifelse(has.oct.games,
                   c(paste(year,9,1,sep="-"),paste(year,10,1,sep="-")),
                   c(paste(year,9,1,sep="-")))
    start.l <- c(s1.l,s2.l,s3.l)
    return(start.l)
}

month.last.day.list.f <- function(year,max.date,has.mar.games,has.oct.games) {
    e1.l <- ifelse(has.mar.games,
                   c(paste(year,3,31,sep="-"),paste(year,4,30,sep="-")),
                   c(paste(year,4,30,sep="-")))
    e2.l <- c(paste(year,5,31,sep="-"),paste(year,6,30,sep="-"),paste(year,7,31,sep="-"),paste(year,8,31,sep="-"))
    e3.l <- ifelse(has.oct.games,
                   c(paste(year,9,30,sep="-"),paste(year,10,as.POSIXlt(max.date)$mday,sep="-")),
                   c(paste(year,9,as.POSIXlt(max.date)$mday,sep="-")))
    end.l <- c(e1.l,e2.l,e3.l)
    return(end.l)
}

wrapper.generic.monthly.f <- function(year) {
    start <- paste(year,"1-1",sep="-")
    end <- paste(year,"12-31",sep="-")
    dt.r <- data.table(d.s=start,d.e=end)
    dt.r$stand <- "R"
    dt.l <- data.table(d.s=start,d.e=end)
    dt.l$stand <- "L"

    dt.t <- rbind(dt.r,dt.l)

#    dt.t$path <- mapply(ump.train.and.save.f,d.s=as.Date(dt.t$d.s),d.e=as.Date(dt.t$d.e),
#        stand=dt.t$stand,pitch.limit=99999)

    dt.t$path <- mcmapply(ump.train.and.save.f,d.s=as.Date(dt.t$d.s),d.e=as.Date(dt.t$d.e),
        stand=dt.t$stand,pitch.limit=99999,
        mc.cores=getOption("mc.cores",16L))

    return(dt.t$path)
}

# Test that it works
m.list <- wrapper.generic.monthly.f(2015,as.Date("2015-04-05"),as.Date("2015-10-04"),FALSE,TRUE)