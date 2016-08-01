library(data.table)
library(parallel)

source("umpire_nn_tools.R")

min.y <- 2015
max.y <- 2016

y.l <- min.y:max.y

wrapper.generic.yearly.f <- function(year) {
    start <- paste(year,"1-1",sep="-")
    end <- paste(year,"12-31",sep="-")
    dt.r <- data.table(d.s=start,d.e=end)
    dt.r$stand <- "R"
    dt.l <- data.table(d.s=start,d.e=end)
    dt.l$stand <- "L"

    dt.t <- rbind(dt.r,dt.l)

    dt.t$path <- mapply(ump.train.and.save.f,d.s=as.Date(dt.t$d.s),d.e=as.Date(dt.t$d.e),
        stand=dt.t$stand,pitch.limit=99999)

    # dt.t$path <- mcmapply(ump.train.and.save.f,d.s=as.Date(dt.t$d.s),d.e=as.Date(dt.t$d.e),
    #     stand=dt.t$stand,pitch.limit=99999,
    #     mc.cores=getOption("mc.cores",2L))

    return(dt.t$path)
}

# Test that it works
m.list <- wrapper.generic.yearly.f(max.y)
# m.list <- mcmapply(wrapper.generic.yearly.f, y.l,
#    mc.cores=getOption("mc.cores",10L))
