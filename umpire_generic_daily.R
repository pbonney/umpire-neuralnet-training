library(data.table)
library(parallel)

source("umpire_nn_tools.R")

min.y <- 2007
max.y <- 2015

y.l <- min.y:max.y
m.l <- mapply(get.date.range.f,y.l)
dt.y <- data.table(year=y.l,min=m.l[1,],max=m.l[2,])

wrapper.generic.daily.f <- function(year,min.date,max.date) {
    day.l <- seq(from=min.date,to=max.date,by="1 day")
    dt.r <- data.table(d.s=day.l)
    dt.r$stand <- "R"
    dt.l <- data.table(d.s=day.l)
    dt.l$stand <- "L"

    dt.t <- rbind(dt.r,dt.l)
    dt.t$d.e <- as.Date(dt.t$d.s)+1
    
    dt.t$path <- mcmapply(ump.train.and.save.f,d.s=as.Date(dt.t$d.s),d.e=as.Date(dt.t$d.e),
        stand=dt.t$stand,pitch.limit=15000,
        mc.cores=getOption("mc.cores",20L))

    return(dt.t$path)
}

# Test that it works
# m.list <- wrapper.generic.daily.f(2015,as.Date("2015-04-05"),as.Date("2015-10-04"))

# Note: this could be done more efficiently by combining all days into a single data table regardless
# of year, so there were no idle cores waiting for other threads to wrap up before moving on to the 
# next year. But for my purposes this is good enough, and anyone else is welcome to redo it.

m.list <- mapply(wrapper.generic.daily.f,dt.y$year,as.Date(dt.y$min),as.Date(dt.y$max))
