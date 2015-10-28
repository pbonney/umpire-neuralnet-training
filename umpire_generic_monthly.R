library(data.table)

source("umpire_nn_tools.R")

min.y <- 2007
max.y <- 2015

y.l <- min.y:max.y
m.l <- mapply(get.date.range.f,y.l)
dt.y <- data.table(year=y.l,min=m.l[1,],max=m.l[2,])

