library(neuralnet)
library(data.table)
library(parallel)
library(RMySQL)

by.x <- 0.02
by.z <- 0.02

x <- seq(-2,2,by=by.x)
z <- seq(1,6,by=by.z)

grid.b <- expand.grid(x=x,z=z)
dt.grid <- data.table(x=grid.b$x,z=grid.b$z)
rm(grid.b)

bat.path <- "/users/peter/Documents/baseball/analysis/umpires/models.batter/"

load.bat.zone <- function(batter.t,year.t) {
	filename <- paste("nn.bat",batter.t,year.t,"rda",sep=".")
	if(!file.exists(paste(bat.path,filename,sep=""))) {
        stand <- get.batter.hand.f(batter.t, year.t)
        filename <- paste("nn.bat.generic",year.t,stand,"rda",sep=".")
    }
	load(paste(bat.path,filename,sep=""))
	nn.x <- m.bat
	return(nn.x)
}

get.batter.hand.f <- function(batter.t, year.t) {
    sqlstring <- "select stand, count(*) ab from atbats where batter="
    sqlstring <- paste(sqlstring,batter.t," and substr(gameName,5,4)=",year.t,sep="")
    sqlstring <- paste(sqlstring," group by 1;",sep="")

    mydb <- dbConnect(dbDriver("MySQL"),user="bbos",password="bbos",host="localhost",dbname="gameday")
    rs <- dbSendQuery(mydb,sqlstring)
    dt <- fetch(rs,-1)
    dbDisconnect(mydb)

    ab_max=max(dt$ab)
    hand=dt[dt$ab==ab_max,1]
    return(hand[1])
}

sz.top.bot <- function(nn.t) {
	dt.grid.t <- data.table(dt.grid)
	dt.grid.t$p <- compute(nn.t,data.frame(px=dt.grid.t$x,pz=dt.grid.t$z))$net.result
	dt.cand <- dt.grid.t[dt.grid.t$p>=0.5,]
	sz.top <- max(dt.cand$z)
	sz.bot <- min(dt.cand$z)
	return(list(sz.top,sz.bot))
}

batter.sz.top.bot <- function(batter.t,year.t) {
	nn.x <- load.bat.zone(batter.t,year.t)
	sz.dim <- sz.top.bot(nn.x)
	return(sz.dim)
}

sz.top.bot.l <- list(1:nrow(dt.batlist))

sz.top.bot.l <- mapply(batter.sz.top.bot,dt.batlist$id,year=dt.batlist$year)
# sz.top.bot.l <- mcmapply(batter.sz.top.bot,dt.batlist$id,dt.batlist$year,
# 	mc.preschedule=TRUE,mc.set.seed=TRUE,mc.silent=FALSE,mc.cores=getOption("mc.cores",8L),mc.cleanup=TRUE)

dt.batlist$sz.top <- as.numeric(sz.top.bot.l[1,])
dt.batlist$sz.bot <- as.numeric(sz.top.bot.l[2,])

mydb            <- dbConnect(dbDriver("MySQL"),user="bbos",password="bbos",host="localhost",dbname="gameday")
dt.write <- data.table(batter=dt.batlist$id,year=dt.batlist$year,sz_top=dt.batlist$sz.top,sz_bot=dt.batlist$sz.bot,generic=ifelse(dt.batlist$result==TRUE,0,1))
fields.l <- list(batter="int",year="int",sz_top="double",sz_bot="double",generic="int")
dbWriteTable(mydb,"batter_sz_top_bot",dt.write,field.types=fields.l,overwrite=TRUE,append=FALSE,row.names=FALSE)
dbDisconnect(mydb)
