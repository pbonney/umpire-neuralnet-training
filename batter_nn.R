library(RMySQL)
library(parallel)
library(data.table)
library(neuralnet)

pcount.min.b <- 200
pcount.max.g <- 99999
batter.path <- "/users/peter/Documents/baseball/analysis/umpires/models.batter/"

sqlStringBatterList <- sqlImport("/users/peter/Documents/baseball/analysis/umpires/batter_list.sql")
mydb            <- dbConnect(dbDriver("MySQL"),user="bbos",password="bbos",host="localhost",dbname="gameday")
rs.batlist      <- dbSendQuery(mydb,sqlStringBatterList)
df.batlist      <- fetch(rs.batlist,-1)
dbDisconnect(mydb)

dt.batlist	<- data.table(id=df.batlist$id,year=df.batlist$year)
rm(df.batlist)
# dt.batlist  <- dt.batlist[dt.batlist$year>=min.year,]

bat.model.f	<- function(id,year) {
	message(paste(Sys.time(),"Updating batter model",id,year))

	if(file.exists(paste(batter.path,paste("nn.bat",id,year,"rda",sep="."),sep=""))) { return(TRUE) }

	sqlString <- "SELECT p.px,p.pz,p.des FROM atbats a, pitches p, games g "
	sqlString <- paste(sqlString, " WHERE a.gameName=p.gameName AND a.num=p.gameAtBatID ")
	sqlString <- paste(sqlString, " AND a.gameName=g.gameName AND g.type='R' ")
	sqlString <- paste(sqlString, " AND a.batter=",id,sep="")
	sqlString <- paste(sqlString, " AND substr(a.gameName,5,4)='",year,"'",sep="")
	sqlString <- paste(sqlString, " AND p.px is not null and p.pz is not null ")
	sqlString <- paste(sqlString, " AND p.des in ('Ball','Ball in Dirt','Called Strike') ")

	mydb    <- dbConnect(dbDriver("MySQL"),user="bbos",password="bbos",host="localhost",dbname="gameday")
	rs <- dbSendQuery(mydb,sqlString)
	df      <- fetch(rs,-1)
	dbDisconnect(mydb)
	dt	<- data.table(df)

    pcount <- nrow(dt)
	if(pcount < pcount.min.b) { return(FALSE) }

	h.b <- 4

	dt$s.f	<- as.numeric(dt$des %in% "Called Strike")
	m.bat	<- try(neuralnet(s.f~px+pz,data=dt,hidden=h.b,linear.output=FALSE,lifesign='full',threshold=max(0.01,pcount/100000)))
	err	<- m.bat$result.matrix[1]
	message(paste(Sys.time(),"Updating batter model",year,id,"got result",err))
	if(!is.numeric(err)) { return(FALSE) }
	if(err==0) { return(FALSE) }

	m.bat.name <- paste(batter.path,paste("nn.bat",id,year,"rda",sep="."),sep="")
	save(m.bat,file=m.bat.name)

	return(TRUE)
}

bat.model.generic.f <- function(year, stand) {
	if(file.exists(paste(batter.path,paste("nn.bat.generic",year,stand,"rda",sep="."),sep=""))) { return(TRUE) }

	sqlString <- "SELECT p.px,p.pz,p.des FROM atbats a, pitches p, games g "
	sqlString <- paste(sqlString, " WHERE a.gameName=p.gameName AND a.num=p.gameAtBatID ")
	sqlString <- paste(sqlString, " AND a.gameName=g.gameName AND g.type='R' ")
  sqlString <- paste(sqlString, " AND a.stand='",stand,"'",sep="")
	sqlString <- paste(sqlString, " AND substr(a.gameName,5,4)='",year,"'",sep="")
	sqlString <- paste(sqlString, " AND p.px is not null and p.pz is not null ")
	sqlString <- paste(sqlString, " AND p.des in ('Ball','Ball in Dirt','Called Strike') ")
	sqlString <- paste(sqlString, " LIMIT",pcount.max.g)

	mydb    <- dbConnect(dbDriver("MySQL"),user="bbos",password="bbos",host="localhost",dbname="gameday")
	rs <- dbSendQuery(mydb,sqlString)
	df      <- fetch(rs,-1)
	dbDisconnect(mydb)
	dt	<- data.table(df)

    pcount <- nrow(dt)
	if(pcount < pcount.min.b) { return(FALSE) }

	h.b <- 4

	dt$s.f	<- as.numeric(dt$des %in% "Called Strike")
	m.bat	<- try(neuralnet(s.f~px+pz,data=dt,hidden=h.b,linear.output=FALSE,lifesign='full',threshold=max(0.01,pcount/100000)))
	err	<- m.bat$result.matrix[1]
	message(paste(Sys.time(),"Updating generic batter model",year,stand,"got result",err))
	if(!is.numeric(err)) { return(FALSE) }
	if(err==0) { return(FALSE) }

	m.bat.name <- paste(paste(batter.path,"nn.bat.generic",sep=""),year,stand,"rda",sep=".")
	save(m.bat,file=m.bat.name)

	return(TRUE)
}

max.year <- max(dt.batlist$year)
min.year <- min(dt.batlist$year)

if(max.year==min.year) {
	message(paste(Sys.time(),"Updating generic batter models.R"))
  bat.model.generic.f(min.year,"L")
  bat.model.generic.f(min.year,"R")
} else {
    dt.generic <- data.table(year=c(min.year:max.year))
    dt.generic$result.L <- mcmapply(bat.model.generic.f,dt.generic$year,"L",
	    mc.preschedule=TRUE,mc.set.seed=TRUE,mc.silent=FALSE,mc.cores=getOption("mc.cores",8L),mc.cleanup=TRUE)
    dt.generic$result.R <- mcmapply(bat.model.generic.f,dt.generic$year,"R",
	    mc.preschedule=TRUE,mc.set.seed=TRUE,mc.silent=FALSE,mc.cores=getOption("mc.cores",8L),mc.cleanup=TRUE)
}

# bat.model.f(113028,2007)
dt.batlist$result <- mapply(bat.model.f,dt.batlist$id,dt.batlist$year)
# dt.batlist$result <- mcmapply(bat.model.f,dt.batlist$id,dt.batlist$year,
# 	mc.preschedule=TRUE,mc.set.seed=TRUE,mc.silent=FALSE,mc.cores=getOption("mc.cores",8L),mc.cleanup=TRUE)
