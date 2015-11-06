library(RMySQL)
library(data.table)
library(neuralnet)

h.ind.min <- 4
h.ind.max <- 5
pcount.cut <- 1500
plimit.c <- 5000
h.gen <- 5

# Gets the min and max date for regular season games in a given year.
get.date.range.f <- function(year) {
	sqlString <- 	"SELECT min(STR_TO_DATE(concat(substr(gameName,5,4),'-',substr(gameName,10,2),'-',substr(gameName,13,2)), '%Y-%m-%d')) as min_date,
				max(STR_TO_DATE(concat(substr(gameName,5,4),'-',substr(gameName,10,2),'-',substr(gameName,13,2)), '%Y-%m-%d')) as max_date
			FROM	games
			WHERE	type='R'
			AND	substr(gameName,5,4) ="
	sqlString <- paste(sqlString,year)

	mydb <- dbConnect(dbDriver("MySQL"),user="bbos",password="bbos",host="localhost",dbname="gameday")
	rs <- dbSendQuery(mydb,sqlString)
	dt <- fetch(rs,-1)
	dbDisconnect(mydb)
	
	return(c(dt$min_date[1],dt$max_date[1]))
}

# Return a string with the SQL query for retrieving umpire training data.
# If an id is specified the query will be for that specific umpire, otherwise it will be for all umpires.

ump.training.data.query <- function(id = -1,d.s = as.Date("2006-01-01"),d.e = as.Date("2006-01-02"),
				stand = "", incl.spring = FALSE, pitch.limit=plimit.c) {
        sqlString <- "  SELECT  p.gamedayPitchID,
                                concat(substr(a.gameName,5,4),'-',substr(a.gameName,10,2),'-',substr(a.gameName,13,2)) as date,
                                a.stand,
                                a.b_height_in,
                                b.sz_top,
                                b.sz_bot,
                                (b.sz_top+b.sz_bot)/2 as sz_mid,
                                p.px,
                                p.pz,
                                p.des
                        FROM    umpires u,
                                atbats a,
                                pitches p,
                                batter_sz_top_bot b,
                                games g
                        WHERE   u.gameName=p.gameName
                        AND     u.gameName=g.gameName
                        AND     p.gameName=a.gameName
                        AND     a.num=p.gameAtBatId
                        AND     a.batter=b.batter
                        AND     substr(a.gameName,5,4)=b.year
                        AND     u.position='home'
                        AND     p.des in ('Ball','Ball In Dirt','Called Strike')
                        AND     a.b_height_in is not null
                        AND     a.b_height_in!=0
                        AND     p.px is not null
                        AND     p.pz is not null"
        if(id != -1) { sqlString <- paste(sqlString,"AND u.id=",id) }
	if(stand %in% c('R','L')) { sqlString <- paste(sqlString," AND a.stand='",stand,"' ",sep="") }
	if(incl.spring) { sqlString <- paste(sqlString,"AND g.type IN ('R','S')") }
	else 		{ sqlString <- paste(sqlString,"AND g.type='R'") }
        sqlString <- paste(sqlString," AND STR_TO_DATE(concat(substr(a.gameName,5,4),'-',substr(a.gameName,10,2),'-',substr(a.gameName,13,2)), '%Y-%m-%d') < '",d.e,"'",sep="")
        sqlString <- paste(sqlString," AND STR_TO_DATE(concat(substr(a.gameName,5,4),'-',substr(a.gameName,10,2),'-',substr(a.gameName,13,2)), '%Y-%m-%d') >= '",d.s,"'",sep="")
        sqlString <- paste(sqlString,"ORDER BY concat(substr(a.gameName,5,4),'-',substr(a.gameName,10,2),'-',substr(a.gameName,13,2)) DESC, p.gamedayPitchID DESC LIMIT",pitch.limit)
}

# Function to load data and train model for specified date range and umpire.
# Fields:
# 	id: GameDay umpire id (if unspecified it will use all umpires)
#	d.s: start of date range (i.e. get data on or after this date)
#	d.e: end of date range (i.e. get date before this date)
#	stand: batter hand (L or R) (if unspecified it will use data for both hands)
#	pitch.limit: max number of pitches to use for training; will use most recent pitches regardless. default is plimit.c
#	incl.spring: include spring training results in training data? default is "FALSE"
# Function returns "FALSE" if model fails to converge, returns nn model otherwise.
ump.train.model.f <- function(id = -1, d.s = as.Date("2006-01-01"), d.e = as.Date("2006-01-02"),
			    stand = "B", pitch.limit = plimit.c, incl.spring = FALSE) {
	sqlString <- ump.training.data.query(id=id,d.s=d.s,d.e=d.e,stand=stand,pitch.limit=pitch.limit,incl.spring=incl.spring)

	mydb <- dbConnect(dbDriver("MySQL"),user="bbos",password="bbos",host="localhost",dbname="gameday")
	rs <- dbSendQuery(mydb,sqlString)
	dt <- fetch(rs,-1)
	dbDisconnect(mydb)

	dt$pz.ratio <- (dt$pz-dt$sz_mid)/((dt$sz_top-dt$sz_bot)/2)
	dt$s.f <- as.numeric(dt$des == "Called Strike")
	pcount <- nrow(dt)

	h <- ifelse(pcount > pcount.cut, h.ind.max, h.ind.min)

	m <- try(neuralnet(s.f~px+pz.ratio,data=dt,hidden=h,linear.output=FALSE))
	if (class(m) == "try-error") { return(FALSE) }

	return(m)
}

# Saves umpire strike zone model to disk.
#
# Fields:
#	m.t: an object of type nn (from neuralnet package)
#	id: GameDay umpire id
#	d.s: start of date range (inclusive)
#	d.e: end of date range (exclusive)
#	stand: batter hand
#	dir (optional): path in which to save file; default is to save to R working directory
ump.save.model.f <- function(m.t,id=-1,d.s,d.e,stand="B",dir = ".") {
	if(dir==".") { dir <- getwd() }
	prefix <- ifelse(id==-1,"generic",id)
	file.name <- paste(prefix,d.s,d.e,stand,"rda",sep=".")
	save.string <- paste(dir,file.name,sep="/")
	save(m.t,file=save.string)
	return(save.string)
}

# Wrapper function to train and save a strike zone model
#
# Fields:
#   id (optional): GameDay umpire id
#   d.s: start of date range (inclusive)
#   d.e: end of date range (exclusive)
#   stand (optional): batter hand
#   pitch.limit (optional): maximum number of training pitches (default is plimit.c)
#   incl.spring (optional): include spring training in training data? (default is FALSE)
#   dir (optional): path in which to save file; default is to save to R working directory
ump.train.and.save.f <- function(id=-1,d.s,d.e,stand="B",pitch.limit=plimit.c,incl.spring=FALSE,dir=".") {
    model <- ump.train.model.f(id,d.s,d.e,stand,pitch.limit,incl.spring)
    save <- ump.save.model.f(model,id,d.s,d.e,stand,dir)
    return(save)
}
