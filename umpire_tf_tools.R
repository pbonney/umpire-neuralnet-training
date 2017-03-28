library(RMySQL)
library(data.table)
library(tensorflow)
source("gameday_date_functions.R")

plimit.c <- 10000000
plimit.g <- 10000000
ump.mod.dir <- "models.umpire"

# Return a string with the SQL query for retrieving umpire training data.
# If an id is specified the query will be for that specific umpire, otherwise it will be for all umpires.

ump.training.data.query.tf <- function(id = -1,d.s = as.Date("2006-01-01"),d.e = as.Date("2006-01-02"),
				stand = "", incl.spring = FALSE, pitch.limit=plimit.c) {
        sqlString <- "  SELECT  p.gamedayPitchID,
                                concat(substr(a.gameName,5,4),'-',substr(a.gameName,10,2),'-',substr(a.gameName,13,2)) as date,
																u.id,
																a.batter,
																a.pitcher,
                                a.stand,
																a.p_throws,
                                a.b_height_in,
                                b.sz_top,
                                b.sz_bot,
                                (b.sz_top+b.sz_bot)/2 as sz_mid,
                                p.px,
                                p.pz,
                                p.des,
																p.start_speed,
																p.pfx_x,
																p.pfx_z
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
ump.train.model.tf <- function(id = -1,
															d.s = as.Date("2006-01-01"),
															d.e = as.Date("2006-01-02"),
			    										stand = "B",
															pitch.limit = plimit.c,
															incl.spring = FALSE,
															incl.pitchdata = FALSE) {
	sqlString <- ump.training.data.query.tf(id=id,
																			 d.s=d.s,
																			 d.e=d.e,
																			 stand=stand,
																			 pitch.limit=pitch.limit,
																			 incl.spring=incl.spring)

	mydb <- dbConnect(dbDriver("MySQL"),user="bbos",password="bbos",host="192.168.235.38",dbname="gameday")
	rs <- dbSendQuery(mydb,sqlString)
	dt <- fetch(rs,-1)
	dbDisconnect(mydb)

	dt$pz.ratio <- (dt$pz-dt$sz_mid)/((dt$sz_top-dt$sz_bot)/2)
	dt$s.f <- as.numeric(dt$des == "Called Strike")
	pcount <- nrow(dt)

	if(!incl.pitchdata) {
	} else {
	}

	return(m)
}

test_split <- function(df, cuts, prob, ...) {
  idx <- sample(seq(1, cuts), size = nrow(df), replace = TRUE, prob = prob, ...)
  z = list()
  for (i in 1:cuts)
    z[[i]] <- df[idx == i,]
  z
}

sqlString <- ump.training.data.query.tf(id=id,
																		 d.s=d.s,
																		 d.e=d.e,
																		 stand=stand,
																		 pitch.limit=pitch.limit,
																		 incl.spring=incl.spring)

mydb <- dbConnect(dbDriver("MySQL"),user="bbos",password="bbos",host="localhost",dbname="gameday")
rs <- dbSendQuery(mydb,sqlString)
dt <- fetch(rs,-1)
dbDisconnect(mydb)

dt$pz.ratio <- (dt$pz-dt$sz_mid)/((dt$sz_top-dt$sz_bot)/2)
dt$s.f <- as.numeric(dt$des == "Called Strike")

z <- test_split(dt, 3, c(0.6,0.2,0.2))

train <- z[1]
test <- z[2]
validate <- z[3]
