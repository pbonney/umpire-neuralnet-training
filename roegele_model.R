library(data.table)
library(RMySQL)

ump.mod.dir <- "models.umpire"

pitch.query <- function(id = -1,d.s = as.Date("2006-01-01"),d.e = as.Date("2006-01-02"),
                stand = "", incl.spring = FALSE) {
        sqlString <- "  SELECT  u.id as umpire,
                                p.gamedayPitchID,
                                concat(substr(a.gameName,5,4),'-',substr(a.gameName,10,2),'-',substr(a.gameName,13,2)) as Date,
                                a.stand,
                                b.sz_top,
                                b.sz_bot,
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
                        AND     p.px is not null
                        AND     p.pz is not null"
        if(id != -1) { sqlString <- paste(sqlString,"AND u.id=",id) }
        if(stand %in% c('R','L')) { sqlString <- paste(sqlString," AND a.stand='",stand,"' ",sep="") }
        else { sqlString <- paste(sqlString, "AND a.stand in ('R','L')") }
        if(incl.spring) { sqlString <- paste(sqlString,"AND g.type IN ('R','S')") }
        else        { sqlString <- paste(sqlString,"AND g.type='R'") }
        sqlString <- paste(sqlString," AND STR_TO_DATE(concat(substr(a.gameName,5,4),'-',substr(a.gameName,10,2),'-',substr(a.gameName,13,2)), '%Y-%m-%d') < '",d.e,"'",sep="")
        sqlString <- paste(sqlString," AND STR_TO_DATE(concat(substr(a.gameName,5,4),'-',substr(a.gameName,10,2),'-',substr(a.gameName,13,2)), '%Y-%m-%d') >= '",d.s,"'",sep="")
        sqlString <- paste(sqlString,"ORDER BY concat(substr(a.gameName,5,4),'-',substr(a.gameName,10,2),'-',substr(a.gameName,13,2)) DESC, p.gamedayPitchID DESC")
}

# Function to build a strike zone model using Jon Roegele's method
# (which is to split the home plate area up into 1 inch squares and count up
# how many of them had pitches called a strike > 50% of the time)
sz.model.roegele.f <- function(id = -1, d.s = as.Date("2006-01-01"), d.e = as.Date("2006-01-02"),
                stand = "B", incl.spring = FALSE) {
    sqlString <- pitch.query(id=id,d.s=d.s,d.e=d.e,stand=stand,incl.spring=incl.spring)

    mydb <- dbConnect(dbDriver("MySQL"),user="bbos",password="bbos",host="localhost",dbname="gameday")
    rs <- dbSendQuery(mydb,sqlString)
    dt <- fetch(rs,-1)
    dbDisconnect(mydb)

    dt$n <- 1
    dt$s.f <- as.numeric(dt$des == "Called Strike")

    # create X,Y labels for each pitch by rounding down to nearest even inch in both dimensions
    dt$grid.x <- floor(dt$px*12)
    dt$grid.z <- floor(dt$pz*12)

    # Aggregate by X,Y labels and find the ratio of called strikes in each bucket
    dt.agg <- aggregate(cbind(n,s.f) ~ grid.x + grid.z, data = dt, sum)
    dt.agg$ratio <- dt.agg$s.f/dt.agg$n

    # Voila, a model! The "ratio" column is the fraction of pitches that were called
    # strikes in a given X,Y location.
    return(dt.agg)
}

# For a given strike zone model, find the probability that a pitch located at
# [px, pz] would be called a strike.
eval.roegele.f <- function(dt.m, px, pz) {
  # Convert px, pz to grid coordinates
  x = floor(px*12)
  z = floor(pz*12)

  # Find the matching row
  r = dt.m[dt.m$grid.x == x & dt.m$grid.z == z,]

  # If there's a matching row, return that ratio
  if(nrow(r) == 1) {
    return(r$ratio)
  }

  # More than one matching row => an error with the model
  if(nrow(r) > 1) {
    return(-1)
  }

  # No matching rows, return 0
  return(0)
}
eval.roegele.from.file.f <- function(filepath, px, pz) {
  load(file=filepath)
  dt.t <- dt.mod
  rm(dt.mod)
  return(eval.roegele.f(dt.t, px, pz))
}

# Save a given Roegele-based strike zone model for later use
save.model.roegele.f <- function(dt.mod,id=-1,d.s,d.e,stand="B",dir = ".") {
	if(dir==".") {
    dir <- paste(getwd(),ump.mod.dir,sep="/")
  }
	prefix <- paste("roeg",ifelse(id==-1,"generic",id),sep=".")
	file.name <- paste(prefix,d.s,d.e,stand,"rda",sep=".")
	save.string <- paste(dir,file.name,sep="/")
	save(dt.mod,file=save.string)
	return(save.string)
}

# Wrapper function to train and save a Roegele-based strike zone model
#
# Fields:
#   id (optional): GameDay umpire id
#   d.s: start of date range (inclusive)
#   d.e: end of date range (exclusive)
#   stand (optional): batter hand
#   incl.spring (optional): include spring training in training data? (default is FALSE)
#   dir (optional): path in which to save file; default is to save to [R working directory]/models.umpire/
roeg.train.and.save.f <- function(id=-1,d.s,d.e,stand="B",incl.spring=FALSE,dir=".") {
    model <- sz.model.roegele.f(id,d.s,d.e,stand,incl.spring)
    save <- save.model.roegele.f(model,id,d.s,d.e,stand,dir)
    return(save)
}
