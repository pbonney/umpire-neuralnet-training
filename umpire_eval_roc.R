library(neuralnet)
library(parallel)
library(data.table)
library(RMySQL)

score.pitch.nn.f <- function(id, year, month=0, generic=FALSE, predict=TRUE, regress=1) {
  sqlString <- "  SELECT  p.gamedayPitchID,
                          u.id,
                          concat(substr(a.gameName,5,4),'-',substr(a.gameName,10,2),'-',substr(a.gameName,13,2)) as date,
                          a.stand,
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
                  AND     a.num=p.gameAtBatID
                  AND     a.batter=b.batter
                  AND     substr(a.gameName,5,4)=b.year
                  AND     u.position='home'
                  AND     g.type='R'
                  AND     p.des in ('Ball','Ball In Dirt','Called Strike')
                  AND     p.px is not null
                  AND     p.pz is not null"
  sqlString <- paste(sqlString,"AND u.id=",id)
  sqlString <- paste(sqlString,"AND substr(a.gameName,5,4)=",year)
  if(month>0) { sqlString <- paste(sqlString,"AND substr(a.gameName,10,2)=",month) }

  mydb    <- dbConnect(dbDriver("MySQL"),user="bbos",password="bbos",host="localhost",dbname="gameday")
  rs      <- dbSendQuery(mydb,sqlString)
  df      <- fetch(rs,-1)
  dbDisconnect(mydb)

  dt      <- data.table(df)

  dt$pz.ratio <- (dt$pz-dt$sz_mid)/((dt$sz_top-dt$sz_bot)/2)
  dt$s.f  <- as.numeric(dt$des %in% "Called Strike")

  if(month == 0) {
    start   <- paste(year,"01","01",sep="-")
    end     <- paste(year,"12","31",sep="-")
  } else {
    start   <- as.Date(paste(year, month-1, 1, sep="-"))
    end     <- as.Date(paste(year, month-1, month.last.day.f(month-1), sep="-"))
  }
  m.r.gen.name <- paste(paste("./models.umpire",ifelse(month=0,"","monthly"),"generic",sep="/"),start,end,"R","rda",sep=".")
  m.l.gen.name <- paste(paste("./models.umpire",ifelse(month=0,"","monthly"),"generic",sep="/"),start,end,"L","rda",sep=".")
  m.r.name  <- paste(paste("./models.umpire",ifelse(month=0,"","monthly"),id,sep="/"),start,end,"R","rda",sep=".")
  m.l.name  <- paste(paste("./models.umpire",ifelse(month=0,"","monthly"),id,sep="/"),start,end,"L","rda",sep=".")
  m.r.roeg.name <- paste(paste("./models.umpire",ifelse(month=0,"","monthly.roegele"),"roeg.generic",sep="/"),start,end,"R","rda",sep=".")
  m.l.roeg.name <- paste(paste("./models.umpire",ifelse(month=0,"","monthly.roegele"),"roeg.generic",sep="/"),start,end,"L","rda",sep=".")

  if(file.exists(m.r.name)) {
    load(file=m.r.name)
    m1.r <- m.t
    rm(m.t)
    p.test <- try(compute(m1.r,data.frame(px=0,pz.ratio=0))$net.result)
    if (class(p.test) == "try-error") {
        cat(paste(id,year,month,"R"),file=log,sep="\n",append=TRUE)
    }
  }
  if(file.exists(m.l.name)) {
    load(file=m.r.name)
    m1.l <- m.t
    rm(m.t)
    p.test <- try(compute(m1.l,data.frame(px=0,pz.ratio=0))$net.result)
    if (class(p.test) == "try-error") {
        cat(paste(id,year,month,"L"),file=log,sep="\n",append=TRUE)
    }
  }
  if(file.exists(m.r.gen.name)) {
    load(file=m.r.gen.name)
    mg.r <- m.t
    rm(m.t)
    p.test <- try(compute(mg.r,data.frame(px=0,pz.ratio=0))$net.result)
    if (class(p.test) == "try-error") {
        cat(paste("generic",year,month,"R"),file=log,sep="\n",append=TRUE)
    }
  }
  if(file.exists(m.l.gen.name)) {
    load(file=m.l.gen.name)
    mg.l <- m.t
    rm(m.t)
    p.test <- try(compute(mg.l,data.frame(px=0,pz.ratio=0))$net.result)
    if (class(p.test) == "try-error") {
        cat(paste("generic",year,month,"L"),file=log,sep="\n",append=TRUE)
    }
  }
  if(file.exists(m.r.roeg.name)) {
    load(file=m.r.roeg.name)
    mr.r <- m.t
    rm(m.t)
    p.test <- try(compute(mr.r,data.frame(px=0,pz.ratio=0))$net.result)
    if (class(p.test) == "try-error") {
        cat(paste("roeg.generic",year,month,"R"),file=log,sep="\n",append=TRUE)
    }
  }
  if(file.exists(m.l.roeg.name)) {
    load(file=m.l.roeg.name)
    mr.l <- m.t
    rm(m.t)
    p.test <- try(compute(mr.l,data.frame(px=0,pz.ratio=0))$net.result)
    if (class(p.test) == "try-error") {
        cat(paste("roeg.generic",year,month,"L"),file=log,sep="\n",append=TRUE)
    }
  }

  # Apply each model to data
}
