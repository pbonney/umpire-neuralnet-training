library(neuralnet)
library(parallel)
library(data.table)
library(RMySQL)
source("umpire_nn_tools.R")
source("roegele_model.R")

roc.dbtable <- "umpire_roc"

sqlStringRocList <- sqlImport("./umpire_roc_list_monthly.sql")
mydb            <- dbConnect(dbDriver("MySQL"),user="bbos",password="bbos",host="localhost",dbname="gameday")
rs.umplist      <- dbSendQuery(mydb,sqlStringRocList)
df.umplist      <- fetch(rs.umplist,-1)
dbDisconnect(mydb)

dt.roc      <- data.table(df.umplist)
rm(df.umplist)

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

  dt.r    <- dt[dt$stand=="R",]
  dt.l    <- dt[dt$stand=="L",]

  if(month == 0) {
    start   <- paste(year,"01","01",sep="-")
    r.start <- paste(year,"01","01",sep="-")
    end     <- paste(year,"12","31",sep="-")
  } else {
    start   <- paste(year,"01","01",sep="-")
    r.start <- as.Date(paste(year, month-1, 1, sep="-"))
    end     <- as.Date(paste(year, month-1, month.last.day.f(month-1), sep="-"))
    # Stupid bug with file names - just deal with it for now and fix it after analysis is Done
    end2    <- as.Date(paste(year, month-1, 30, sep="-"))
  }
  m.r.gen.name <- paste(paste("./models.umpire",ifelse(month==0,"","monthly"),"generic",sep="/"),r.start,end,"R","rda",sep=".")
  m.l.gen.name <- paste(paste("./models.umpire",ifelse(month==0,"","monthly"),"generic",sep="/"),r.start,end,"L","rda",sep=".")
  m.r.name  <- paste(paste("./models.umpire",ifelse(month==0,"","monthly"),id,sep="/"),start,end2,"R","rda",sep=".")
  m.l.name  <- paste(paste("./models.umpire",ifelse(month==0,"","monthly"),id,sep="/"),start,end2,"L","rda",sep=".")
  m.r.roeg.name <- paste(paste("./models.umpire",ifelse(month==0,"","monthly.roegele"),"roeg.generic",sep="/"),r.start,end,"R","rda",sep=".")
  m.l.roeg.name <- paste(paste("./models.umpire",ifelse(month==0,"","monthly.roegele"),"roeg.generic",sep="/"),r.start,end,"L","rda",sep=".")
  m.r.gen.pdata.name <- paste(paste("./models.umpire",ifelse(month==0,"","monthly.pdata"),"generic.pitchdata",sep="/"),r.start,end,"R","rda",sep=".")
  m.l.gen.pdata.name <- paste(paste("./models.umpire",ifelse(month==0,"","monthly.pdata"),"generic.pitchdata",sep="/"),r.start,end,"L","rda",sep=".")

  # message(paste("m.r.name:",m.r.name))
  # message(paste("m.r.gen.name",m.r.gen.name))
  # message(paste("m.r.roeg.name",m.r.roeg.name))

  m1.r.ok <- FALSE
  m1.l.ok <- FALSE
  mg.r.ok <- FALSE
  mg.l.ok <- FALSE
  mr.r.ok <- FALSE
  mr.l.ok <- FALSE
  mp.r.ok <- FALSE
  mp.l.ok <- FALSE

  if(file.exists(m.r.name)) {
    load(file=m.r.name)
    m1.r <- m.t
    rm(m.t)
    p.test <- try(compute(m1.r,data.frame(px=0,pz.ratio=0))$net.result)
    if (class(p.test) == "try-error") {
      message(paste("Bad model",id,year,month,"R"))
    } else {
      m1.r.ok <- TRUE
    }
  }

  if(file.exists(m.l.name)) {
    load(file=m.r.name)
    m1.l <- m.t
    rm(m.t)
    p.test <- try(compute(m1.l,data.frame(px=0,pz.ratio=0))$net.result)
    if (class(p.test) == "try-error") {
      message(paste("Bad model",id,year,month,"L"))
    } else {
      m1.l.ok <- TRUE
    }
  }
  if(file.exists(m.r.gen.name)) {
    load(file=m.r.gen.name)
    mg.r <- m.t
    rm(m.t)
    p.test <- try(compute(mg.r,data.frame(px=0,pz.ratio=0))$net.result)
    if (class(p.test) == "try-error") {
      message(paste("Bad model","generic",year,month,"R"))
    } else {
      mg.r.ok <- TRUE
    }
  }
  if(file.exists(m.l.gen.name)) {
    load(file=m.l.gen.name)
    mg.l <- m.t
    rm(m.t)
    p.test <- try(compute(mg.l,data.frame(px=0,pz.ratio=0))$net.result)
    if (class(p.test) == "try-error") {
      message(paste("Bad model","generic",year,month,"L"))
    } else {
      mg.l.ok <- TRUE
    }
  }
  if(file.exists(m.r.gen.pdata.name)) {
    load(file=m.r.gen.pdata.name)
    mp.r <- m.t
    rm(m.t)
    p.test <- try(compute(mp.r,data.frame(px=0,pz.ratio=0,pfx_x=0,pfx_z=0))$net.result)
    if (class(p.test) == "try-error") {
      message(paste("Bad model","generic.pitchdata",year,month,"R"))
    } else {
      mp.r.ok <- TRUE
    }
  }
  if(file.exists(m.l.gen.pdata.name)) {
    load(file=m.l.gen.pdata.name)
    mp.l <- m.t
    rm(m.t)
    p.test <- try(compute(mp.l,data.frame(px=0,pz.ratio=0,pfx_x=0,pfx_z=0))$net.result)
    if (class(p.test) == "try-error") {
        message(paste("Bad model","generic.pitchdata",year,month,"L"))
    } else {
      mp.l.ok <- TRUE
    }
  }
  if(file.exists(m.r.roeg.name)) {
    mr.r.ok <- TRUE
  }
  if(file.exists(m.l.roeg.name)) {
    mr.l.ok <- TRUE
  }

  # Apply each model to data
  # Apply individual, non-regressed model (use generic model if model doesn't exist)
  if(m1.r.ok & !generic) {
    n.r <- nrow(m1.r$data)
    dt.r$n <- n.r
    dt.r$nn_ind <- with(dt.r, compute(m1.r,data.frame(px=px,pz.ratio=pz.ratio)))$net.result
  } else if(mg.r.ok) {
    n.r <- 0
    dt.r$nn_ind <- with(dt.r, compute(mg.r,data.frame(px=px,pz.ratio=pz.ratio)))$net.result
  } else {
    n.r <- 0
    dt.r$nn_ind <- -999
    message(paste("Bad model:",m.r.name))
  }
  dt.r$n <- n.r
  if(m1.l.ok & !generic) {
    n.l <- nrow(m1.l$data)
    dt.l$nn_ind <- with(dt.l, compute(m1.l,data.frame(px=px,pz.ratio=pz.ratio)))$net.result
  } else if(mg.l.ok) {
    n.l <- 0
    dt.l$nn_ind <- with(dt.l, compute(mg.l,data.frame(px=px,pz.ratio=pz.ratio)))$net.result
  } else {
    n.l <- 0
    dt.l$nn_ind <- -999
    message(paste("Bad model:",m.l.name))
  }
  dt.l$n <- n.l

  # Apply generic nn model
  if(mg.r.ok) {
    dt.r$nn_gen <- with(dt.r, compute(mg.r,data.frame(px=px,pz.ratio=pz.ratio)))$net.result
  } else {
    dt.r$nn_gen <- -999
    message(paste("Bad model:",m.r.gen.name))
  }
  if(mg.l.ok) {
    dt.l$nn_gen <- with(dt.l, compute(mg.l,data.frame(px=px,pz.ratio=pz.ratio)))$net.result
  } else {
    dt.l$nn_gen <- -999
    message(paste("Bad model:",m.l.gen.name))
  }

  # Apply generic pitchdata nn model
  if(mp.r.ok) {
    dt.r$nn_pdata <- with(dt.r, compute(mp.r,data.frame(px=px,pz.ratio=pz.ratio,pfx_x=pfx_x,pfx_z=pfx_z)))$net.result
  } else {
    dt.r$nn_pdata <- -999
    message(paste("Bad model:",m.r.gen.pdata.name))
  }
  if(mp.l.ok) {
    dt.l$nn_pdata <- with(dt.l, compute(mp.l,data.frame(px=px,pz.ratio=pz.ratio,pfx_x=pfx_x,pfx_z=pfx_z)))$net.result
  } else {
    dt.l$nn_pdata <- -999
    message(paste("Bad model:",m.l.gen.pdata.name))
  }

  # Apply regressed individual model
  # dt.r$nn_regress <- ifelse(is.null(dt.r$nn_ind) || is.null(dt.r$nn_gen), NULL, (n.r*dt.r$nn_ind + regress*dt.r$nn_gen)/(n.r+regress))
  # dt.l$nn_regress <- ifelse(is.null(dt.l$nn_ind) || is.null(dt.l$nn_gen), NULL, (n.l*dt.l$nn_ind + regress*dt.l$nn_gen)/(n.l+regress))
  dt.r$nn_regress <- (n.r*dt.r$nn_ind + regress*dt.r$nn_gen)/(n.r+regress)
  dt.l$nn_regress <- (n.l*dt.l$nn_ind + regress*dt.l$nn_gen)/(n.l+regress)

  # Apply Roegele-based model
  if(mr.r.ok) {
    dt.r$roeg <- mapply(eval.roegele.from.file.f, m.r.roeg.name, dt.r$px, dt.r$pz)
  } else {
    dt.r$roeg <- -999
    message(paste("Bad model:",m.r.roeg.name))
  }
  if(mr.l.ok) {
    dt.l$roeg <- mapply(eval.roegele.from.file.f, m.l.roeg.name, dt.l$px, dt.l$pz)
  } else {
    dt.l$roeg <- -999
    message(paste("Bad model:",m.l.roeg.name))
  }

  # Merge left & right data tables
  dt.o <- rbind(dt.r, dt.l)

  # Save results to database
  db.result <- NULL
  if(!is.null(dt.o)) {
      dt.out <- data.table(gamedayPitchID=dt.o$gamedayPitchID,
                           umpire=dt.o$id,
                           date=dt.o$date,
                           n=dt.o$n,
                           nn_ind=dt.o$nn_ind,
                           nn_gen=dt.o$nn_gen,
                           nn_pdata=dt.o$nn_pdata,
                           nn_regress=dt.o$nn_regress,
                           roeg=dt.o$roeg,
                           c=dt.o$s.f,
                           stand=dt.o$stand)
      db.result <- roc.db.write.f(dt.out)
  }
  return(db.result)

}

roc.db.write.f <- function(dt.out.t) {
    result  <- FALSE
    mydb    <- dbConnect(dbDriver("MySQL"),user="bbos",password="bbos",host="localhost",dbname="gameday")
    fields.l <- list(gamedayPitchID="int",
                     umpire="int",
                     date="date",
                     n="int",
                     nn_ind="double",
                     nn_gen="double",
                     nn_pdata="double",
                     nn_regress="double",
                     roeg="double",
                     c="smallint",
                     stand="varchar")

    p.try   <- try(dbWriteTable(mydb,roc.dbtable,dt.out.t,field.types=fields.l,overwrite=FALSE,append=TRUE,row.names=FALSE))
    if (class(p.try) != "try-error") {
        result <- TRUE
    }

    dbDisconnect(mydb)
    return(result)
}

sqlString <- paste("DELETE FROM",roc.dbtable,";")
mydb    <- dbConnect(dbDriver("MySQL"),user="bbos",password="bbos",host="localhost",dbname="gameday")
dbSendQuery(mydb,sqlString)
dbDisconnect(mydb)

# Test that it works
# score.pitch.nn.f(503077,2015,month=6,generic=FALSE,regress=900)

dt.roc$roc  <- mcmapply(score.pitch.nn.f,
                        dt.roc$id,
                        dt.roc$year,
                        month=dt.roc$month,
                        generic=FALSE,
                        regress=900,
                        mc.preschedule=TRUE,
                        mc.set.seed=TRUE,
                        mc.silent=FALSE,
                        mc.cores=getOption("mc.cores",20L),
                        mc.cleanup=TRUE)
