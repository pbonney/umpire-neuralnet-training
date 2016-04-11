#!/opt/local/bin/Rscript

library(neuralnet)
library(parallel)
library(data.table)
library(RMySQL)

args <- commandArgs(trailingOnly = TRUE)

year <- ifelse(length(args)>0, args[1], 2016)

eval.dbtable <- "umpire_ucs_generic"

ucs.f <- function(p,c) {
    ucs <- (2*p-1)*(c-p)
    return(ucs)
}

sqlStringUmpList <- sqlImport("./umpire_eval_list.sql")
mydb            <- dbConnect(dbDriver("MySQL"),user="bbos",password="bbos",host="localhost",dbname="gameday")
rs.umplist      <- dbSendQuery(mydb,sqlStringUmpList)
df.umplist      <- fetch(rs.umplist,-1)
dbDisconnect(mydb)

dt.ucs      <- data.table(df.umplist)
rm(df.umplist)

ump.eval.year.generic.f <- function(id, year) {
    eval.r <- 0
    eval.l <- 0

    dt.o <- NULL

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

    mydb    <- dbConnect(dbDriver("MySQL"),user="bbos",password="bbos",host="localhost",dbname="gameday")
    rs      <- dbSendQuery(mydb,sqlString)
    df      <- fetch(rs,-1)
    dbDisconnect(mydb)

    dt      <- data.table(df)

    dt$pz.ratio <- (dt$pz-dt$sz_mid)/((dt$sz_top-dt$sz_bot)/2)
    dt$s.f  <- as.numeric(dt$des %in% "Called Strike")

    dt.r    <- dt[dt$stand=="R",]
    dt.l    <- dt[dt$stand=="L",]

    start   <- paste(year,"01","01",sep="-")
    end     <- paste(year,"12","31",sep="-")

    m.r.name <- paste("./models.umpire/generic",start,end,"R","rda",sep=".")
    m.l.name <- paste("./models.umpire/generic",start,end,"L","rda",sep=".")

    if(file.exists(m.r.name)) {
        load(file=m.r.name)
        m1.r <- m.t
        rm(m.t)
        p.test <- try(compute(m1.r,data.frame(px=0,pz.ratio=0))$net.result)
        if (class(p.test) == "try-error") {
            cat(paste(id,year,month,"R"),file=log,sep="\n",append=TRUE)
        } else {
            if(!is.numeric(m1.r$result.matrix[1])) { ucs.r <- 0 }
            else if(m1.r$result.matrix[1]==0) { ucs.r <- 0 }
            else {
                dt.r$p  <- with(dt.r, compute(m1.r,data.frame(px=px,pz.ratio=pz.ratio)))$net.result
                dt.r$ucs <- mapply(ucs.f,dt.r$p,dt.r$s.f)
                dt.o <- dt.r
            }
        }
    }

    if(file.exists(m.l.name)) {
        load(file=m.l.name)
        m1.l <- m.t
        rm(m.t)
        p.test <- try(compute(m1.l,data.frame(px=0,pz.ratio=0))$net.result)
        if (class(p.test) == "try-error") {
            cat(paste(id,year,month,"L"),file=log,sep="\n",append=TRUE)
        } else {
            if(!is.numeric(m1.l$result.matrix[1])) { ucs.l <- 0 }
            else if(m1.l$result.matrix[1]==0) { ucs.l <- 0 }
            else {
                dt.l$p  <- with(dt.l, compute(m1.l,data.frame(px=px,pz.ratio=pz.ratio)))$net.result
                dt.l$ucs <- mapply(ucs.f,dt.l$p,dt.l$s.f)
                if(!is.null(dt.o)) { dt.o <- rbind(dt.o,dt.l) } else { dt.o <- dt.l }
            }
        }
    }

    db.result <- NULL
    if(!is.null(dt.o)) {
        dt.out <- data.table(gamedayPitchID=dt.o$gamedayPitchID,umpire=dt.o$id,date=dt.o$date,p=dt.o$p,ucs=dt.o$ucs)
        db.result <- ucs.db.write.f(dt.out)
    }
    return(db.result)
}

ucs.db.write.f <- function(dt.out.t) {
    result  <- FALSE
    mydb    <- dbConnect(dbDriver("MySQL"),user="bbos",password="bbos",host="localhost",dbname="gameday")
    fields.l <- list(gamedayPitchID="int",umpire="int",date="date",p="double",ucs="double")

    p.try   <- try(dbWriteTable(mydb,eval.dbtable,dt.out.t,field.types=fields.l,overwrite=FALSE,append=TRUE,row.names=FALSE))
    if (class(p.try) != "try-error") {
        result <- TRUE
    }

    dbDisconnect(mydb)
    return(result)
}

dt.ucs$ucs <- mapply(ump.eval.year.generic.f,dt.ucs$id,dt.ucs$year)
# dt.ucs$ucs  <- mcmapply(ump.eval.year.generic.f,dt.ucs$id,dt.ucs$year,
#        mc.preschedule=TRUE,mc.set.seed=TRUE,mc.silent=FALSE,mc.cores=getOption("mc.cores",20L),mc.cleanup=TRUE)

