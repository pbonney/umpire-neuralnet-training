library(data.table)
library(RMySQL)
library(parallel)

source("umpire_graph_functions.R")

by.c <- 1/40
x1 <- seq(-2,2,by=by.c)
z1 <- seq(-2,2,by=by.c)
x2 <- seq(-24,24,by=1)
z2 <- seq(12,72,by=1)

grid1 <- expand.grid(x=x1,z=z1)
grid2 <- expand.grid(x=x2,z=z2)
dt.grid1 <- data.table(x=grid1$x,z=grid1$z)
dt.grid2 <- data.table(x=grid2$x,z=grid2$z)
rm(grid1)
rm(grid2)

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

# Function to calculate the are of the 50%+ strike zone by Jon Roegle's method
# (which is to split the home plate area up into 1 inch squares and count up
# how many of them had pitches called a strike > 50% of the time)
sz.area.roegele.f <- function(id = -1, d.s = as.Date("2006-01-01"), d.e = as.Date("2006-01-02"),
                stand = "B", incl.spring = FALSE) {
    sqlString <- pitch.query(id=id,d.s=d.s,d.e=d.e,stand=stand,incl.spring=incl.spring)

    mydb <- dbConnect(dbDriver("MySQL"),user="bbos",password="bbos",host="localhost",dbname="gameday")
    rs <- dbSendQuery(mydb,sqlString)
    dt <- fetch(rs,-1)
    dbDisconnect(mydb)

    dt$n <- 1
    dt$s.f <- as.numeric(dt$des == "Called Strike")
    dt$grid.x <- floor(dt$px*12)
    dt$grid.z <- floor(dt$pz*12)

    dt.agg <- aggregate(cbind(n,s.f) ~ grid.x + grid.z, data = dt, sum)
    dt.agg$ratio <- dt.agg$s.f/dt.agg$n
    area <- nrow(dt.agg[dt.agg$ratio>0.5,])

    return(area)
}

roegele.all.years.f <- function(min.year=2008, max.year=2016) {
    y.l <- seq(min.year, max.year)
    dt.r <- data.table(year=y.l,stand="R")
    dt.l <- data.table(year=y.l,stand="L")
    dt <- rbind(dt.r,dt.l)
    dt$d.s <- as.Date(paste(dt$year,"01","01",sep="-"))
    dt$d.e <- as.Date(paste(dt$year,"12","31",sep="-"))
    
    dt$area <- mcmapply(sz.area.roegele.f, d.s=dt$d.s, d.e=dt$d.e, stand=dt$stand,
        mc.cores=getOption("mc.cores", 20L))

    dt.return <- data.table(year=dt$year, stand=dt$stand, area=dt$area)

    return(dt.return)
}

unadjusted.area.zone.f <- function(id = -1, d.s = as.Date("2006-01-01"), d.e = as.Date("2006-01-02"),
                stand = "B", incl.spring = FALSE) {
    nn <- load.zone("generic",d.s,d.e,stand)
    dt <- dt.grid1
    dt$p <- compute(nn,data.frame(px=dt$x,pz.ratio=dt$z))$net.result
    count <- nrow(dt[dt$p>0.5,])

    return(count)
}

area.adjustment.f <- function(year,stand) {
    sqlstring <- "  select  z.year,
                            a.stand,
                            count(*),
                            sum(z.sz_top)/count(*) as top,
                            sum(z.sz_bot)/count(*) as bot
                    from    gameday.atbats a,
                            gameday.pitches p,
                            gameday.batter_sz_top_bot z
                    where   a.GameName=p.GameName
                    and     a.num=p.gameAtBatID
                    and     a.batter=z.batter
                    and     substr(a.gameName,5,4)=z.year
                    and     (p.des like 'Called%' or p.des like 'Ball%')"
    sqlstring <- paste(sqlstring," and z.year=",year,sep="")
    sqlstring <- paste(sqlstring," and a.stand='",stand,"'",sep="")
    sqlstring <- paste(sqlstring," group by 1,2;",sep="")

    mydb <- dbConnect(dbDriver("MySQL"),user="bbos",password="bbos",host="localhost",dbname="gameday")
    rs <- dbSendQuery(mydb,sqlstring)
    dt <- fetch(rs,-1)
    dbDisconnect(mydb)

    dt$height <- dt$top-dt$bot
    return(dt$height[1])
}

area.all.years.f <- function(min.year=2008, max.year=2016) {
    y.l <- seq(min.year, max.year)

    dt.r <- data.table(year=y.l,stand="R")
    dt.l <- data.table(year=y.l,stand="L")
    
    dt <- rbind(dt.r,dt.l)
    dt$d.s <- as.Date(paste(dt$year,"01","01",sep="-"))
    dt$d.e <- as.Date(paste(dt$year,"12","31",sep="-"))
    
    dt$unadjusted <- mcmapply(unadjusted.area.zone.f, d.s=dt$d.s, d.e=dt$d.e, stand=dt$stand,
        mc.cores=getOption("mc.cores", 20L))

    dt$height <- mcmapply(area.adjustment.f, dt$year, dt$stand,
        mc.cores=getOption("mc.cores", 20L))

    # The vertical dimension of the zone is scaled such that +1 is at the top and -1 at the bottom
    # Thus the real-world height of each box, with scaled height by.c, is by.c * (sz_top-sz_bot)/2
    # Therefore each box in the grid has is by.c feet wide, and by.c * (sz_top-sz_bot)/2 feet high
    # (multiply by 144 for square inches)
    dt$area <- dt$unadjusted * by.c * by.c * dt$height/2 * 144

    dt.return <- data.table(year=dt$year, stand=dt$stand, area=dt$area)

    return(dt.return)
}