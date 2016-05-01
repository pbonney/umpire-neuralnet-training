library(data.table)
library(RMySQL)

x1 <- seq(-2,2,by=0.025)
z1 <- seq(-2,2,by=0.025)
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
                        AND     a.b_height_in is not null
                        AND     a.b_height_in!=0
                        AND     p.px is not null
                        AND     p.pz is not null"
        if(id != -1) { sqlString <- paste(sqlString,"AND u.id=",id) }
        if(stand %in% c('R','L')) { sqlString <- paste(sqlString," AND a.stand='",stand,"' ",sep="") }
        if(incl.spring) { sqlString <- paste(sqlString,"AND g.type IN ('R','S')") }
        else        { sqlString <- paste(sqlString,"AND g.type='R'") }
        sqlString <- paste(sqlString," AND STR_TO_DATE(concat(substr(a.gameName,5,4),'-',substr(a.gameName,10,2),'-',substr(a.gameName,13,2)), '%Y-%m-%d') < '",d.e,"'",sep="")
        sqlString <- paste(sqlString," AND STR_TO_DATE(concat(substr(a.gameName,5,4),'-',substr(a.gameName,10,2),'-',substr(a.gameName,13,2)), '%Y-%m-%d') >= '",d.s,"'",sep="")
        sqlString <- paste(sqlString,"ORDER BY concat(substr(a.gameName,5,4),'-',substr(a.gameName,10,2),'-',substr(a.gameName,13,2)) DESC, p.gamedayPitchID DESC")
}

# Function to calculate the are of the 50%+ strike zone by Jon Roegle's method
# (which is to split the home plate area up into 1 inch squares and count up
# how many of them had pitches called a strike > 50% of the time)
sz.area.roegle.f <- function(id = -1, d.s = as.Date("2006-01-01"), d.e = as.Date("2006-01-02"),
                stand = "B", incl.spring = FALSE) {
    sqlString <- pitch.query(id=id,d.s=d.s,d.e=d.e,stand=stand,incl.spring=incl.spring)

    mydb <- dbConnect(dbDriver("MySQL"),user="bbos",password="bbos",host="localhost",dbname="gameday")
    rs <- dbSendQuery(mydb,sqlString)
    dt <- fetch(rs,-1)
    dbDisconnect(mydb)

    dt$s.f <- as.numeric(dt$des == "Called Strike")
    dt$grid.x <- floor(dt$px*12)
    dt$grid.z <- floor(dt$pz*12)

    dt.grid2$count <- nrow(dt[dt$grid.x==dt.grid2$x & dt$grid.z==dt.grid2$z,])
    dt.grid2$strike <- nrow(dt[dt$grid.x==dt.grid2$x & dt$grid.z==dt.grid2$z & dt$s.f==1,])

    dt.grid2$ratio <- dt.grid2$strike/dt.grid2$count
    area <- nrow(dt.grid2[dt.grid2$ratio>0.5,])

    return(area)
}
