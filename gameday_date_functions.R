library(RMySQL)
library(data.table)

# Gets the min and max date for regular season games in a given year.
get.date.range.f <- function(year) {
	sqlString <- "SELECT min(STR_TO_DATE(concat(substr(gameName,5,4),'-',substr(gameName,10,2),'-',substr(gameName,13,2)), '%Y-%m-%d')) as min_date,
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

# Get first and last game date in each month for each season
min.y <- 2007
max.y <- 2017

y.l <- min.y:max.y
m.l <- mapply(get.date.range.f,y.l)
dt.y <- data.table(year=y.l,min=m.l[1,],max=m.l[2,])

dt.y$has.mar.games <- ifelse(month(dt.y$min)<4,TRUE,FALSE)
dt.y$has.oct.games <- ifelse(month(dt.y$max)>9,TRUE,FALSE)

month.first.day.list.f <- function(year,min.date,has.mar.games,has.oct.games) {
    s1.l <- ifelse(has.mar.games,
                   c(paste(year,3,as.POSIXlt(min.date)$mday,sep="-"),paste(year,4,1,sep="-")),
                   c(paste(year,4,as.POSIXlt(min.date)$mday,sep="-")))
    s2.l <- c(paste(year,5,1,sep="-"),paste(year,6,1,sep="-"),paste(year,7,1,sep="-"),paste(year,8,1,sep="-"))
    s3.l <- ifelse(has.oct.games,
                   c(paste(year,9,1,sep="-"),paste(year,10,1,sep="-")),
                   c(paste(year,9,1,sep="-")))
    start.l <- c(s1.l,s2.l,s3.l)
    return(start.l)
}

month.last.day.list.f <- function(year,max.date,has.mar.games,has.oct.games) {
    e1.l <- ifelse(has.mar.games,
                   c(paste(year,3,31,sep="-"),paste(year,4,30,sep="-")),
                   c(paste(year,4,30,sep="-")))
    e2.l <- c(paste(year,5,31,sep="-"),paste(year,6,30,sep="-"),paste(year,7,31,sep="-"),paste(year,8,31,sep="-"))
    e3.l <- ifelse(has.oct.games,
                   c(paste(year,9,30,sep="-"),paste(year,10,as.POSIXlt(max.date)$mday,sep="-")),
                   c(paste(year,9,as.POSIXlt(max.date)$mday,sep="-")))
    end.l <- c(e1.l,e2.l,e3.l)
    return(end.l)
}

month.last.day.f <- function(month) {
  last = ifelse(month %in% c(3,5,7,8,10), 31, 30)
  return(last)
}
