library(RMySQL)
library(data.table)

save_dir <- "./umpire_uzr"
res_tot_prefix <- "results_tot_"
res_prefix <- "results_"
file_type <- ".txt"

message(paste(Sys.time(),"Loading umpire UZR queries"))
sqlStringUZR    <- sqlImport("./umpire_uzrish.sql")
sqlStringUZRyr  <- sqlImport("./umpire_uzrish_year.sql")

message(paste(Sys.time(),"Running individual UZR query"))
mydb            <- dbConnect(dbDriver("MySQL"),user="bbos",password="bbos",host="localhost",dbname="gameday")
rs.umpuzr       <- dbSendQuery(mydb,sqlStringUZR)
df.umpuzr       <- fetch(rs.umpuzr,-1)
dbDisconnect(mydb)

message(paste(Sys.time(),"Running aggregate UZR query"))
mydb            <- dbConnect(dbDriver("MySQL"),user="bbos",password="bbos",host="localhost",dbname="gameday")
rs.umpuzryr     <- dbSendQuery(mydb,sqlStringUZRyr)
df.umpuzryr     <- fetch(rs.umpuzryr, -1)
dbDisconnect(mydb)

message(paste(Sys.time(),"Queries done"))

message(paste(Sys.time(),"Saving individual ump-UZR results"))
res_filename <- paste(res_prefix, Sys.Date(), file_type, sep="")
res_savestring <- paste(save_dir, res_filename, sep="/")
write.table(df.umpuzr, file=res_savestring, quote=FALSE, row.names=FALSE)
message(paste(Sys.time(),"Saved individual ump-UZR results"))

message(paste(Sys.time(),"Saving aggregate ump-UZR results"))
res_tot_filename <- paste(res_tot_prefix, Sys.Date(), file_type, sep="")
res_tot_savestring <- paste(save_dir, res_tot_filename, sep="/")
write.table(df.umpuzryr, file=res_tot_savestring, quote=FALSE, row.names=FALSE)
message(paste(Sys.time(),"Saved aggregate ump-UZR results"))
