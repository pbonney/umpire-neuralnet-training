message("Starting up R script")

library(RMySQL)
library(data.table)

# Constants
save_dir <- "./umpire_uzr"
area_prefix <- "sz_area_"
res_tot_prefix <- "results_tot_"
res_prefix <- "results_"
file_type <- ".txt"
model_dir <- "./models.umpire"

# Process:
# 0. Backup old umpire model files to directory
# 1. Update batter models (load batter_nn.R)
# 2. Repair any bad models (load fix_bad_batters, run fix.bad.models.f("./models.batter"))
# 3. Update database with batter strike zone data (load batter_sz.R)
# 4. Update umpire NN models (load umpire_generic_yearly.R)
# 5. Repair any bad models (load fix_bad_models, run fix.bad.models.f("./models.umpire"))
# 6. Calculate NN zone area (load sz_area.R, run dt.me <- area.all.years.f(min.year=2016, max.year=2016))
# 7. Calculate Roeg zone area (run dt.roeg <- roegele.all.years.f(min.year=2016, max.year=2016))
# 8. Calculate diff (diff <- dt.me$area - dt.roeg$area)
# 9. Save diff to file
# 10. Update umpire evaluations (load umpire_eval.R)
# 11. Run SQL queries to get umpire data
# 12. Save umpire data to file

# 0
backup_dir <- paste(paste(model_dir, "backup", sep="/"), format(Sys.time(), "%Y%m%d.%H%M%S"), sep=".")

cmd_mkdir <- paste("mkdir", backup_dir, sep=" ")
message(paste(Sys.time(),cmd_mkdir))
system(cmd_mkdir)

cmd_cp <- paste("cp", paste(model_dir, "generic.2016*", sep="/"), paste(backup_dir, "/", sep=""), sep=" ")
message(paste(Sys.time(),cmd_cp))
system(cmd_cp)

# 1
message(paste(Sys.time(),"Loading batter_nn.R"))
source("batter_nn.R")
message(paste(Sys.time(),"Done with batter_nn.R"))

# 2
message(paste(Sys.time(),"Loading fix_bad_batters.R"))
source("fix_bad_batters.R")
message(paste(Sys.time(),"Loaded - running fix.bad.models.f('./models.batter')"))
dt.fixbat <- find.bad.models.f("./models.batter")
if(nrow(dt.fixbat) > 0) { fix.bad.models.f("./models.batter") }
message(paste(Sys.time(),"Done with fixing batter models"))

# 3
message(paste(Sys.time(),"Loading batter_sz.R"))
source("batter_sz.R")
message(paste(Sys.time(),"Done with batter_sz.R"))

# 4
message(paste(Sys.time(),"Loading umpire_generic_yearly.R"))
source("umpire_generic_yearly.R")
message(paste(Sys.time(),"Done with umpire_generic_yearly.R"))

# 5
message(paste(Sys.time(),"Loading fix_bad_models.R"))
source("fix_bad_models.R")
message(paste(Sys.time(),"Loaded - running fix.bad.models.f('./models.umpire')"))
dt.fixump <- find.bad.models.f("./models.umpire")
if(nrow(dt.fixump) > 0) { fix.bad.models.f("./models.umpire") }
message(paste(Sys.time(),"Done with fixing umpire models"))

# 6
message(paste(Sys.time(),"Loading sz_area.R"))
source("sz_area.R")
message(paste(Sys.time(),"Calculating NN-based area"))
dt.me <- area.all.years.f(min.year=2016, max.year=2016)
message(paste(Sys.time(),"Done with NN-based area"))

# 7
message(paste(Sys.time(),"Calculating Roegele area"))
dt.roeg <- roegele.all.years.f(min.year=2016, max.year=2016)
message(paste(Sys.time(),"Done with Roegele area"))

# 8
dt.area <- data.table(year=dt.me$year, stand=dt.me$stand, my_area=dt.me$area, roegele_area=dt.roeg$area, diff=dt.me$area-dt.roeg$area)

# 9
message(paste(Sys.time(),"Saving area results"))
area_filename <- paste(area_prefix, Sys.Date(), file_type, sep="")
area_savestring <- paste(save_dir, area_filename, sep="/")
write.table(dt.area, file=area_savestring, quote=FALSE, row.names=FALSE)

# 10
message(paste(Sys.time(),"Loading umpire_eval.R"))
source("umpire_eval.R")
message(paste(Sys.time(),"Done with umpire_eval.R"))

# 11
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

# 12
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

message(paste(Sys.time(),"Done with R script"))
