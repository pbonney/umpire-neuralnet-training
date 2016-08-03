library(RMySQL)
library(data.table)

message(paste(Sys.time(),"Loading batter_nn.R"))
source("/users/peter/Documents/baseball/analysis/umpires/batter_nn.R")
message(paste(Sys.time(),"Done running batter_nn.R"))

message(paste(Sys.time(),"Loading fix_bad_batters.R"))
source("/users/peter/Documents/baseball/analysis/umpires/fix_bad_batters.R")
message(paste(Sys.time(),"Loaded - running fix.bad.batters.f('./models.batter')"))
dt.fixbat <- find.bad.batters.f("/users/peter/Documents/baseball/analysis/umpires/models.batter")
if(nrow(dt.fixbat) > 0) {
  fix.bad.batters.f("/users/peter/Documents/baseball/analysis/umpires/models.batter")
  message(paste(Sys.time(),"Done with fixing batter models"))
} else {
  message(paste(Sys.time(),"No bad batter models to fix"))
}

message(paste(Sys.time(),"Loading batter_sz.R"))
source("/users/peter/Documents/baseball/analysis/umpires/batter_sz.R")
message(paste(Sys.time(),"Done running batter_sz.R"))
