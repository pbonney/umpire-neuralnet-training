library(RMySQL)
library(data.table)

message(paste(Sys.time(),"Loading fix_bad_models.R"))
source("fix_bad_models.R")
message(paste(Sys.time(),"Loaded - running fix.bad.models.f('./models.umpire')"))
dt.fixump <- find.bad.models.f("/users/peter/Documents/baseball/analysis/umpires/models.umpire")
if(nrow(dt.fixump) > 0) {
  fix.bad.models.f("/users/peter/Documents/baseball/analysis/umpires/models.umpire")
  message(paste(Sys.time(),"Done with fixing umpire models"))
} else {
  message(paste(Sys.time(),"No bad umpire models to fix"))
}
