library(parallel)
library(neuralnet)
library(data.table)
# source("batter_nn.R")

err.threshold <- 0.1

try.model.f <- function(m.temp) {
    if (length(m.temp) < 2) {
        return(FALSE)
    }
    if (!is.numeric(m.temp$result.matrix[1])) {
        return(FALSE)
    }
    p.test <- try(compute(m.temp,data.frame(px=0,pz=6))$net.result)
    if (class(p.test) == "try-error") {
        return(FALSE)
    } else if (p.test > err.threshold) {
        return(FALSE)
    }
    if (try(compute(m.temp,data.frame(px=3, pz=2.25))$net.result) > err.threshold) { return(FALSE) }
    if (try(compute(m.temp,data.frame(px=-3, pz=2.25))$net.result) > err.threshold) { return(FALSE) }
    if (try(compute(m.temp,data.frame(px=-2, pz=6))$net.result) > err.threshold) { return(FALSE) }
    if (try(compute(m.temp,data.frame(px=2, pz=6))$net.result) > err.threshold) { return(FALSE) }
    if (try(compute(m.temp,data.frame(px=-2, pz=1))$net.result) > err.threshold) { return(FALSE) }
    if (try(compute(m.temp,data.frame(px=2, pz=1))$net.result) > err.threshold) { return(FALSE) }

    return(TRUE)
}

try.model.wrapper.f <- function(filepath) {
    result <- FALSE
    if(file.exists(filepath)) {
        load(file=filepath)
        m1 <- m.bat
        rm(m.t)
        result <- try.model.f(m1)
    }
    return(result)
}

find.bad.batters.f <- function(dir) {
    models.l <- dir(path=dir, pattern="*rda")
    dt.result <- data.table(file=paste(dir,models.l,sep="/"))
#    dt.result$test <- lapply(dt.result$file, try.model.wrapper.f)
    dt.result$test <- mcmapply(try.model.wrapper.f, dt.result$file,
                               mc.cores=getOption("mc.cores",12L))
    return(dt.result[dt.result$test==FALSE,])
}

fix.bad.batters.f <- function(dir) {
  dt <- fix.bad.batters.helper.f(dir, FALSE)
  return(dt)
}

fix.bad.batters.parallel.f <- function(dir) {
  dt <- fix.bad.batters.helper.f(dir, TRUE)
  return(dt)
}

fix.bad.batters.helper.f <- function(dir, parallel) {
  dt.fix <- find.bad.models.f(dir)
  n <- nchar(dir) + nchar("/nn.bat.") + 1
  k <- n + 5 # n to n+5 for batter id
  l <- k + 2 # move over 2 for year
  m <- l + 3 # l to l+3 for year
  dt.fix$id <- substr(dt.fix$file,n,k)
  dt.fix$year <- substr(dt.fix$file,l,m)

  # remove bad models
  mapply(file.remove, dt.fix$file)

  if(parallel) {
    dt.fix$fixed <- mcmapply(bat.model.f,dt.fix$id,dt.fix$year,
      mc.preschedule=TRUE,mc.set.seed=TRUE,mc.silent=FALSE,mc.cores=getOption("mc.cores",20L),mc.cleanup=TRUE)
  } else {
    dt.fix$fixed <- mapply(bat.model.f,dt.fix$id,dt.fix$year)
  }

  return(dt.fix)
}
