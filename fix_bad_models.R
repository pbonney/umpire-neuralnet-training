library(parallel)
library(neuralnet)
library(data.table)
library(RMySQL)

ump.err.threshold <- 0.8

try.model.f <- function(m.temp) {
    if (length(m.temp) < 2) {
        return(FALSE)
    }
    if (!is.numeric(m.temp$result.matrix[1])) {
        return(FALSE)
    }
    p.test <- try(compute(m.temp,data.frame(px=0,pz.ratio=0))$net.result)
    if (class(p.test) == "try-error") {
        return(FALSE)
    } else if (p.test < ump.err.threshold) { return(FALSE) }
    return(TRUE)
}

try.model.wrapper.f <- function(filepath) {
    result <- FALSE
    if(file.exists(filepath)) {
        load(file=filepath)
        m1 <- m.t
        rm(m.t)
        result <- try.model.f(m1)
    }
    return(result)
}

find.bad.models.f <- function(dir) {
    models.l <- dir(path=dir, pattern="*rda")
    dt.result <- data.table(file=paste(dir,models.l,sep="/"))
    dt.result$test <- lapply(dt.result$file, try.model.wrapper.f)
#    dt.result$test <- mcmapply(try.model.wrapper.f, dt.result$file,
#                               mc.cores=getOption("mc.cores",20L))
    return(dt.result[dt.result$test==FALSE,])
}

fix.bad.models.f <- function(dir) {
    dt.fix <- find.bad.models.f(dir)
    n <- nchar(dir) + nchar("/generic.") + 1
    k <- n + 9 # n to n+9 for start date
    l <- k + 2 # move over 2 for next date
    m <- l + 9 # l to l+3 for end date
    c <- m + 2 # move over 2 for batter hand
    dt.fix$d.s <- as.Date(substr(dt.fix$file,n,k))
    dt.fix$d.e <- as.Date(substr(dt.fix$file,l,m))
    dt.fix$stand <- substr(dt.fix$file,c,c)

    # remove bad models
    mapply(file.remove, dt.fix$file)

    dt.fix$fixed <- mcmapply(ump.train.and.save.f,d.s=as.Date(dt.fix$d.s),d.e=as.Date(dt.fix$d.e),
        stand=dt.fix$stand,pitch.limit=25000,
        mc.preschedule=TRUE,mc.set.seed=TRUE,mc.silent=FALSE,mc.cores=getOption("mc.cores",20L),mc.cleanup=TRUE)

    return(dt.fix)
}


