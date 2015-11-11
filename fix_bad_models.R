library(parallel)
library(neuralnet)
library(data.table)

logfile <- "model_err.txt"

try.model.f <- function(m.temp) {
    if (!is.numeric(m.temp$result.matrix[1])) {
        return(FALSE)
    }
    p.test <- try(compute(m.temp,data.frame(px=0,pz.ratio=0))$net.result)
    if (class(p.test) == "try-error") {
        return(FALSE)
    }
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
    log <- paste(dir,logfile,sep="/")
    if(file.exists(log)) { file.remove(log) }

    models.l <- dir(path=dir, pattern="*rda")
    dt.result <- data.table(file=paste(dir,models.l,sep="/"))
    dt.result$test <- mcmapply(try.model.wrapper.f,dt.result$file,
                               mc.cores=getOption("mc.cores",20L))
    return(dt.result[dt.result$test==FALSE,])
}
