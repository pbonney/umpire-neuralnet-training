library(parallel)
library(neuralnet)

logfile <- "model_err.txt"

try.model.f <- function(m.t) {
    if (!is.numeric(m.t$result.matrix[1])) {
        return(FALSE)
    }
    p.test <- try(compute(m.t,data.frame(px=0,pz.ratio=0))$net.result)
    if (class(p.test) == "try-error") {
        return(FALSE)
    }
    return(TRUE)
}
