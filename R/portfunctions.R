
frontier <- function(er, vcv, ticks = NULL, rp = seq(0, .25, by = .005)) {
    ## tickers be either integers or characters, depending on the
    ## structure of the covariance matrix
    ## follows Petters and Dong notation, sect 3.3
    if (!is.null(ticks)) {
        vcv <- vcv[ticks, ticks]
        er <- er[ticks]
    } else {
        ticks <- colnames(vcv)
    }
    ones <- rep(1, nrow(vcv))
    vcvinv <- solve(vcv)
    A <- (t(ones) %*% vcvinv %*% ones)[[1]]
    B <- (t(er) %*% vcvinv %*% ones)[[1]]
    C <- (t(er) %*% vcvinv %*% er)[[1]]
    v <- (A*rp^2 - 2*B*rp + C) /(A*C - B^2)
    wtsfn <- function(rpi) {
        ((C - rpi*B)*vcvinv %*% ones + (rpi*A - B)*vcvinv %*% er)/(A*C - B^2)
    }
    wts <- Map(wtsfn, rp)
    wts <- do.call(cbind,  wts)
    rownames(wts) <- ticks
    wts <- rbind(rp, wts)
    list(rp = rp, v = v, sd = sqrt(v), weights = wts,
         N = factor(length(er)),
         symbol = colnames(vcv), er = er, vcv = vcv, mvp_var = 1/A,
         mvp_mean = B/A, mvp_wts = (vcvinv %*% ones)/A)
    }


plotfrontier <- function(er, vcv, ticks = NULL, labelxadj = 0.0125, labelyadj = 0,
                         rp = seq(0, .15, by = .005)) {
    f <- frontier(er,  vcv,  ticks, rp)
    minvar  <- tibble(rp = f$rp,  v = f$v, sd = f$sd)
    stkinfo <- tibble(means = f$er,  vars = diag(f$vcv),
                      sd = sqrt(diag(f$vcv)), nms = f$symbol)
    p <- ggplot(minvar,  aes(x = sd,  y = rp)) +
        geom_point() +
        geom_path() +
        geom_point(data = stkinfo,
                   aes(x = sd, y = means), cex = 5) +
        annotate('text', label = stkinfo$nms, x = stkinfo$sd+0.1, #labelxadj*max(f$v),
                 y = stkinfo$means, cex = 7)
    return(list(plot = p,  frontier_info = f))
}

testcase <- function() {
    er <- c('KO' = 0.07,  'TSLA' = 0.12)
    vcv <- matrix(c(.04, .001, .001, .25), nrow = 2)
    colnames(vcv) <- names(er)
    plotfrontier(er,  vcv)
}
