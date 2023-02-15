
frontier <- function(er, vcv, rf = 0.05, ticks = NULL, rp = seq(0, .25, by = .005)) {
    ## tickers be either integers or characters, depending on the
    ## structure of the covariance matrix. Algebra follows Petters and Dong
    ## notation, sect 3.3. For tangent portfolio, see also
    ## https://bookdown.org/compfinezbook/introcompfinr/Efficient-portfolios-of.html
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
    tangentportfolio = (vcvinv %*% (er- rf*ones)) / (t(ones) %*% vcvinv %*% (er-rf*ones))[1,1]
    ertport <- t(tangentportfolio) %*% er
    vartport <- t(tangentportfolio) %*% vcv %*% tangentportfolio
    list(rp = rp, v = v, sd = sqrt(v), weights = wts,
         N = factor(length(er)),
         symbol = rownames(vcv), er = er, vcv = vcv, mvp_var = 1/A,
         mvp_mean = B/A, mvp_wts = (vcvinv %*% ones)/A,
         tport = tangentportfolio,
         ertport = ertport, vartport = vartport)
    }


plotfrontier <- function(er, vcv, ticks = NULL, labelxadj = 0.0125, labelyadj = 0,
                         rp = seq(0, .15, by = .005)) {
    f <- frontier(er,  vcv,  ticks, rp)
    minvar  <- tibble(rp = f$rp,  v = f$sd )
    stkinfo <- tibble(means = f$er,  vars = diag(f$vcv),
                      nms = f$symbol)
    p <- ggplot(minvar,  aes(x = v,  y = rp)) +
        geom_point() +
        geom_path() +
        geom_point(data = stkinfo,
                   aes(x = vars, y = means), cex = 5) +
        annotate('text', label = stkinfo$nms, x = stkinfo$vars+0.1, #labelxadj*max(f$v),
                 y = stkinfo$means, cex = 7)
    return(list(plot = p,  frontier_info = f))
}


