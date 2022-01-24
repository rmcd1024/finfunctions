#' @title pvfunctions
#'
#' @description Experimental implemention of basic present value spreadsheet
#'     functions. The functions `pv`, `fv`, `pmt`, `ipmt`, `ppmt`, deal
#'     with annuities (reqularly repeating payments). The functions
#'     `irr` and `npv` deal with unequal cash flows, with `npv`
#'     assuming the first payment occurs in one period. Specifically,
#' * `pv` and `fv` compute the present and future value of an
#'     annuity. The optional argument (which defaults to 0) permits an
#'     ongoing annuity to have a current balance (with the `fv`
#'     function) or for there to be a future balance (with the `pv`
#'     function)
#' * `pmt`, `ipmt`, and `ppmt` compute the annuity
#'     payment, and for any given payment the breakdown between
#'     interest and principal
#' * `npv` and `irr` accept a vector as input to handle cash flows that are not annuities.
#' * These functions should adhere to the sign conventions a spreadsheet user would expect. 
#'
#'
#' \code{pv(rate,nper,pymt,0 )}
#' \code{pv(rate,nper,pymt,fvamt)}
#' \code{fv(rate,nper,pymt,0 )}
#' \code{fv(rate,nper,pymt,pvamt)}
#' \code{npv(rate,values )}
#' \code{irr(values2, lower = -0.5, upper = 1.5, tolerance = .Machine$double.eps^0.25)}
#' \code{pmt(rate,nper,pvamt, fvamt=0)}
#' \code{ipmt(rate, period, nper, pvamt)}
#' \code{ppmt(rate, period,nper, pvamt)}
#'
#' @name pvfunctions
#' @importFrom stats uniroot
#' @param rate effective per-period interest rate
#' @param nper number of periods
#' @param pymt periodic payment (e.g. annuity)
#' @param fvamt future value amount not accounted for by periodic
#'     payment. 
#' @param pvamt present amount (e.g. amt to be annuitized); in case of `FV`, specifies current amount paid on annuity
#' @param period specific period for which calculation is to be
#'     performed (e.g. interest component of annuity)
#' @param values vector of payments
#' @param lower lower bound of search for irr
#' @param upper upper bound of search for irr
#' @param tolerance convergence tolerance for irr. Set to `uniroot` default
#' @aliases pv fv npv irr pmt ipmt ppmt
#'
#' @usage
#' pv(rate,nper,pymt,fvamt)
#' npv(rate, values)
#' irr(values, lower = -0.5, upper = 1.5, tolerance = .Machine$double.eps^0.25)
#' pmt(rate,nper,pvamt, fvamt)
#' ipmt(rate, period, nper, pvamt, fvamt)
#' ppmt(rate, period,nper, pvamt, fvamt)
#'
#' @details These functions (mostly) mimic the behavior of the
#'     standard basic financial function in Excel, Google Sheets, and
#'     Libreoffice. With the exception of \code{irr}, they are
#'     vectorized.

#' @export
pv <- function(rate, nper, pymt, fvamt = 0) {
    pv <- pymt/rate*(1-(1+rate)^(-nper)) + fvamt*(1+rate)^(-nper)
    -pv
}

#' @export
fv <- function(rate, nper, pymt, pvamt = 0) {
    fv <- pymt/rate*((1+rate)^nper - 1) + pvamt*(1+rate)^nper
    -fv
}

#' @export
npv <- function(rate, values) {
    ## Assumes equally-spaced payments
    tt <- 1:length(values)
    print(tt)
    npv <- sum(values*(1+rate)^(-tt))
    npv
}

#' @export
irr <- function(values, lower = -0.5, upper = 1.5,
    tolerance = .Machine$double.eps^0.25) {
    y <- uniroot(function(x) sum(values/(1+x)^(1:length(values))),
                 lower = lower, upper = upper, tol = tolerance)
    y$root
}

#' @export
pmt <- function(rate, nper, pvamt, fvamt = 0) {
    ## need to make a bigger payment if there is to be an FV residual
    pmt <- (pvamt + fvamt*(1+rate)^(-nper))*
        rate*(1+rate)^nper/((1+rate)^nper-1)
    -pmt
}

#' @export
ipmt <- function(rate, period, nper, pvamt, fvamt = 0) {
    pymt <- pmt(rate, nper, pvamt, fvamt)
    interest <- pv(rate, nper-period+1, pymt, fvamt)*rate
    -interest
}

#' @export
ppmt <- function(rate, period, nper, pvamt, fvamt = 0) {
    pymt <- pmt(rate, nper, pvamt, fvamt)
    principal <- pymt - ipmt(rate, period, nper, pvamt, fvamt)
    ## don't need negative because pymt is negative
    principal 
}

