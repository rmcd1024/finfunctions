context('R version of spreadsheet functions')

rate <- 0.05
nper <- 20
pymt <- 2
fvamt <- 3
pvamt <- 110
period <- 5
values <- 1:5
values2 <- c(-10, values)


## values all checked against google sheets `finfunction` sheet

testthat::test_that("finfunctions satisfy simple tests", {
    expect_equal(pv(rate, nper, pymt, 0),  -24.92442069)
    expect_equal(pv(rate, nper, pymt, fvamt), -26.05508913)
    expect_equal(fv(rate, nper, pymt, 0), -66.13190821)
    expect_equal(fv(rate, nper, pymt, pvamt),  -357.9946558)
    expect_equal(npv(rate, values), 12.56639344)
    expect_equal(irr(values2, lower = 0, upper = .2) , 0.1200583767 )
    expect_equal(pmt(rate,nper,pvamt), -8.826684591)
    expect_equal(pmt(rate,nper,pvamt,fvamt), -8.917412353)
    expect_equal(ipmt(rate, period, nper, pvamt), -4.783078679)
    expect_equal(ipmt(rate, period, nper, pvamt, fvamt), -4.763526279)
    expect_equal(ppmt(rate, period,nper, pvamt), -4.043605912)
    expect_equal(ppmt(rate, period,nper, pvamt, fvamt), -4.153886073)
})
