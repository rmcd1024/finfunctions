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
    expect_equal(pv(rate, nper, pymt,  0, type = 0),  -24.92442069)
    expect_equal(pv(rate, nper, pymt, fvamt, type = 0), -26.05508913)
    expect_equal(pv(rate, nper, pymt, 0, 1),  -26.1706417193)
    expect_equal(pv(rate, nper, pymt, fvamt, 1), -27.3013101680)

    expect_equal(fv(rate, nper, pymt, 0, 0), -66.13190821)
    expect_equal(fv(rate, nper, pymt, pvamt, 0),  -357.9946558)
    expect_equal(fv(rate, nper, pymt, 0, 1), -69.4385036161)
    expect_equal(fv(rate, nper, pymt, pvamt, 1), -361.3012511820)

    expect_equal(npv(rate, values), 12.56639344)

    expect_equal(irr(values2, lower = 0, upper = .2) , 0.1200583767 )

    expect_equal(pmt(rate, nper, pvamt, 0, 0), -8.826684590976)
    expect_equal(pmt(rate, nper, pvamt, fvamt, 0), -8.917412353)
    expect_equal(pmt(rate, nper,  0, fvamt, 0), -0.090727761572)
    expect_equal(pmt(rate, nper, pvamt, 0, 1), -8.406366277120)
    expect_equal(pmt(rate, nper, 0, fvamt, 1), -0.086407391973)
    expect_equal(pmt(rate, nper, pvamt, fvamt, 1), -8.492773669093)

    expect_equal(ipmt(rate, period, nper, pvamt, 0, 0), -4.783078679)
    expect_equal(ipmt(rate, period, nper, 0, fvamt, 0), 0.019552399667)
    expect_equal(ipmt(rate, period, nper, pvamt, fvamt, 0), -4.763526279)
    expect_equal(ipmt(rate, period, nper, pvamt, 0, 1), -4.555313027491)
    expect_equal(ipmt(rate, period, nper, 0, fvamt, 1), 0.018621333016)
    expect_equal(ipmt(rate, period, nper, pvamt, fvamt, 1), -4.536691694475)

    expect_equal(ppmt(rate, period, nper, pvamt, 0, 0), -4.043605912)
    expect_equal(ppmt(rate, period, nper, 0, fvamt, 0), -0.110280161239)
    expect_equal(ppmt(rate, period, nper, pvamt, fvamt, 0), -4.153886073)
    expect_equal(ppmt(rate, period, nper, pvamt, 0, 1), -3.851053249629)
    expect_equal(ppmt(rate, period, nper, 0, fvamt, 1), -0.105028724990)
    expect_equal(ppmt(rate, period, nper, pvamt, fvamt, 1), -3.956081974619)
})
