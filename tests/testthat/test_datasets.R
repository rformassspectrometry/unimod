context("datasets")

# may move to longtests/
# see: https://stat.ethz.ch/pipermail/bioc-devel/2017-November/012327.html

skip_if_not_installed("xml2")

xml <- unimod:::.unimodDb()

test_that("aminoacids", {
    data(aminoacids, package="unimod")
    expect_equal(unimod:::.aminoacids(xml), aminoacids)
})

test_that("elements", {
    data(elements, package="unimod")
    expect_equal(unimod:::.elements(xml), elements)
})

test_that("modifications", {
    data(modifications, package="unimod")
    expect_equal(unimod:::.modifications(xml), modifications)
})
