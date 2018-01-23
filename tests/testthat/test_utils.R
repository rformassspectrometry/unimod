context("utils")

test_that(".composition2character", {
    expect_error(unimod:::.composition2character("foo"), "must be a 'numeric'")
    expect_error(unimod:::.composition2character(1:3), "must be a named")
    expect_equal(unimod:::.composition2character(c(H=1, P=1, O=4)),
                 "H(1) P(1) O(4)")
})

test_that(".character2composition", {
    expect_error(unimod:::.character2composition(1:3), "must be a 'character'")
    expect_equal(unimod:::.character2composition("H(1) P(1) O(4)"),
                 c(H=1, P=1, O=4))
    expect_equal(unimod:::.character2composition("H P O(4)"), c(H=1, P=1, O=4))
})

test_that(".string2character", {
    expect_error(unimod:::.string2character(1:3))
    expect_equal(unimod:::.string2character(c(a="ABC", b="DEF")),
                 list(a=c("A", "B", "C"), b=c("D", "E", "F")))
    skip_if_not_installed("Biostrings")
    expect_equal(unimod:::.string2character(
        Biostrings::AAStringSet(c(a="ABC", b="DEF"))
    ), list(a=c("A", "B", "C"), b=c("D", "E", "F")))
})
