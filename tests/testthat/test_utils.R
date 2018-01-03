context("utils")

test_that(".isModification", {
  expect_true(unimod:::.isModification(new("Modification")))
  expect_error(unimod:::.isModification(1L), "has to be an 'Modification' object")
})

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
  expect_equal(unimod:::.character2composition("H P O(4)"),
               c(H=1, P=1, O=4))
})

test_that(".unimodDb", {
  expect_equal(unimod:::.unimodDb(),
               read_xml(system.file(file.path("extdata", "unimod.xml"),
                                    package="unimod")))
})
