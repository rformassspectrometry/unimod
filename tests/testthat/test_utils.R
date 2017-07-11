context("utils")

test_that(".isModification", {
  expect_true(unimod:::.isModification(new("Modification")))
  expect_error(unimod:::.isModification(1L), "has to be an 'Modification' object")
})

test_that(".unimodDb", {
  expect_equal(unimod:::.unimodDb(),
               read_xml(system.file(file.path("extdata", "unimod.xml"),
                                    package="unimod")))
})
