context("utils")

test_that(".unimodDb", {
  expect_equal(unimod:::.unimodDb(),
               read_xml(system.file(file.path("extdata", "unimod.xml"),
                                    package="unimod")))
})
