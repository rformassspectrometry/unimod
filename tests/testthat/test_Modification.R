context("Modification")

sp <- data.frame(site=c("N-term", "C", "S"),
                 position=c("Any N-term", rep("Anywhere", 2)),
                 classification=c("Multiple", rep("Post-translational", 2)),
                 hidden=c(FALSE, TRUE, TRUE),
                 group=2:4,
                 stringsAsFactors=FALSE)

ref <- data.frame(text=c("14730666", "15350136"),
                  source=rep("PubMed PMID", 2),
                  url=character(2), stringsAsFactors=FALSE)

mod <- new("Modification", id=1L, name="Acetyl", description="Acetylation",
           lastModified="2008-02-15 05:20:02", approved=TRUE,
           deltaAvgMass=42.0367, deltaMonoMass=42.010565,
           composition=c(H=2L, C=2L, O=1L), specificity=sp, refs=ref)

test_that("accessors", {
  expect_identical(id(mod), 1L)
  expect_identical(accession(mod), 1L)
  expect_identical(accessions(mod), 1L)
  expect_true(approved(mod))
  expect_equal(name(mod), "Acetyl")
  expect_equal(names(mod), "Acetyl")
  expect_equal(description(mod), "Acetylation")
  expect_equal(deltaAvgMass(mod), 42.0367)
  expect_equal(deltaMonoMass(mod), 42.010565)
  expect_equal(mass(mod), 42.010565)
  expect_equal(dim(mod), c(3, 5))
  expect_equal(nrow(mod), 3)
  expect_equal(ncol(mod), 5)
  expect_equal(length(mod), 3)
  expect_equal(composition(mod), c(H=2L, C=2L, O=1L))
  expect_equal(specificity(mod), sp)
  expect_equal(specificity(mod, all=FALSE), sp[1,,drop=FALSE])
  hcol <- which(colnames(sp) == "hidden")
  mod@specificity <- sp[,-hcol]
  expect_equal(specificity(mod), sp[,-hcol])
  expect_equal(specificity(mod, all=FALSE), sp[,-hcol])
  mod@specificity <- sp
  expect_equal(references(mod), ref)
})

test_that("validation", {
  expect_true(validObject(mod))
  expect_error(validObject(new("Modification")), "'deltaMonoMass' has to be")
  expect_error(validObject(new("Modification", deltaMonoMass=1)),
               "not be empty")
  expect_error(validObject(new("Modification",
                               deltaMonoMass=1,
                               specificity=data.frame(foo=1:2, bar=1:2))),
               "'site' or 'position' column")
})
