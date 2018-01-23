context("mass")

test_that(".aamass", {
    expect_error(unimod:::.aamass(1:3))
    expect_error(unimod:::.aamass("ACE", "FOO"))
    expect_equal(unimod:::.aamass("ACE"), unimod:::.aamass("ACE", "MonoMass"))
    expect_equal(unimod:::.aamass("ACE"), 303.088892)
    expect_equal(unimod:::.aamass("ACE", "AvgMass"), 303.3348)
    expect_equal(unimod:::.aamass(c(foo="ACE", bar="CEA")),
                 c(foo=303.088892, bar=303.088892))
})

test_that(".unimodMass", {
    expect_error(unimod:::.unimodMass(1:3))
    expect_error(unimod:::.unimodMass("ACE"))
    expect_error(unimod:::.unimodMass("ACE", "FOO"))
    expect_error(unimod:::.unimodMass("ACE", c("FOO", "BAR")))
    expect_equal(unimod:::.unimodMass("ACE", "Met-loss:P-M"), 0)
    expect_equal(unimod:::.unimodMass("MACE", "Met-loss:P-M"),
                 unimod:::.unimodMass("MACE", "Met-loss:P-M", "MonoMass"))
    expect_equal(unimod:::.unimodMass("MACE", "Met-loss:P-M", "MonoMass"),
                 -131.040485)
    expect_equal(unimod:::.unimodMass("MACE", "Met-loss:P-M", "AvgMass"),
                 -131.1961)
    expect_equal(unimod:::.unimodMass("MCCE", "Met-loss:P-M"),
                 unimod:::.unimodMass("MCCE", "Met-loss+Acetyl:P-M"))
    expect_equal(unimod:::.unimodMass("MACE", "Met-loss+Acetyl:P-M"),
                 -89.02992)
    expect_equal(unimod:::.unimodMass("ACE", "Acetyl:N-term"), 42.010565)
    expect_equal(unimod:::.unimodMass("ACE", "Acetyl:C"), 42.010565)
    expect_equal(unimod:::.unimodMass("ACE", "Acetyl:H"), 0)
    expect_equal(unimod:::.unimodMass(c("ABE", "ACE"), "Acetyl:C"),
                 c(0, 42.010565))
    expect_message(unimod:::.unimodMass("ACE", "Unknown:420:N-term"),
                   "Applying the default rule for .* create an issue")
    expect_silent(unimod:::.unimodMass("ACE", "Unknown:420:N-term", msg=FALSE))
})

test_that(".unimodSequence", {
    expect_error(unimod:::.unimodSequence(1:3))
    expect_error(unimod:::.unimodSequence("ACE"))
    expect_error(unimod:::.unimodSequence("ACE", "FOO"))
    expect_error(unimod:::.unimodSequence("ACE", c("FOO", "BAR")))
    expect_equal(unimod:::.unimodSequence("ACE", "Met-loss:P-M"), "ACE")
    expect_equal(unimod:::.unimodSequence("MACE", "Met-loss:P-M"), "ACE")
    expect_equal(unimod:::.unimodSequence("MACE", "Met-loss+Acetyl:P-M"),
                 "ACE")
    expect_equal(unimod:::.unimodSequence(c("MBCE", "MACE"), "Met-loss:P-M"),
                 c("MBCE", "ACE"))
})

test_that(".mass", {
    expect_error(unimod:::.mass(1:3))
    expect_error(unimod:::.mass("ACE", "FOO"))
    expect_error(unimod:::.mass("ACE", fixedModifications="FOO"), "is not part")
    expect_error(unimod:::.mass("ACE",
                                fixedModifications=c("FOO", "Met-loss:P-M",
                                                     "BAR")), "are not part")
    expect_error(unimod:::.mass("ACE", fixedModifications=1:3),
                 "must be a `character`")
    expect_error(unimod:::.mass("ACE",
                                fixedModifications=c("Acetyl:K",
                                                     "Carbamidomethyl:K"),
                       "Duplicated modification sites are not allowed"))
})

test_that(".countSite", {
    expect_error(unimod:::.countSite(1:3, "C"))
    expect_error(unimod:::.countSite("ACE", 1:3))
    expect_error(unimod:::.countSite("ACE", c("A", "C")))
    expect_equal(unimod:::.countSite("ACE", "Nterm"), 1)
    expect_equal(unimod:::.countSite("ACE", "Cterm"), 1)
    expect_equal(unimod:::.countSite("ACE", "C"), 1)
    expect_equal(unimod:::.countSite(c("ACCE", "ACE"), "Nterm"), c(1, 1))
    expect_equal(unimod:::.countSite(c("ACCE", "ACE"), "Cterm"), c(1, 1))
    expect_equal(unimod:::.countSite(c("ACCE", "ACE"), "C"), 2:1)
    expect_equal(unimod:::.countSite(c("ACCE", "ACE"), "H"), c(0, 0))
})
