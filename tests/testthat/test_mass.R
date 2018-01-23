context("mass")

test_that(".aamass", {
    expect_error(.aamass(1:3))
    expect_error(.aamass("ACE", "FOO"))
    expect_equal(.aamass("ACE"), .aamass("ACE", "mono"))
    expect_equal(.aamass("ACE")[[1]],
                 c(A=71.037114, C=103.009185, E=129.042593))
    expect_equal(.aamass("ACE", "average")[[1]],
                 c(A=71.0779, C=103.1429, E=129.1140))
    expect_equal(.aamass(c(foo="ACE", bar="CEA")),
                 list(foo=c(A=71.037114, C=103.009185, E=129.042593),
                      bar=c(C=103.009185, E=129.042593, A=71.037114)))
})
