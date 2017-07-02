context("modifications")

x <- read_xml(paste0(
  '<umod:unimod xmlns:umod="http://www.unimod.org/xmlns/schema/unimod_2">',
  '<umod:modifications>',
  '<umod:mod title="Acetyl" full_name="Acetylation" username_of_poster="unimod" group_of_poster="admin" date_time_posted="2002-08-19 19:17:11" date_time_modified="2008-02-15 05:20:02" approved="1" record_id="1">',
  '<umod:specificity hidden="1" site="S" position="Anywhere" classification="Post-translational" spec_group="4"/>',
  '<umod:specificity hidden="1" site="C" position="Anywhere" classification="Post-translational" spec_group="3"/>',
  '<umod:specificity hidden="0" site="N-term" position="Any N-term" classification="Multiple" spec_group="2">',
  '<umod:misc_notes>GIST acetyl light</umod:misc_notes>',
  '</umod:specificity>',
  '<umod:delta mono_mass="42.010565" avge_mass="42.0367" composition="H(2) C(2) O"> <umod:element symbol="H" number="2"/> <umod:element symbol="C" number="2"/> <umod:element symbol="O" number="1"/>',
  '</umod:delta>',
  '<umod:xref>',
  '<umod:text>14730666</umod:text>',
  '<umod:source>PubMed PMID</umod:source>',
  '<umod:url/>',
  '</umod:xref>',
  '<umod:xref>',
  '<umod:text>15350136</umod:text>',
  '<umod:source>PubMed PMID</umod:source>',
  '<umod:url/>',
  '</umod:xref>',
  '</umod:mod>',
  '</umod:modifications>',
  '</umod:unimod>'))
nodes <- xml_find_all(x, "//umod:mod[@record_id=\"1\"]")

test_that(".unimodId", {
  expect_error(unimod:::.umodId("xml", 1L), "xml_document")
  expect_error(unimod:::.umodId(x, -1L), "has to be positive")
  expect_error(unimod:::.umodId(x, "foo"), "has to be numeric")
  expect_error(unimod:::.umodId(x, 1:2), "length one")
  expect_error(unimod:::.umodId(x, 100000), "not found")
})

test_that(".unimodTitle", {
  expect_error(unimod:::.umodTitle("xml", 1L), "xml_document")
  expect_error(unimod:::.umodTitle(x, 1L), "has to be a character")
  expect_error(unimod:::.umodTitle(x, c("foo", "bar")), "length one")
  expect_error(unimod:::.umodTitle(x, "foo"), "not found")
})

test_that(".title", {
  title <- c(title="Acetyl", name="Acetylation",
             lastModified="2008-02-15 05:20:02", approved="1", id="1")
  expect_equal(unimod:::.title(nodes), title)
})

test_that(".delta", {
  delta <- c(avgMass=42.0367, monoMass=42.010565)
  expect_equal(unimod:::.delta(nodes), delta)
})

test_that(".composition", {
  composition <- c(H=2, C=2, O=1)
  expect_equal(unimod:::.composition(nodes), composition)
})

test_that(".specificity", {
  sp <- data.frame(site=c("N-term", "C", "S"),
                   position=c("Any N-term", rep("Anywhere", 2)),
                   classification=c("Multiple", rep("Post-translational", 2)),
                   hidden=c(FALSE, TRUE, TRUE),
                   group=2:4,
                   stringsAsFactors=FALSE)
  expect_equal(unimod:::.specificity(nodes), sp)
})

test_that(".xref", {
  ref <- data.frame(text=c("14730666", "15350136"),
                    source=rep("PubMed PMID", 2),
                    url=character(2), stringsAsFactors=FALSE)
  expect_equal(unimod:::.xref(nodes), ref)
})
