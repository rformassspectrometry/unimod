context("modifications")

x <- read_xml(paste0(
  '<umod:unimod xmlns:umod="http://www.unimod.org/xmlns/schema/unimod_2">',
  '<umod:modifications>',
  '<umod:mod title="Acetyl" full_name="Acetylation" username_of_poster="unimod" group_of_poster="admin" date_time_posted="2002-08-19 19:17:11" date_time_modified="2008-02-15 05:20:02" approved="1" record_id="1">',
  '<umod:specificity hidden="1" site="T" position="Anywhere" classification="Post-translational" spec_group="6"/>',
  '<umod:specificity hidden="0" site="N-term" position="Protein N-term" classification="Post-translational" spec_group="5"/>',
  '<umod:specificity hidden="1" site="S" position="Anywhere" classification="Post-translational" spec_group="4"/>',
  '<umod:specificity hidden="1" site="C" position="Anywhere" classification="Post-translational" spec_group="3"/>',
  '<umod:specificity hidden="0" site="N-term" position="Any N-term" classification="Multiple" spec_group="2">',
  '<umod:misc_notes>GIST acetyl light</umod:misc_notes>',
  '</umod:specificity>',
  '<umod:specificity hidden="0" site="K" position="Anywhere" classification="Multiple" spec_group="1">',
  '<umod:misc_notes>PT and GIST acetyl light</umod:misc_notes> </umod:specificity>',
  '<umod:specificity hidden="1" site="Y" position="Anywhere" classification="Chemical derivative" spec_group="7">',
  '<umod:misc_notes>O-acetyl</umod:misc_notes> </umod:specificity>',
  '<umod:specificity hidden="1" site="H" position="Anywhere" classification="Chemical derivative" spec_group="8"/>',
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
