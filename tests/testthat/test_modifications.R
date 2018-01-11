context("modifications")

skip_if_not_installed("xml2")
x <- xml2::read_xml(paste0(
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
node <- xml2::xml_find_first(x, "//umod:mod[@record_id=\"1\"]")

sp <- cbind(
    hidden=c("0", "1", "1"),
    site=c("N-term", "C", "S"),
    position=c("Any N-term", rep("Anywhere", 2)),
    classification=c("Multiple", rep("Post-translational", 2)),
    spec_group=as.character(2:4)
)

test_that(".title", {
    title <- c(record_id="1", title="Acetyl", full_name="Acetylation",
               date_time_modified="2008-02-15 05:20:02", approved="1")
    expect_equal(unimod:::.title(node), title)
})

test_that(".delta", {
    delta <- c(avge_mass="42.0367", mono_mass="42.010565",
               composition="H(2) C(2) O")
    expect_equal(unimod:::.delta(node), delta)
})

test_that(".specificity", {
    expect_equal(unimod:::.specificity(node), sp)

    sp2 <- cbind(
        hidden="1",
        site="S",
        position="Anywhere",
        classification="Post-translational",
        spec_group="4"
    )
    node2 <- xml2::xml_find_first(xml2::read_xml(paste0(
'<umod:unimod xmlns:umod="http://www.unimod.org/xmlns/schema/unimod_2">',
'<umod:modifications>',
'<umod:mod title="Acetyl" full_name="Acetylation" username_of_poster="unimod" group_of_poster="admin" date_time_posted="2002-08-19 19:17:11" date_time_modified="2008-02-15 05:20:02" approved="1" record_id="1">',
'<umod:specificity hidden="1" site="S" position="Anywhere" classification="Post-translational" spec_group="4"/>',
'</umod:mod>',
'</umod:modifications>',
'</umod:unimod>')), "//umod:mod")
    expect_equal(unimod:::.specificity(node2), sp2)
})
