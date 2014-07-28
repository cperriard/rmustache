require(yaml)
require(testthat)

sections <- yaml.load_file("inst/tests/specs/sections.yml")

tests <- sections$tests

context("Sections")
for (test in tests) {
    test_that(test$name, 
              expect_that(render_mustache(test$template, test$data), 
                          equals(test$expected)
                          )
              )
}
