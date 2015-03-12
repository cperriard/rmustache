require(yaml)
require(testthat)

sections <- yaml.load_file("inst/tests/specs/sections.yml")
delimiters <- yaml.load_file("inst/tests/specs/delimiters.yml")
inverted <- yaml.load_file("inst/tests/specs/inverted.yml")


tests <- sections$tests

context("Sections")
for (i in c(1:25)
     ) {
    test <- tests[[i]]
    print(i)
    test_that(test$name, 
              expect_that(render_mustache(test$template, test$data), 
                          equals(test$expected)
                          )
              )
}




