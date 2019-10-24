library(testthat)
library(prt)

if (requireNamespace("xml2")) {
  test_check("prt", reporter = MultiReporter$new(reporters = list(JunitReporter$new(file = "test-results.xml"), CheckReporter$new())))
} else {
  test_check("prt")
}
