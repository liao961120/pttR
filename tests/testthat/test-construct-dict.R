context("data-raw/scrap-ptt-wiki.R")

test_that("ptt_dict is in right format", {
  expect_equal(colnames(ptt_dict), c("term", "source"))
  expect_true(nrow(ptt_dict) >= 100)
  expect_equal(class(attr(ptt_dict, "date")), "Date")
})
