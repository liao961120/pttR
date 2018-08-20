context("get_post() list column test")

## Helper Function: eval string as object
eval_str <- function(str) eval(parse(text = str))

url <- "https://www.ptt.cc/bbs/Gossiping/M.1514735254.A.8ED.html"
df0 <- get_post(url)
df1 <- get_post(url, , index = 1L)
df2 <- get_post(url, index = 1L, board_col = T)

test_that("dimension of get_post()",{
  expect_equal(ncol(df0) + 1, ncol(df1))
  expect_equal(ncol(df1) + 1, ncol(df2))
  expect_equal(nrow(df1), 1)
  expect_true(nrow(df0) == nrow(df1))
  expect_true(nrow(df2) == nrow(df1))
})


ls <- paste0(c("df0", "df1", "df2"), "$comment[[1]]")
ls <- sample(ls, 1)
cmt_df <- eval_str(ls)

test_that("comment list column structure", {
  expect_true("data.frame" %in% class(cmt_df))
  expect_true("POSIXct" %in% class(cmt_df$time))
})
