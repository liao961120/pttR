context("index2df() and helpers")

# index2df(board, newest = vec(1), pages = vec(NA),
#          search_term = chr(NA), search_page = vec(1))

## Global variables --------
n_var <- 7  # Number of columns returned

## Test General input --------------

test_that("arg.board input", {
  expect_error(index2df("gossip"))
  expect_error(index2df("gossiping", newest = 0),
               "'newest' must be a positive int")
})


## Test: Argument 'newest' ------------
df <- index2df("gossiping")
df1 <- index2df("gossiping", newest = 2)
uniq_val <- min(unique(df1$idx_n))
uniq_len <- length(unique(df1$idx_n))
url <- "https://www.ptt.cc/bbs/NCCU_EDUGRAD/index.html" #cold board

test_that("index2df arg.newest", {
  expect_error(index2df(url, newest = 50000000),
               "Range exceeds possible")
  expect_true(nrow(df) <= 30)
  expect_true(nrow(df1) >= nrow(df))
  expect_true(ncol(df1) >= n_var)
  expect_true(uniq_val >= 38815)
  expect_equal(uniq_len, 2)  # n = 2
})


## Test Ordered: Argument 'pages' ------------
df2 <- index2df("gossiping", pages = c(1, 3, 5))
uniq_val <- min(unique(df2$idx_n))
uniq_len <- length(unique(df2$idx_n))

test_that("index2df arg.pages", {
  expect_equal(uniq_val, 1)
  expect_equal(uniq_len, 3)
  expect_true(nrow(df2) >= 30)
  expect_true(ncol(df2) >= n_var)
})

## Test Searching: Argument 'search_term' -------------

# cold board: NCCU_EDUGRAD
board <- "NCCU_EDUGRAD"
df <- index2df(board, search_term = "教", search_page = 1:2)
uniq_val <- unique(df$idx_n)[1]
uniq_len <- length(unique(df$idx_n))

test_that("index2df arg.search_term", {
  expect_equal(uniq_val, 1)
  expect_equal(uniq_len, 2)
  expect_true(nrow(df) >= 30)
  expect_true(ncol(df) >= n_var)
  expect_error(index2df(board, search_term = "教",
                        search_page = 1:5),
               "Page Not Found")
})
