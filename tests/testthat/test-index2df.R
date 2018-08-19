context("index2df() and helpers")

# index2df(board, n = 1, range = c(NA, NA),
#          search = c(term = NA, from = 1, to = 2))

## Global variables --------
n_var <- 7  # Number of columns returned

## Test General input --------------

test_that("arg.board input", {
  expect_error(index2df("gossip"))
  expect_error(index2df("http", search = c("a", 1, 1)),
               "[Ii]nvalid board name")
})


## Test Ordered: Argument 'n' ------------
df <- index2df("gossiping")
df1 <- index2df("gossiping", n = 2)
uniq_val <- min(unique(df1$idx_n))
uniq_len <- length(unique(df1$idx_n))
df2 <- index2df("gossiping", range = c(1, 1))
url <- "https://www.ptt.cc/bbs/NCCU_EDUGRAD/index.html" #cold board

test_that("check_index2df_order() arg.n", {
  expect_error(index2df(url, n = 500000))
  expect_error(index2df("gossiping", n = 0), "Invalid n")
  expect_true(nrow(df) <= 25)
  expect_true(nrow(df1) >= 30)
  expect_true(ncol(df1) >= n_var)
  expect_true(uniq_val >= 38815)
  expect_equal(uniq_len, 2)  # n = 2
})


## Test Ordered: Argument 'range' ------------
df <- index2df("gossiping", range = c(1554, 1555))
uniq_val <- min(unique(df$idx_n))
uniq_len <- length(unique(df$idx_n))

test_that("check_index2df_order() arg.range", {
  expect_error(index2df("gossiping", range = c(NA, 1)),
               "Arg. 'range' needs two integer")
  expect_error(index2df("gossiping", range = c(1, NA)))
  expect_equal(uniq_val, 1554)
  expect_equal(uniq_len, 2)
  expect_true(nrow(df) >= 30)
  expect_true(ncol(df) >= n_var)
})

## Test Searching: Argument 'search' -------------

# cold board: NCCU_EDUGRAD
board <- "NCCU_EDUGRAD"
df <- index2df(board, search = c("教", 1, 2))
uniq_val <- unique(df$idx_n)[1]
uniq_len <- length(unique(df$idx_n))

test_that("check_index2df_order() arg.range", {
  expect_error(index2df("gossiping", range = c(NA, 1)),
               "Arg. 'range' needs two integer")
  expect_error(index2df("gossiping", range = c(1, NA)))
  expect_equal(uniq_val, 1)
  expect_equal(uniq_len, 2)
  expect_true(nrow(df) >= 30)
  expect_true(ncol(df) >= n_var)
  expect_error(index2df(board, search = c("教", 1, 5)),
               "Page Not Found")
})
