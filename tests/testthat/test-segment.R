context("R/seg.R")

# Sample from web
board <- pttR::hotboards()$board
board <- sample(c(board, 'ntucourse'), 1)
print(board)
idx_df <- index2df(board, newest = 3)
post_df <- post2df(sample(as_url(idx_df$link), 5))
post_df1 <- post_df[, c('content', 'comment')]

post_df1$content <- seg_content(post_df$content)
post_df1$comment <- seg_comment(post_df$comment)

rand <- sample(seq_along(post_df1$content), 1)

test_that("Segmentation returns right format", {
  expect_equal(nrow(post_df1), nrow(post_df))
  expect_true(post_df1$content[rand] != post_df$content[rand])
  expect_true(class(post_df1$comment) == 'list')
})
