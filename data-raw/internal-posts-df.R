library(pttR)
library(dplyr)

base <- system.file("gossiping/posts/", package = "pttR")
file_list <- paste0(base, "/", list.files(base))

posts_df <- post2df(file_list)


saveRDS(posts_df, "data-raw/posts_df.rds")
