ptt_dict <- readRDS("data-raw/ptt_dict.rds")
hotboard_df <- readRDS("data-raw/hotboard_df.rds")
pingying2zh <- read.csv("data-raw/translation.csv", header = F)
posts_df <- readRDS("data-raw/posts_df.rds")

devtools::use_data(ptt_dict, hotboard_df, pingying2zh,
                   posts_df,
                   internal = T, overwrite = T)
