ptt_dict <- readRDS("data-raw/ptt_dict.rds")
hotboard_df <- readRDS("data-raw/hotboard_df.rds")
pingying2zh <- read.csv("data-raw/translation.csv", header = F)
posts_df <- readRDS("data-raw/posts_df.rds")


# Common RegEx
# ※ 編輯: wronglove       來自: 122.121.216.107      (08/11 02:12)
# ※ 編輯: key000130 (122.116.231.226), 08/27/2018 20:35:50
reg_reply <- "\u203b \u7de8\u8f2f: [:alnum:]+ (.| )+\n"
reg_url <- "(http[^ ]*)|(www\\.[^ ]*)"



devtools::use_data(ptt_dict, hotboard_df, pingying2zh,
                   posts_df, reg_reply, reg_url,
                   internal = T, overwrite = T)
