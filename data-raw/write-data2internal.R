ptt_dict <- readRDS("data-raw/ptt_dict.rds")
hotboard_df <- readRDS("data-raw/hotboard_df.rds")

devtools::use_data(ptt_dict, hotboard_df,
                   internal = T, overwrite = T)
