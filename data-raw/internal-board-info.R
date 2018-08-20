devtools::load_all()

hotboard_df <- hotboards(get_new = TRUE)

# Export data to RDS for Internal Usage

saveRDS(hotboard_df, "data-raw/hotboard_df.rds")
