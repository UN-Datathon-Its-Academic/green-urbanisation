if(!file.exists('merged-data.csv')) {
  merged_df <- list.files('./merged-final/', pattern = 'csv') |>
    purrr::map(~readr::read_csv(paste0('merged-final/', .x), show_col_types = F) |>
                 dplyr::mutate(city = stringr::str_extract(.x, '.*(?=_merged)'), .before=1)) |>
    purrr::reduce(dplyr::bind_rows)
  merged_df |>
    write_csv('merged-data.csv')
}
merged_df <- read_csv('merged-data.csv')
mdl <- merged_df |>
  lme4::lmer(greenness ~ I(total_area/1000^2) + (1|city), data=_)
# MuMIn::r.squaredGLMM(mdl)
coef_value <- lme4::fixef(mdl)[[2]]

