# dta_for_decade_comparison <- 
#   hmd_ex_selected_countries_with_synth %>% 
#   filter(x == 0) %>% 
#   filter(sex == "female") %>% 
#   filter(code == "GBRTENW") %>% 
#   filter(year >= 1979) %>% 
#   # ggplot(aes(x = year, y = ex)) +
#   #   geom_line()
#   arrange(year) %>% 
#   mutate(delta_ex = ex - lag(ex)) %>% 
#   #   ggplot(aes(x = year, y = delta_ex)) + 
#   # geom_point() + geom_line() +
#   # geom_hline(yintercept = 0)
#   mutate(
#     decade = year - (year %% 10)
#   ) %>% 
#   filter(!is.na(delta_ex))
# model_null <- 
#   lm(delta_ex ~ 1, data = dta_for_decade_comparison)
# model_alt <- 
#   lm(delta_ex ~ factor(decade), data= dta_for_decade_comparison)
# summary(model_null)
# summary(model_alt)
# anova(model_null, model_alt)
# to get the value of interest do tmp[2,6]
# Generalising 
tidy_dataset <- function(df){
  df %>% 
    filter(year >= 1979) %>% 
    arrange(year) %>% 
    mutate(delta_ex = ex - lag(ex)) %>% 
    mutate(
      decade = year - (year %% 10)
    ) %>% 
    filter(!is.na(delta_ex))
}

make_null_model <- function(df){
  lm(delta_ex ~ 1, data = df)
}
all_null_decadal_models <-
  hmd_ex_selected_countries_with_synth %>%
  group_by(x, sex, code) %>% 
  nest() %>% 
  mutate(
    prep_data = map(data, tidy_dataset) 
  ) %>% 
  mutate(
    model_null = map(prep_data, ~lm(delta_ex ~ 1, data = .)),
    model_alt  = map(prep_data, ~lm(delta_ex ~ factor(decade), data = .))
    #    model_null = map(prep_data, make_null_model)
  ) %>% 
  mutate(
    anova_results = map2(model_null, model_alt, anova)
  ) %>% 
  mutate(
    anova_pval = map_dbl(anova_results, ~.[2,6])
  ) 

# all_null_alt_models %>% 
#   filter(anova_pval < 0.05) -> tmp
# summary(tmp$model_alt[[1]])
# summary(tmp$model_alt[[2]])
# build null model
# build alt model
# do anova
# get p value from anova