hmd_ex_selected_countries <- 
  hmd_lt %>% 
  filter(code %in% countries_of_interest) %>% 
  filter(Age %in% c(0, 65)) %>% 
  select(code, Year, Age, sex, ex) %>% 
  rename(x = Age, year = Year)
#hmd_ex_selected_countries
# Code for producing synthetic Germany

series_east_f0 <- hmd_ex_selected_countries %>% 
  filter(code == "DEUTE") %>% 
  filter(between(year, 1980, 2020)) %>% 
  filter(x == 0) %>% 
  filter(sex == "female") %>% 
  arrange(year) %>% 
  pull(ex)
series_west_f0 <- hmd_ex_selected_countries %>% 
  filter(code == "DEUTW") %>% 
  filter(between(year, 1980, 2020)) %>% 
  filter(x == 0) %>% 
  filter(sex == "female") %>% 
  arrange(year) %>% 
  pull(ex)
tmp_df_f0 <- tibble(
  code = "DEUTSYNTH",
  year = 1980:2017,
  x = 0,
  sex = "female", 
  ex = make_synthetic_population(series_east_f0, series_west_f0, p_east = 0.2)
)
###
series_east_f65 <- hmd_ex_selected_countries %>% 
  filter(code == "DEUTE") %>% 
  filter(between(year, 1980, 2020)) %>% 
  filter(x == 65) %>% 
  filter(sex == "female") %>% 
  arrange(year) %>% 
  pull(ex)
series_west_f65 <- hmd_ex_selected_countries %>% 
  filter(code == "DEUTW") %>% 
  filter(between(year, 1980, 2020)) %>% 
  filter(x == 65) %>% 
  filter(sex == "female") %>% 
  arrange(year) %>% 
  pull(ex)
tmp_df_f65 <- tibble(
  code = "DEUTSYNTH",
  year = 1980:2017,
  x = 65,
  sex = "female", 
  ex = make_synthetic_population(series_east_f65, series_west_f65, p_east = 0.2)
)
##
series_east_m0 <- hmd_ex_selected_countries %>% 
  filter(code == "DEUTE") %>% 
  filter(between(year, 1980, 2020)) %>% 
  filter(x == 0) %>% 
  filter(sex == "male") %>% 
  arrange(year) %>% 
  pull(ex)
series_west_m0 <- hmd_ex_selected_countries %>% 
  filter(code == "DEUTW") %>% 
  filter(between(year, 1980, 2020)) %>% 
  filter(x == 0) %>% 
  filter(sex == "male") %>% 
  arrange(year) %>% 
  pull(ex)
tmp_df_m0 <- tibble(
  code = "DEUTSYNTH",
  year = 1980:2017,
  x = 0,
  sex = "male", 
  ex = make_synthetic_population(series_east_m0, series_west_m0, p_east = 0.2)
)
###
series_east_m65 <- hmd_ex_selected_countries %>% 
  filter(code == "DEUTE") %>% 
  filter(between(year, 1980, 2020)) %>% 
  filter(x == 65) %>% 
  filter(sex == "male") %>% 
  arrange(year) %>% 
  pull(ex)
series_west_m65 <- hmd_ex_selected_countries %>% 
  filter(code == "DEUTW") %>% 
  filter(between(year, 1980, 2020)) %>% 
  filter(x == 65) %>% 
  filter(sex == "male") %>% 
  arrange(year) %>% 
  pull(ex)
tmp_df_m65 <- tibble(
  code = "DEUTSYNTH",
  year = 1980:2017,
  x = 65,
  sex = "male", 
  ex = make_synthetic_population(series_east_m65, series_west_m65, p_east = 0.2)
)
hmd_ex_selected_countries_with_synth <- bind_rows(
  hmd_ex_selected_countries,
  tmp_df_m0,
  tmp_df_m65,
  tmp_df_f0,
  tmp_df_f65
)

rm(list = ls(pattern = "^series"))
rm(list = ls(pattern = "^tmp_df"))
rm(hmd_ex_selected_countries)
