### England and Wales

#ENW female at birth
f_e0_ENW <- 
  hmd_ex_selected_countries_with_synth %>% 
  filter(x == 0) %>% 
  filter(sex == "female") %>% 
  filter(code == "GBRTENW") %>% 
  filter(year >= 1979)

# Now we create the change in ex variable as that's the response variable in the model
f_e0_ENW <- f_e0_ENW %>% 
  arrange(year) %>% 
  mutate(delta_ex = ex - lag(ex)) %>% 
  ggplot(aes(x = year, y = delta_ex)) + geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0) + 
  stat_smooth(colour = "lightpink3") 

#ENW male at birth
m_e0_ENW <- 
  hmd_ex_selected_countries_with_synth %>% 
  filter(x == 0) %>% 
  filter(sex == "male") %>% 
  filter(code == "GBRTENW") %>% 
  filter(year >= 1979)

m_e0_ENW <- m_e0_ENW %>% 
  arrange(year) %>% 
  mutate(delta_ex = ex - lag(ex)) %>% 
  ggplot(aes(x = year, y = delta_ex)) + geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0) + 
  stat_smooth(colour = "aquamarine4") 

#ENW female at age 65
f_e65_ENW <- 
  hmd_ex_selected_countries_with_synth %>% 
  filter(x == 65) %>% 
  filter(sex == "female") %>% 
  filter(code == "GBRTENW") %>% 
  filter(year >= 1979)

# Now we create the change in ex variable 
f_e65_ENW <- f_e65_ENW %>% 
  arrange(year) %>% 
  mutate(delta_ex = ex - lag(ex)) %>% 
  ggplot(aes(x = year, y = delta_ex)) + geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0) + 
  stat_smooth(colour = "lightpink3") 

#ENW male at aged 65
m_e65_ENW <- 
  hmd_ex_selected_countries_with_synth %>% 
  filter(x == 65) %>% 
  filter(sex == "male") %>% 
  filter(code == "GBRTENW") %>% 
  filter(year >= 1979)

m_e65_ENW <- m_e65_ENW %>% 
  arrange(year) %>% 
  mutate(delta_ex = ex - lag(ex)) %>% 
  ggplot(aes(x = year, y = delta_ex)) + geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0) + 
  stat_smooth(colour = "aquamarine4") 

#add labels
p_f_e0_ENW <- f_e0_ENW + labs(y = "Annual change in e0", title = "England and Wales", subtitle = "Life expectancy changes at birth for females", caption = "Source: Human Mortality Database")

p_m_e0_ENW <- m_e0_ENW + labs(y = "Annual change in e0", title = "England and Wales", subtitle = "Life expectancy changes at birth for males", caption = "Source: Human Mortality Database")

p_f_e65_ENW <- f_e65_ENW + labs(y = "Annual change in e65", title = "England and Wales", subtitle = "Life expectancy changes at aged 65 for females", caption = "Source: Human Mortality Database")

p_m_e65_ENW <- m_e65_ENW + labs(y = "Annual change in e65", title = "England and Wales", subtitle = "Life expectancy changes at aged 65 for males", caption = "Source: Human Mortality Database") 


ex_ENW <- hmd_ex_selected_countries_with_synth %>%
  filter(sex %in% c("female", "male")) %>%
  filter(code == "GBRTENW") %>%
  group_by(sex, x) %>% arrange(year) %>%
  mutate(delta_ex = ex - lag(ex)) %>%
  ungroup %>%
  filter(year >= 1979) %>%
  ggplot(aes(x = year, y = delta_ex)) + geom_point(size=0.3) +
  geom_line() +
  geom_hline(yintercept = 0) +
  stat_smooth(mapping = aes(colour = sex)) +
  facet_grid(x ~ sex) 

p_ex_ENW <- ex_ENW + labs(x = "Year", y = "Annual change in life expectancy", title = "England and Wales", subtitle = "Life expectancy changes at birth (0) and aged 65 (65) by sex", caption = "Source: Human Mortality Database") 


#SCO female at birth
f_e0_SCO <- 
  hmd_ex_selected_countries_with_synth %>% 
  filter(x == 0) %>% 
  filter(sex == "female") %>% 
  filter(code == "GBR_SCO") %>% 
  filter(year >= 1979)

# Now we create the change in ex variable as that's the response variable in the model
f_e0_SCO <- f_e0_SCO %>% 
  arrange(year) %>% 
  mutate(delta_ex = ex - lag(ex)) %>% 
  ggplot(aes(x = year, y = delta_ex)) + geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0) + 
  stat_smooth(colour = "lightpink3") 

#SCO male at birth
m_e0_SCO <- 
  hmd_ex_selected_countries_with_synth %>% 
  filter(x == 0) %>% 
  filter(sex == "male") %>% 
  filter(code == "GBR_SCO") %>% 
  filter(year >= 1979)

m_e0_SCO <- m_e0_SCO %>% 
  arrange(year) %>% 
  mutate(delta_ex = ex - lag(ex)) %>% 
  ggplot(aes(x = year, y = delta_ex)) + geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0) + 
  stat_smooth(colour = "aquamarine4") 

#SCO female at age 65
f_e65_SCO <- 
  hmd_ex_selected_countries_with_synth %>% 
  filter(x == 65) %>% 
  filter(sex == "female") %>% 
  filter(code == "GBR_SCO") %>% 
  filter(year >= 1979)

# Now we create the change in ex variable as that's the response variable in the model
f_e65_SCO <- f_e65_SCO %>% 
  arrange(year) %>% 
  mutate(delta_ex = ex - lag(ex)) %>% 
  ggplot(aes(x = year, y = delta_ex)) + geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0) + 
  stat_smooth(colour = "lightpink3") 

#SCO male at aged 65
m_e65_SCO <- 
  hmd_ex_selected_countries_with_synth %>% 
  filter(x == 65) %>% 
  filter(sex == "male") %>% 
  filter(code == "GBR_SCO") %>% 
  filter(year >= 1979)

m_e65_SCO <-  m_e65_SCO %>% 
  arrange(year) %>% 
  mutate(delta_ex = ex - lag(ex)) %>% 
  ggplot(aes(x = year, y = delta_ex)) + geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0) + 
  stat_smooth(colour = "aquamarine4", show.legend = TRUE) 

#add labels
p_f_e0_SCO <-  f_e0_SCO + labs(y = "Annual change in e0", title = "Scotland", subtitle = "Life expectancy changes at birth for females", caption = "Source: Human Mortality Database")
p_m_e0_SCO <-  m_e0_SCO + labs(y = "Annual change in e0", title = "Scotland", subtitle = "Life expectancy changes at birth for males", caption = "Source: Human Mortality Database")
p_f_e65_SCO <- f_e65_SCO + labs(y = "Annual change in e65", title = "Scotland", subtitle = "Life expectancy changes at aged 65 for females", caption = "Source: Human Mortality Database")
p_m_e65_SCO <- m_e65_SCO + labs(y = "Annual change in e65", title = "Scotland", subtitle = "Life expectancy changes at aged 65 for males", caption = "Source: Human Mortality Database")

ex_SCO <- hmd_ex_selected_countries_with_synth %>%
  filter(sex %in% c("female", "male")) %>%
  filter(code == "GBR_SCO") %>%
  group_by(sex, x) %>% arrange(year) %>%
  mutate(delta_ex = ex - lag(ex)) %>%
  ungroup %>%
  filter(year >= 1979) %>%
  ggplot(aes(x = year, y = delta_ex)) + geom_point(size=0.3) +
  geom_line() +
  geom_hline(yintercept = 0) +
  stat_smooth(mapping = aes(colour = sex)) +
  facet_grid(x ~ sex) 

p_ex_SCO <- ex_SCO + labs(x = "Year", y = "Annual change in life expectancy", title = "Scotland", subtitle = "Life expectancy changes at birth (0) and aged 65 (65) by sex", caption = "Source: Human Mortality Database")

#FRA female at birth
f_e0_FRA <- 
  hmd_ex_selected_countries_with_synth %>% 
  filter(x == 0) %>% 
  filter(sex == "female") %>% 
  filter(code == "FRATNP") %>% 
  filter(year >= 1979)

# Now we create the change in ex variable as that's the response variable in the model
f_e0_FRA <- f_e0_FRA %>% 
  arrange(year) %>% 
  mutate(delta_ex = ex - lag(ex)) %>% 
  ggplot(aes(x = year, y = delta_ex)) + geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0) + 
  stat_smooth(colour = "lightpink3") 

#FRA male at birth
m_e0_FRA <- 
  hmd_ex_selected_countries_with_synth %>% 
  filter(x == 0) %>% 
  filter(sex == "male") %>% 
  filter(code == "FRATNP") %>% 
  filter(year >= 1979)

m_e0_FRA <- m_e0_FRA %>% 
  arrange(year) %>% 
  mutate(delta_ex = ex - lag(ex)) %>% 
  ggplot(aes(x = year, y = delta_ex)) + geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0) + 
  stat_smooth(colour = "aquamarine4") 

#FRA female at age 65
f_e65_FRA <- 
  hmd_ex_selected_countries_with_synth %>% 
  filter(x == 65) %>% 
  filter(sex == "female") %>% 
  filter(code == "FRATNP") %>% 
  filter(year >= 1979)

# Now we create the change in ex variable as that's the response variable in the model
f_e65_FRA <- f_e65_FRA %>% 
  arrange(year) %>% 
  mutate(delta_ex = ex - lag(ex)) %>% 
  ggplot(aes(x = year, y = delta_ex)) + geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0) + 
  stat_smooth(colour = "lightpink3") 

#FRA male at aged 65
m_e65_FRA <- 
  hmd_ex_selected_countries_with_synth %>% 
  filter(x == 65) %>% 
  filter(sex == "male") %>% 
  filter(code == "FRATNP") %>% 
  filter(year >= 1979)

m_e65_FRA <-  m_e65_FRA %>% 
  arrange(year) %>% 
  mutate(delta_ex = ex - lag(ex)) %>% 
  ggplot(aes(x = year, y = delta_ex)) + geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0) + 
  stat_smooth(colour = "aquamarine4", show.legend = TRUE) 

#add labels

p_f_e0_FRA <- f_e0_FRA + labs(y = "Annual change in e0", title = "France", subtitle = "Life expectancy changes at birth for females", caption = "Source: Human Mortality Database")
p_m_e0_FRA <- m_e0_FRA + labs(y = "Annual change in e0", title = "France", subtitle = "Life expectancy changes at birth for males", caption = "Source: Human Mortality Database")
p_f_e65_FRA <- f_e65_FRA + labs(y = "Annual change in e65", title = "France", subtitle = "Life expectancy changes at aged 65 for females", caption = "Source: Human Mortality Database")
p_m_e65_FRA <- m_e65_FRA + labs(y = "Annual change in e65", title = "France", subtitle = "Life expectancy changes at aged 65 for males", caption = "Source: Human Mortality Database")

ex_FRA <- hmd_ex_selected_countries_with_synth %>%
  filter(sex %in% c("female", "male")) %>%
  filter(code == "FRATNP") %>%
  group_by(sex, x) %>% arrange(year) %>%
  mutate(delta_ex = ex - lag(ex)) %>%
  ungroup %>%
  filter(year >= 1979) %>%
  ggplot(aes(x = year, y = delta_ex)) + geom_point(size=0.3) +
  geom_line() +
  geom_hline(yintercept = 0) +
  stat_smooth(mapping = aes(colour = sex)) +
  facet_grid(x ~ sex) 

p_ex_FRA <- ex_FRA + labs(x = "Year", y = "Annual change in life expectancy", title = "France", subtitle = "Life expectancy changes at birth (0) and aged 65 (65) by sex", caption = "Source: Human Mortality Database")

#ESP female at birth
f_e0_ESP <- 
  hmd_ex_selected_countries_with_synth %>% 
  filter(x == 0) %>% 
  filter(sex == "female") %>% 
  filter(code == "ESP") %>% 
  filter(year >= 1979)

# Now we create the change in ex variable as that's the response variable in the model
f_e0_ESP <- f_e0_ESP %>% 
  arrange(year) %>% 
  mutate(delta_ex = ex - lag(ex)) %>% 
  ggplot(aes(x = year, y = delta_ex)) + geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0) + 
  stat_smooth(colour = "lightpink3") 

#ESP male at birth
m_e0_ESP <- 
  hmd_ex_selected_countries_with_synth %>% 
  filter(x == 0) %>% 
  filter(sex == "male") %>% 
  filter(code == "ESP") %>% 
  filter(year >= 1979)

m_e0_ESP <- m_e0_ESP %>% 
  arrange(year) %>% 
  mutate(delta_ex = ex - lag(ex)) %>% 
  ggplot(aes(x = year, y = delta_ex)) + geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0) + 
  stat_smooth(colour = "aquamarine4") 

#ESP female at age 65
f_e65_ESP <- 
  hmd_ex_selected_countries_with_synth %>% 
  filter(x == 65) %>% 
  filter(sex == "female") %>% 
  filter(code == "ESP") %>% 
  filter(year >= 1979)

# Now we create the change in ex variable as that's the response variable in the model
f_e65_ESP <- f_e65_ESP %>% 
  arrange(year) %>% 
  mutate(delta_ex = ex - lag(ex)) %>% 
  ggplot(aes(x = year, y = delta_ex)) + geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0) + 
  stat_smooth(colour = "lightpink3") 

#ESP male at aged 65
m_e65_ESP <- 
  hmd_ex_selected_countries_with_synth %>% 
  filter(x == 65) %>% 
  filter(sex == "male") %>% 
  filter(code == "ESP") %>% 
  filter(year >= 1979)

m_e65_ESP <-  m_e65_ESP %>% 
  arrange(year) %>% 
  mutate(delta_ex = ex - lag(ex)) %>% 
  ggplot(aes(x = year, y = delta_ex)) + geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0) + 
  stat_smooth(colour = "aquamarine4", show.legend = TRUE) 

#add labels
p_f_e0_ESP <- f_e0_ESP + labs(y = "Annual change in e0", title = "Spain", subtitle = "Life expectancy changes at birth for females", caption = "Source: Human Mortality Database")
p_m_e0_ESP <- m_e0_ESP + labs(y = "Annual change in e0", title = "Spain", subtitle = "Life expectancy changes at birth for males", caption = "Source: Human Mortality Database")
p_f_e65_ESP <- f_e65_ESP + labs(y = "Annual change in e65", title = "Spain", subtitle = "Life expectancy changes at aged 65 for females", caption = "Source: Human Mortality Database")
p_m_e65_ESP <- m_e65_ESP + labs(y = "Annual change in e65", title = "Spain", subtitle = "Life expectancy changes at aged 65 for males", caption = "Source: Human Mortality Database")

ex_ESP <- hmd_ex_selected_countries_with_synth %>%
  filter(sex %in% c("female", "male")) %>%
  filter(code == "ESP") %>%
  group_by(sex, x) %>% arrange(year) %>%
  mutate(delta_ex = ex - lag(ex)) %>%
  ungroup %>%
  filter(year >= 1979) %>%
  ggplot(aes(x = year, y = delta_ex)) + geom_point(size=0.3) +
  geom_line() +
  geom_hline(yintercept = 0) +
  stat_smooth(mapping = aes(colour = sex)) +
  facet_grid(x ~ sex) 

p_ex_ESP <- ex_ESP + labs(x = "Year", y = "Annual change in life expectancy", title = "Spain", subtitle = "Life expectancy changes at birth (0) and aged 65 (65) by sex", caption = "Source: Human Mortality Database")


#ITA female at birth
f_e0_ITA <- 
  hmd_ex_selected_countries_with_synth %>% 
  filter(x == 0) %>% 
  filter(sex == "female") %>% 
  filter(code == "ITA") %>% 
  filter(year >= 1979)

# Now we create the change in ex variable as that's the response variable in the model
f_e0_ITA <- f_e0_ITA %>% 
  arrange(year) %>% 
  mutate(delta_ex = ex - lag(ex)) %>% 
  ggplot(aes(x = year, y = delta_ex)) + geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0) + 
  stat_smooth(colour = "lightpink3") 

#ITA male at birth
m_e0_ITA <- 
  hmd_ex_selected_countries_with_synth %>% 
  filter(x == 0) %>% 
  filter(sex == "male") %>% 
  filter(code == "ITA") %>% 
  filter(year >= 1979)

m_e0_ITA <- m_e0_ITA %>% 
  arrange(year) %>% 
  mutate(delta_ex = ex - lag(ex)) %>% 
  ggplot(aes(x = year, y = delta_ex)) + geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0) + 
  stat_smooth(colour = "aquamarine4") 

#ITA female at age 65
f_e65_ITA <- 
  hmd_ex_selected_countries_with_synth %>% 
  filter(x == 65) %>% 
  filter(sex == "female") %>% 
  filter(code == "ITA") %>% 
  filter(year >= 1979)

# Now we create the change in ex variable as that's the response variable in the model
f_e65_ITA <- f_e65_ITA %>% 
  arrange(year) %>% 
  mutate(delta_ex = ex - lag(ex)) %>% 
  ggplot(aes(x = year, y = delta_ex)) + geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0) + 
  stat_smooth(colour = "lightpink3") 

#ITA male at aged 65
m_e65_ITA <- 
  hmd_ex_selected_countries_with_synth %>% 
  filter(x == 65) %>% 
  filter(sex == "male") %>% 
  filter(code == "ITA") %>% 
  filter(year >= 1979)

m_e65_ITA <-  m_e65_ITA %>% 
  arrange(year) %>% 
  mutate(delta_ex = ex - lag(ex)) %>% 
  ggplot(aes(x = year, y = delta_ex)) + geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0) + 
  #Trying to add legend
  #stat_smooth(aes(colour = "aquamarine4"), show.legend = TRUE, labels = "Smoothed")
  stat_smooth(colour = "aquamarine4")

#add labels
p_f_e0_ITA <- f_e0_ITA + labs(y = "Annual change in e0", title = "Italy", subtitle = "Life expectancy changes at birth for females", caption = "Source: Human Mortality Database")
p_m_e0_ITA <- m_e0_ITA + labs(y = "Annual change in e0", title = "Italy", subtitle = "Life expectancy changes at birth for males", caption = "Source: Human Mortality Database")
p_f_e65_ITA <- f_e65_ITA + labs(y = "Annual change in e65", title = "Italy", subtitle = "Life expectancy changes at aged 65 for females", caption = "Source: Human Mortality Database")
p_m_e65_ITA <- m_e65_ITA + labs(y = "Annual change in e65", title = "Italy", subtitle = "Life expectancy changes at aged 65 for males", caption = "Source: Human Mortality Database")

ex_ITA <- hmd_ex_selected_countries_with_synth %>%
  filter(sex %in% c("female", "male")) %>%
  filter(code == "ITA") %>%
  group_by(sex, x) %>% arrange(year) %>%
  mutate(delta_ex = ex - lag(ex)) %>%
  ungroup %>%
  filter(year >= 1979) %>%
  ggplot(aes(x = year, y = delta_ex)) + geom_point(size=0.3) +
  geom_line() +
  geom_hline(yintercept = 0) +
  stat_smooth(mapping = aes(colour = sex)) +
  facet_grid(x ~ sex) 

p_ex_ITA <- ex_ITA + labs(x = "Year", y = "Annual change in life expectancy", title = "Italy", subtitle = "Life expectancy changes at birth (0) and aged 65 (65) by sex", caption = "Source: Human Mortality Database")


#DEUTS female at birth
f_e0_DEUTS <- 
  hmd_ex_selected_countries_with_synth %>% 
  filter(x == 0) %>% 
  filter(sex == "female") %>% 
  filter(code == "DEUTSYNTH") %>% 
  filter(year >= 1979)

# Now we create the change in ex variable as that's the response variable in the model
f_e0_DEUTS <- f_e0_DEUTS %>% 
  arrange(year) %>% 
  mutate(delta_ex = ex - lag(ex)) %>% 
  ggplot(aes(x = year, y = delta_ex)) + geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0) + 
  stat_smooth(colour = "lightpink3") 

#DEUTS male at birth
m_e0_DEUTS <- 
  hmd_ex_selected_countries_with_synth %>% 
  filter(x == 0) %>% 
  filter(sex == "male") %>% 
  filter(code == "DEUTSYNTH") %>% 
  filter(year >= 1979)

m_e0_DEUTS <- m_e0_DEUTS %>% 
  arrange(year) %>% 
  mutate(delta_ex = ex - lag(ex)) %>% 
  ggplot(aes(x = year, y = delta_ex)) + geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0) + 
  stat_smooth(colour = "aquamarine4") 

#DEUTS female at age 65
f_e65_DEUTS <- 
  hmd_ex_selected_countries_with_synth %>% 
  filter(x == 65) %>% 
  filter(sex == "female") %>% 
  filter(code == "DEUTSYNTH") %>% 
  filter(year >= 1979)

# Now we create the change in ex variable as that's the response variable in the model
f_e65_DEUTS <- f_e65_DEUTS %>% 
  arrange(year) %>% 
  mutate(delta_ex = ex - lag(ex)) %>% 
  ggplot(aes(x = year, y = delta_ex)) + geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0) + 
  stat_smooth(colour = "lightpink3") 

#DEUTS male at aged 65
m_e65_DEUTS <- 
  hmd_ex_selected_countries_with_synth %>% 
  filter(x == 65) %>% 
  filter(sex == "male") %>% 
  filter(code == "DEUTSYNTH") %>% 
  filter(year >= 1979)

m_e65_DEUTS <-  m_e65_DEUTS %>% 
  arrange(year) %>% 
  mutate(delta_ex = ex - lag(ex)) %>% 
  ggplot(aes(x = year, y = delta_ex)) + geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0) + 
  stat_smooth(colour = "aquamarine4", show.legend = TRUE) 

#add labels
p_f_e0_DEUTS <- f_e0_DEUTS + labs(y = "Annual change in e0", title = "Germany: Synthetic", subtitle = "Life expectancy changes at birth for females", caption = "Source: Human Mortality Database")
p_m_e0_DEUTS <- m_e0_DEUTS + labs(y = "Annual change in e0", title = "Germany: Synthetic", subtitle = "Life expectancy changes at birth for males", caption = "Source: Human Mortality Database")
p_f_e65_DEUTS <- f_e65_DEUTS + labs(y = "Annual change in e65", title = "Germany: Synthetic", subtitle = "Life expectancy changes at aged 65 for females", caption = "Source: Human Mortality Database")
p_m_e65_DEUTS <- m_e65_DEUTS + labs(y = "Annual change in e65", title = "Germany: Synthetic", subtitle = "Life expectancy changes at aged 65 for males", caption = "Source: Human Mortality Database")

#Grid
ex_DEUTSYNTH <- hmd_ex_selected_countries_with_synth %>%
  filter(sex %in% c("female", "male")) %>%
  filter(code == "DEUTSYNTH") %>%
  group_by(sex, x) %>% arrange(year) %>%
  mutate(delta_ex = ex - lag(ex)) %>%
  ungroup %>%
  filter(year >= 1979) %>%
  ggplot(aes(x = year, y = delta_ex)) + geom_point(size=0.3) +
  geom_line() +
  geom_hline(yintercept = 0) +
  stat_smooth(mapping = aes(colour = sex)) +
  facet_grid(x ~ sex) 

p_ex_DEUTSYNTH <- ex_DEUTSYNTH + labs(x = "Year", y = "Annual change in life expectancy", title = "Germany: Synthetic", subtitle = "Life expectancy changes at birth (0) and aged 65 (65) by sex", caption = "Source: Human Mortality Database")

### Germany: East
#DEUTE female at birth
f_e0_DEUTE <- 
  hmd_ex_selected_countries_with_synth %>% 
  filter(x == 0) %>% 
  filter(sex == "female") %>% 
  filter(code == "DEUTE") %>% 
  filter(year >= 1979)

# Now we create the change in ex variable as that's the response variable in the model
f_e0_DEUTE <- f_e0_DEUTE %>% 
  arrange(year) %>% 
  mutate(delta_ex = ex - lag(ex)) %>% 
  ggplot(aes(x = year, y = delta_ex)) + geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0) + 
  stat_smooth(colour = "lightpink3") 

#DEUTE male at birth
m_e0_DEUTE <- 
  hmd_ex_selected_countries_with_synth %>% 
  filter(x == 0) %>% 
  filter(sex == "male") %>% 
  filter(code == "DEUTE") %>% 
  filter(year >= 1979)

m_e0_DEUTE <- m_e0_DEUTE %>% 
  arrange(year) %>% 
  mutate(delta_ex = ex - lag(ex)) %>% 
  ggplot(aes(x = year, y = delta_ex)) + geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0) + 
  stat_smooth(colour = "aquamarine4") 

#DEUTE female at age 65
f_e65_DEUTE <- 
  hmd_ex_selected_countries_with_synth %>% 
  filter(x == 65) %>% 
  filter(sex == "female") %>% 
  filter(code == "DEUTE") %>% 
  filter(year >= 1979)

# Now we create the change in ex variable as that's the response variable in the model
f_e65_DEUTE <- f_e65_DEUTE %>% 
  arrange(year) %>% 
  mutate(delta_ex = ex - lag(ex)) %>% 
  ggplot(aes(x = year, y = delta_ex)) + geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0) + 
  stat_smooth(colour = "lightpink3") 

#DEUTE male at aged 65
m_e65_DEUTE <- 
  hmd_ex_selected_countries_with_synth %>% 
  filter(x == 65) %>% 
  filter(sex == "male") %>% 
  filter(code == "DEUTE") %>% 
  filter(year >= 1979)

m_e65_DEUTE <-  m_e65_DEUTE %>% 
  arrange(year) %>% 
  mutate(delta_ex = ex - lag(ex)) %>% 
  ggplot(aes(x = year, y = delta_ex)) + geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0) + 
  stat_smooth(colour = "aquamarine4", show.legend = TRUE) 

#add labels
p_f_e0_DEUTE <- f_e0_DEUTE + labs(y = "Annual change in e0", title = "Germany: East", subtitle = "Life expectancy changes at birth for females", caption = "Source: Human Mortality Database")
p_m_e0_DEUTE <- m_e0_DEUTE + labs(y = "Annual change in e0", title = "Germany: East", subtitle = "Life expectancy changes at birth for males", caption = "Source: Human Mortality Database")
p_f_e65_DEUTE <- f_e65_DEUTE + labs(y = "Annual change in e65", title = "Germany: East", subtitle = "Life expectancy changes at aged 65 for females", caption = "Source: Human Mortality Database")
p_m_e65_DEUTE <- m_e65_DEUTE + labs(y = "Annual change in e65", title = "Germany: East", subtitle = "Life expectancy changes at aged 65 for males", caption = "Source: Human Mortality Database")

ex_DEUTE <- hmd_ex_selected_countries_with_synth %>%
  filter(sex %in% c("female", "male")) %>%
  filter(code == "DEUTE") %>%
  group_by(sex, x) %>% arrange(year) %>%
  mutate(delta_ex = ex - lag(ex)) %>%
  ungroup %>%
  filter(year >= 1979) %>%
  ggplot(aes(x = year, y = delta_ex)) + geom_point(size=0.3) +
  geom_line() +
  geom_hline(yintercept = 0) +
  stat_smooth(mapping = aes(colour = sex)) +
  facet_grid(x ~ sex) 

p_ex_DEUTE <- ex_DEUTE + labs(x = "Year", y = "Annual change in life expectancy", title = "Germany: East", subtitle = "Life expectancy changes at birth (0) and aged 65 (65) by sex", caption = "Source: Human Mortality Database")


#DEUTW female at birth
f_e0_DEUTW <- 
  hmd_ex_selected_countries_with_synth %>% 
  filter(x == 0) %>% 
  filter(sex == "female") %>% 
  filter(code == "DEUTW") %>% 
  filter(year >= 1979)

# Now we create the change in ex variable as that's the response variable in the model
f_e0_DEUTW <- f_e0_DEUTW %>% 
  arrange(year) %>% 
  mutate(delta_ex = ex - lag(ex)) %>% 
  ggplot(aes(x = year, y = delta_ex)) + geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0) + 
  stat_smooth(colour = "lightpink3") 

#DEUTW male at birth
m_e0_DEUTW <- 
  hmd_ex_selected_countries_with_synth %>% 
  filter(x == 0) %>% 
  filter(sex == "male") %>% 
  filter(code == "DEUTW") %>% 
  filter(year >= 1979)

m_e0_DEUTW <- m_e0_DEUTW %>% 
  arrange(year) %>% 
  mutate(delta_ex = ex - lag(ex)) %>% 
  ggplot(aes(x = year, y = delta_ex)) + geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0) + 
  stat_smooth(colour = "aquamarine4") 

#DEUTW female at age 65
f_e65_DEUTW <- 
  hmd_ex_selected_countries_with_synth %>% 
  filter(x == 65) %>% 
  filter(sex == "female") %>% 
  filter(code == "DEUTW") %>% 
  filter(year >= 1979)

# Now we create the change in ex variable as that's the response variable in the model
f_e65_DEUTW <- f_e65_DEUTW %>% 
  arrange(year) %>% 
  mutate(delta_ex = ex - lag(ex)) %>% 
  ggplot(aes(x = year, y = delta_ex)) + geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0) + 
  stat_smooth(colour = "lightpink3") 

#DEUTW male at aged 65
m_e65_DEUTW <- 
  hmd_ex_selected_countries_with_synth %>% 
  filter(x == 65) %>% 
  filter(sex == "male") %>% 
  filter(code == "DEUTW") %>% 
  filter(year >= 1979)

m_e65_DEUTW <-  m_e65_DEUTW %>% 
  arrange(year) %>% 
  mutate(delta_ex = ex - lag(ex)) %>% 
  ggplot(aes(x = year, y = delta_ex)) + geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0) + 
  stat_smooth(colour = "aquamarine4", show.legend = TRUE) 

#add labels
p_f_e0_DEUTW <- f_e0_DEUTW + labs(y = "Annual change in e0", title = "Germany: West", subtitle = "Life expectancy changes at birth for females", caption = "Source: Human Mortality Database")
p_m_e0_DEUTW <- m_e0_DEUTW + labs(y = "Annual change in e0", title = "Germany: West", subtitle = "Life expectancy changes at birth for males", caption = "Source: Human Mortality Database")
p_f_e65_DEUTW <- f_e65_DEUTW + labs(y = "Annual change in e65", title = "Germany: West", subtitle = "Life expectancy changes at aged 65 for females", caption = "Source: Human Mortality Database")
p_m_e65_DEUTW <- m_e65_DEUTW + labs(y = "Annual change in e65", title = "Germany: West", subtitle = "Life expectancy changes at aged 65 for males", caption = "Source: Human Mortality Database")

ex_DEUTW <- hmd_ex_selected_countries_with_synth %>%
  filter(sex %in% c("female", "male")) %>%
  filter(code == "DEUTW") %>%
  group_by(sex, x) %>% arrange(year) %>%
  mutate(delta_ex = ex - lag(ex)) %>%
  ungroup %>%
  filter(year >= 1979) %>%
  ggplot(aes(x = year, y = delta_ex)) + geom_point(size=0.3) +
  geom_line() +
  geom_hline(yintercept = 0) +
  stat_smooth(mapping = aes(colour = sex)) +
  facet_grid(x ~ sex) 

p_ex_DEUTW <- ex_DEUTW + labs(x = "Year", y = "Annual change in life expectancy", title = "Germany: West", subtitle = "Life expectancy changes at birth (0) and aged 65 (65) by sex", caption = "Source: Human Mortality Database")


all_country_plots <- list(
  p_f_e0_ENW,
  p_m_e0_ENW,
  p_f_e65_ENW,
  p_m_e65_ENW,
  p_f_e0_SCO,
  p_m_e0_SCO,
  p_f_e65_SCO,
  p_m_e65_SCO,
  p_f_e0_ESP,
  p_m_e0_ESP,
  p_f_e65_ESP,
  p_m_e65_ESP,
  p_f_e0_ITA,
  p_m_e0_ITA,
  p_f_e65_ITA,
  p_m_e65_ITA,
  p_f_e0_DEUTE,
  p_m_e0_DEUTE,
  p_f_e65_DEUTE,
  p_m_e65_DEUTE,
  p_f_e0_DEUTW,
  p_m_e0_DEUTW,
  p_f_e65_DEUTW,
  p_m_e65_DEUTW,
  p_f_e0_DEUTS,
  p_m_e0_DEUTS,
  p_f_e65_DEUTS,
  p_m_e65_DEUTS,
  p_ex_DEUTE,
  p_ex_DEUTSYNTH,
  p_ex_ITA,
  p_ex_DEUTW,
  p_ex_ESP,
  p_ex_FRA,
  p_ex_SCO,
  p_ex_ENW
  
)

rm(list = ls(pattern = "^p_"))
rm(list = ls(pattern = "^f_"))
rm(list = ls(pattern = "^m_"))
rm(list = ls(pattern = "^ex_"))

