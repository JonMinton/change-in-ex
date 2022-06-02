# Segmented regression code 

# This is a long chunk of code which makes used of the segmented package to try to identify if there are 
# breakpoings, and whether they correspond to changepoints in
# Let's start with one of the populations, then generalise 

test_dta <- 
  hmd_ex_selected_countries_with_synth %>%
  filter(year >= 1979) %>% 
  filter(code == "GBRTENW") %>% 
  filter(sex == "female") %>% 
  filter(x == 0)

# Now we create a simple model with no breakpoint for y against t 

test_null_mdl <- lm(ex ~ year, data = test_dta) 

# Then we create a segmented version of the same model 


test_seg_mdl <- segmented::segmented(test_null_mdl)

anova(test_null_mdl, test_seg_mdl)

# now to pack into a function 

estimate_breakpoint_and_pval <- function(df){
  null_mdl <- lm(ex ~ year, data = df) 
  seg_mdl <- segmented::segmented(null_mdl)   
  anova_diff <- anova(null_mdl, seg_mdl)
  
  list(
    null = null_mdl, 
    seg = seg_mdl,
    diff = anova_diff
  )
}

segmented_breakpoint_models <- 
  hmd_ex_selected_countries_with_synth %>% 
  filter(year >= 1979) %>% 
  group_by(code, x, sex) %>% 
  nest() %>% 
  mutate(
    mdl_outputs = map(data, estimate_breakpoint_and_pval)
  ) %>% 
  unnest_wider(mdl_outputs) %>% 
  mutate(
    ftest_pval = map_dbl(diff, ~.[2,6]),
    stat_sig_preference = ftest_pval < 0.05
  ) %>% 
  mutate(
    breakpoint = map_dbl(seg, ~.[["psi"]][1,2]),
    breakpoint_se = map_dbl(seg, ~.[["psi"]][1,3])
  )

segmented_breakpoint_models



estimate_breakpoint_and_pval_v2 <- function(df){
  null_mdl <- lm(ex ~ year, data = df) 
  seg_mdl <- segmented::segmented(null_mdl, seg.Z= ~year, psi = 2010)
  seg2_mdl <- segmented::segmented(null_mdl, seg.z= ~year, psi= c(1985, 2010)) # added to test
  anova_diff <- anova(null_mdl, seg_mdl, seg2_mdl)
  
  list(
    null = null_mdl, 
    seg = seg_mdl,
    seg2 = seg2_mdl,
    diff = anova_diff
  )
}

segmented_breakpoint_models_v2 <- 
  hmd_ex_selected_countries_with_synth %>% 
  filter(code != "DEUTNP") %>%   
  filter(year >= 1979) %>% 
  group_by(code, x, sex) %>% 
  nest() %>% 
  mutate(
    mdl_outputs = map(data, estimate_breakpoint_and_pval_v2)
  ) %>% 
  unnest_wider(mdl_outputs) %>% 
  mutate(
    ftest_pval = map_dbl(diff, ~.[2,6]),
    stat_sig_preference = ftest_pval < 0.05
  ) %>% 
  mutate(
    breakpoint = map_dbl(seg, ~.[["psi"]][1,2]),
    breakpoint_se = map_dbl(seg, ~.[["psi"]][1,3])
  )

segmented_breakpoint_models_v2

# To check the results are the same more formally

identical(
  segmented_breakpoint_models %>% 
    select(
      code, x, sex, ftest_pval, stat_sig_preference, breakpoint
    ), 
  segmented_breakpoint_models_v2 %>% 
    select(
      code, x, sex, ftest_pval, stat_sig_preference, breakpoint
    )
)

# Returns FALSE 

# How are they different? 
models_comparison <- 
  segmented_breakpoint_models %>% 
  select(code, x, sex, ftest_pval, stat_sig_preference, breakpoint, breakpoint_se) %>% 
  group_by(code, x, sex) %>% 
  nest(v1 = c(ftest_pval, stat_sig_preference, breakpoint, breakpoint_se)) %>% 
  left_join(
    segmented_breakpoint_models_v2 %>% 
      select(code, x, sex, ftest_pval, stat_sig_preference, breakpoint, breakpoint_se) %>% 
      group_by(code, x, sex) %>% 
      nest(v2 = c(ftest_pval, stat_sig_preference, breakpoint, breakpoint_se))
  ) %>% 
  unnest_wider(
    v1, names_sep = "."
  ) %>% 
  unnest_wider(
    v2, names_sep = "."
  )


# How do the breakpoints and SEs differ? 
models_comparison %>% 
  ggplot() + 
  geom_point(aes(v1.breakpoint, v2.breakpoint))

# There's just one difference. What is it? 
models_comparison %>% 
  mutate(bp_diff = v2.breakpoint - v1.breakpoint) %>% 
  mutate(population = glue::glue("{code}_{x}_{sex}")) %>% 
  ggplot(aes(bp_diff, fct_reorder(population, bp_diff))) + 
  geom_point()

# So, the different starting points make a substantive difference only 
# for FRATNP males, at age 65

# I suspect they some smaller difference for other populations however. Let's 
# check 

models_comparison %>% 
  mutate(bp_diff = v2.breakpoint - v1.breakpoint) %>% 
  mutate(population = glue::glue("{code}_{x}_{sex}")) %>% 
  filter(population != "FRATNP_65_male") %>% 
  ggplot(aes(bp_diff, fct_reorder(population, bp_diff))) + 
  geom_point()

# Yes, there are differences, but they are tiny...
