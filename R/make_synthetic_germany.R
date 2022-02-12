
pacman::p_load(HMDHFDplus, tidyverse, here)
# Get and calculate changes in e0

source(here("R", "get_data_from_hmd.R"))

rm(hmd_mx)

# Let's just keep e0 and e65, and for Germany

e0_e65 <- 
  hmd_lt  %>% 
    filter(Age %in% c(0, 65)) %>% 
    filter(str_detect(code, "DEU")) %>% 
    select(code, sex, year = Year, x = Age, ex) %>% 
    arrange(year) 


# For each combination of x and sex, I want to
# - pull the east series
# - pull the west series
# - pull the reference series 
# - find the proportion that minimises RMSE 


# For each of these want to 
# - pull the series for the right years in the east population
# - pull the series 

# We want to create a synthetic Germany for 1980-89 which is a 
# mixture of West and East German populations 

make_synthetic_population <- function(east_series, west_series, p_east){
  stopifnot("Series are of different lengths" = length(east_series) == length(west_series))
  stopifnot("Proportion out of possible bounds" = between(p_east, 0, 1))
  
  east_series * p_east + west_series * (1 - p_east)
}

compare_synthetic_to_reference <- function(synthetic, reference, what = c("RMSE", "abs", "rel")){
  stopifnot("Synthetic and reference are different lengths" = length(synthetic) == length(reference))
  
  what <- match.arg(what)
  
  if (what == "RMSE"){
    out <- (synthetic - reference)^2 %>% 
      mean() %>% 
      .^(1/2)
    return(out)
  } else if (what == "abs"){
    return(synthetic - reference)
  } else if (what == "rel"){
    out <- (synthetic - reference)/reference
    return(out)
  } else {
    stop("Wrong what argument (which should have been caught earlier")
  }
  NULL
}

compare_series_get_rmse <- function(data, p_east = 0.20,
                           east_label = "DEUTE", west_label = "DEUTW", ref_label = "DEUTNP",
                           comp_period = c(1990, 2010))
{
  
  stopifnot("data is not a dataframe"      = "data.frame" %in% class(e0_e65)         )
  stopifnot("proportion not valid"         = between(p_east, 0, 1)                   )
  stopifnot("comp_period not a range"      = length(comp_period) == 2                )
  stopifnot("comp_period values not valid" = comp_period %>% is.numeric() %>% all()  )

  # Get the series 
  extract_series <- function(data = data, code_label, comp_period){
    data %>% 
      filter(code == code_label) %>% 
      filter(between(year, comp_period[1], comp_period[2])) %>% 
      arrange(year) %>% 
      pull(ex)
  }
  
  
  message("getting East series")
  east_series <- extract_series(data, east_label, comp_period)
  message("getting West series")
  west_series <- extract_series(data, west_label, comp_period)
  message("getting reference series")
  ref_series  <- extract_series(data, ref_label,  comp_period)
  
  stopifnot("East/West series lengths differ" = length(east_series) == length(west_series))
  stopifnot("Ref series length differs from EastWest" = length(ref_series) == length(east_series))

  message("creating synthetic population and calculating RMSE")
  
  rmse <- make_synthetic_population(east_series, west_series, p_east = p_east) %>% 
    compare_synthetic_to_reference(reference = ref_series)
  
  stopifnot("rmse failed: not length 1" = length(rmse) == 1)
  stopifnot("rmse failed: not right class" = class(rmse) == "numeric")
  
  rmse
}

# Try with one part of e0_e65

pack_for_optim <- function(par, data){
  p_east <- par["p_east"]
  data %>% compare_series_get_rmse(p_east = p_east)
}

run_optim <- function(data){
  optim(
    par = c(p_east = 0.5),
    fn = pack_for_optim, data = data,
    lower = 0, upper = 1,
    method = "L-BFGS-B"
  )
}

e0_e65 %>% 
  group_by(sex, x) %>% 
  nest() %>% 
  mutate(
    optim_outputs = map(data, run_optim)
  ) %>% 
  mutate(
    optim_share = map_dbl(optim_outputs, pluck, "par")
  )

#let's start with a grid search approach, and with comparator period 1990-2010 

east_series <- 
  hmd_e0 %>% 
    filter(code == "DEUTE") %>% 
    filter(between(Year, 1990, 2010)) %>% 
    filter(sex == "total") %>% 
    arrange(Year) %>% 
    pull(e0)

west_series <- 
  hmd_e0 %>% 
  filter(code == "DEUTW") %>% 
  filter(between(Year, 1990, 2010)) %>% 
  filter(sex == "total") %>% 
  arrange(Year) %>% 
  pull(e0)

ref_series <-
  hmd_e0 %>% 
  filter(code == "DEUTNP") %>% 
  filter(between(Year, 1990, 2010)) %>% 
  filter(sex == "total") %>% 
  arrange(Year) %>% 
  pull(e0)

make_and_compare(p_share, east_series, west_series, ref_series){
  make_synthetic_population(east_series = east_series, west_series = west_series, p_share = p_east)
}

grid_pshare <- 
  tibble(
    p_share = seq(0, 1, by = 0.01)
  ) %>% 
    mutate(
      rmse = map_dbl(p_share, 
                     ~make_synthetic_population(east_series, west_series, p_east = .x) %>% 
                       compare_synthetic_to_reference(., reference = ref_series))
    )

grid_pshare %>% 
  ggplot(aes(p_share, rmse)) + 
  geom_point()

grid_pshare %>% 
  filter(rmse == min(rmse))

# So this estimates minimised at p_share of 0.2.

# What does this look like? 

hmd_e0 %>% 
  filter(code %in% c("DEUTE", "DEUTW", "DEUTNP")) %>% 
  filter(sex == "total") %>% 
  pivot_wider(names_from = code, values_from = e0, values_fill = NA)  %>% 
  arrange(Year) %>% 
  mutate(DEUT_SYNTH = 0.2 * DEUTE + (1 - 0.2) * DEUTW) %>% 
  pivot_longer(cols = c("DEUTNP", "DEUTE", "DEUTW", "DEUT_SYNTH"), names_to = "code", values_to = "e0") %>% 
  ggplot(aes(x = Year, y = e0, group = code, colour = code)) + 
  geom_line()


  

