# Breakpoint script 

source(here("R", "breakpoint_functions.R"))
# Below is an example explaining the intuition of the breakpoint analysis

# Let's start with a single example population. (There are many different population subgroups, hence the 
# benefit of being able to automate this later.) We're going to start with England & Wales, females, life 
# expectancy at birth (x == 0)
example_df <- 
  hmd_ex_selected_countries_with_synth %>% 
  filter(x == 0) %>% 
  filter(sex == "female") %>% 
  filter(code == "GBRTENW") %>% 
  filter(year >= 1979)
# Now we create the change in ex variable as that's the response variable in the model
example_df <- 
  example_df %>% 
  arrange(year) %>% 
  mutate(delta_ex = ex - lag(ex)) # %>% 
# ggplot(aes(x = year, y = delta_ex)) + geom_point() + 
# geom_line() +
# geom_hline(yintercept = 0) + 
# stat_smooth()
# The null model is really simple: just an intercept 
model_null <- lm(delta_ex ~ 1, data = example_df)
summary(model_null)
# 
# The code below shows how we can be even simpler, and force the regression model not to 
# even have an intercept (i.e. have zero predictor variables)
# model_nuller <- lm(delta_ex ~ 0, data = example_df)
# summary(model_nuller)
# And we can compare the intercept with the no intercept model as follows 
# anova(model_nuller, model_null)
# a statistically significant p value shows the intercept is definitely worth including in the 
# model. Substantively that it's wrong to assume no improvement on average in ex over time since the 1980s
# There are lots of possible alternative models, some examples of which are below:
model_alt01 <- lm(delta_ex ~ year >= 1990, data = example_df)
summary(model_alt01)
model_alt02 <- lm(delta_ex ~ year >= 1995, data = example_df)
summary(model_alt02)
model_alt03 <- lm(delta_ex ~ year >= 1997, data = example_df)
summary(model_alt03)
model_alt04 <- lm(delta_ex ~ year >= 2008, data = example_df)
summary(model_alt04)
model_alt05 <- lm(delta_ex ~ year >= 2011, data = example_df)
summary(model_alt05)
# each of these models allows an adjustment to the horizontal line after a given year has been reached. 
# In the examples above some possible given years are 1990, 1995, 1997, 2008 and 2011. 
# Each of these is a candidate value for tau. But we've not looked at all possible/plausible values of this. 
# Hence, it would be good to automate further to search for each possible tau and see how the fit and model 
# results change. 
# We'll start to automate by building a function which creates T_param given tau, and runs and returns the model
# with this type of T_param
# (If tau is 1990, then T_param is equivalent to model_alt01 above; if tau were 1995, then it's equivalent to model_alt02, and so on. The only difference is that the new coefficient created has the same name, T_param, 
# whereas in the above it would have a different name each time, because the coefficient is being 
# created in situ, within the formula specification, rather than by pre-processing the dataframe)


grid_search_df <- tibble(
  tau_candidate = 1982:2015 # column of candidate values
) %>% 
  mutate(
    model_alt = map(tau_candidate, run_alt_with_given_tau, df = example_df) # run the model with the value of tau in tau_candidate
  ) %>% 
  mutate(
    aic = map_dbl(model_alt, AIC)
  )
# Now we can see which proposed tau results in what fit value (AIC), initially just by graphing it.
grid_search_df %>% 
  ggplot(aes(tau_candidate, aic)) + 
  geom_point()
# So, in the above we found 2012 as tau minimised AIC. So, that's the best possible alternative model. 
# Let's run and label it as such
best_alt_model <- example_df %>% 
  mutate( T_param = ifelse(year < 2012, FALSE, TRUE)) %>% 
  lm(delta_ex ~ T_param, data = .)
# Now we want to create the null model to compare it against (which just has an intercept term alone)
null_model <- example_df %>% 
  lm(delta_ex ~ 1, data = .)
# And finally, we want to compare the best_alt_model against the null model using an F test
anova(null_model, best_alt_model)
# We are interested in the P value here. If it's statistically significant then the best_alt_model does better
# (Is worth its additional complexity in terms of how much it makes the model fit) than the null model,
# So... for females, from birth, in England & Wales, there's evidence that there WAS a significant change in 
# average life expectancy improvement from 2012 onwards. (i.e. two years after austerity was introduced)
# We can manually repeat the same exercise for some additional subsets of the data (population, gender, starting age combinations). The below shows the same for males in England & Wales, from birth.
example_df <- 
  hmd_ex_selected_countries_with_synth %>% 
  filter(x == 0) %>% 
  filter(sex == "male") %>% 
  filter(code == "GBRTENW") %>% 
  filter(year >= 1979)
example_df <- 
  example_df %>% 
  arrange(year) %>% 
  mutate(delta_ex = ex - lag(ex))
grid_search_df <- tibble(
  tau_candidate = 1982:2015
) %>% 
  mutate(
    model_alt = map(tau_candidate, run_alt_with_given_tau, df = example_df)
  ) %>% 
  mutate(
    aic = map_dbl(model_alt, AIC)
  )
grid_search_df %>% 
  ggplot(aes(tau_candidate, aic)) + 
  geom_point()
best_alt_model <- example_df %>% 
  mutate( T_param = ifelse(year < 2013, FALSE, TRUE)) %>% 
  lm(delta_ex ~ T_param, data = .)
null_model <- example_df %>% 
  lm(delta_ex ~ 1, data = .)
anova(null_model, best_alt_model)
# This time 2013 is a slightly better candidate than 2012, hence 2013 being used in best_alt_model not 2012. 
# Another example: France, males
###
example_df <- 
  hmd_ex_selected_countries_with_synth %>% 
  filter(x == 0) %>% 
  filter(sex == "male") %>% 
  filter(code == "FRATNP") %>% 
  filter(year >= 1979)
example_df <- 
  example_df %>% 
  arrange(year) %>% 
  mutate(delta_ex = ex - lag(ex))
grid_search_df <- tibble(
  tau_candidate = 1982:2015
) %>% 
  mutate(
    model_alt = map(tau_candidate, run_alt_with_given_tau, df = example_df)
  ) %>% 
  mutate(
    aic = map_dbl(model_alt, AIC)
  )
grid_search_df %>% 
  ggplot(aes(tau_candidate, aic)) + 
  geom_point()
best_alt_model <- example_df %>% 
  mutate( T_param = ifelse(year < 2012, FALSE, TRUE)) %>% 
  lm(delta_ex ~ T_param, data = .)
summary(best_alt_model)
null_model <- example_df %>% 
  lm(delta_ex ~ 1, data = .)
summary(null_model)
anova(null_model, best_alt_model)
# Here 2015 has the lowest value, but this might be an artefact of the data period covered. 
# We can also see that 2012 is the next best candidate.
## Generalising and automating. 
# We've manually found the best break points, and compared the best alt with the standard null model 
# for each of these. What we should do now is standardise the process so it 'works' automatically 
# for each of the population subgroups. 
# As with creating synthetic germany, the grid/line search approach shown above should work, 
# but I'm going to try to use optim nonetheless for pedagogic and perfectionist(/masochistic) reasons
# optim uses some starting conditions (and sometimes other conditions) to use an algorithm 
# to find an input or group of inputs that give the best possible output from a function that takes the 
# input/inputs. 
# best can either mean the lowest or the highest achievable value. When trying to minimise errors
# (as with model specifications) that usually means trying to get the minimum value. 
# So, the input to the function will be the tau value. 
# The tau value range should have some reasonable limits. These can be specified 
# for some kinds of algorithm optim uses.
# the limits should also be determined by the data. I don't think the tau should be 
# set at the first or last observed year, as it doesn't make sense to speak of a 'trend' 
# from a single observation. This thinking generalises to at least two observations too. 
# So, that implies there should be a 'buffer' within which the optim function will look 
# for possible tau values. This should be an adjustable parameter for passing for 
# optim, but not the parameter for optim to determine. 
# There's a special type of optim algorithm for bounded optimisation like this.
# 'L-BFGS-B'. This can be selected using the method argument, and also 
# requires the lower and upper arguments be specified, for the lower 
# and upper values to search within respectively. 
# so, we need to write a function a bit like the following



# Now let's finish with the most unneccesarily complicated variant: using a different algorithm in 
# optim which will allow local minima to be escaped. This is the simulated annealing method 
# (method = "SANN") which is based on physics models. Unfortunately this is not a bounded algoritm,
# i.e. does not take values within limits, so instead the parameter passed to it has to be 
# converted and deconverted from a bounded range using a link function. 
# This can be done with a modification of the sigmoid function, which shifts the transformed 
# value from the range 0-1 to the range lower_tau-upper_tau. 
# Let's build up this function as follows: 
# sigmoid_shift <- function(x, lower = 0, upper = 1){
#   (upper - lower) / (1 + exp(-x)) + lower
# }
# # 
# curve(sigmoid_shift, -3, 3) # basic sigmoid 
# 
# sigmoid_shift2 <- function(x){
#   sigmoid_shift(x = x, lower = 1981, upper = 2015)
# }
# 
# curve(sigmoid_shift2, -3, 3)
# So this will work as a transformation from an unbounded space (x) to a 
# bounded space (f(x))
# We'll also need to reverse the operation 
# # After a bit of pen-and-paper algebra...
# sigmoid_unshift <- function(z, lower = 0, upper = 1){
#   -log((upper - lower) / (z - lower) -1)
# }
# 
# Let's double check...
#  
# sigmoid_shift(-4, lower = 1982, upper = 2015) %>% 
#   sigmoid_unshift(lower = 1982, upper = 2015)
# 
# 
# 
# get_best_tau_from_optim_sann <- function(df, buffer = 2, what = c("tau", "tau_z", "optim")){
#   # We can put in some data validation checks too 
#   sigmoid_shift <- function(x, lower = 0, upper = 1){
#     (upper - lower) / (1 + exp(-x)) + lower
#   }
#  
#   sigmoid_unshift <- function(z, lower = 0, upper = 1){
#   -log((upper - lower) / (z - lower) -1)
#   } 
#   
#   
#   what <- match.arg(what)
#   
#   years <- df$year 
#   tau_lower <- min(years) + buffer # the lower bound 
#   tau_upper <- max(years) - buffer # the upper bound
#   # tau_start <- (tau_upper + tau_lower) / 2 # start just in the middle 
#   # (This shouldn't matter if the algorithm is robust) 
#   
#   get_result_from_model <- function(par, df, what = c("AIC", "BIC", "model")){
#     # Again, can carry out more validation checks 
#     what <- match.arg(what)
#     
#     tau_z <- par[["tau_z"]] # This is how parameters are packed up by optim
#     tau   <- sigmoid_shift(tau_z, lower = tau_lower, upper = tau_upper)
#     
#     model <-
#       df %>% 
#         arrange(year) %>% 
#         mutate(delta_ex = ex - lag(ex)) %>% 
#         filter(!is.na(delta_ex)) %>% 
#         mutate(T_param = ifelse(year < tau, FALSE, TRUE)) %>% 
#         lm(delta_ex ~ T_param, data = .)
# 
#     if (what == "model") {return(model)      }
#     if (what == "BIC")   {return(BIC(model)) }
#     if (what == "AIC")   {return(AIC(model)) }
#     
#     # NULL should never be returned! If it has something's gone wrong!
#     NULL
#   }
# 
#   optim_obj <- 
#     optim(
#       par = list(tau_z = 0), 
#       fn = get_result_from_model, 
#       method = "SANN",
#       df = df
#     )
# 
#   # Need to make the result dependent on the what argument
#   
#   if (what == "tau") {return(sigmoid_shift(optim_obj[["par"]], lower = tau_lower, upper = tau_upper))}
#   if (what == "tau_z") {return(optim_obj[["par"]])}  
#   else if (what == "optim"){ return(optim_obj) }
#   
#   # Again, the following should never be triggered
#   NULL
# }
# 
# 
# sann_estimates <- 
#   hmd_ex_selected_countries_with_synth %>%
#     filter(year >= 1979) %>% 
#     group_by(code, x, sex) %>% 
#     nest() %>% 
#     mutate(
#       best_tau = map_dbl(data, get_best_tau_from_optim_sann)
#     )
# 
# # This takes ABSOLUTELY AGES, and really is the sledgehammer-to-crack
# # -a-nut approach to use only as a last resort. #
# # However if you ever do have a complex and non-finite fitness 
# # surface to try to optimise, and it also requires searching 
# # over a bounded area, then this is how you can do it! 