

# Breakpoint functions

run_alt_with_given_tau <- function(tau, df){
  df <- 
    df %>% 
    filter(!is.na(delta_ex)) %>% 
    mutate(
      T_param = ifelse(year < tau, FALSE, TRUE)
    )
  
  lm(delta_ex ~ T_param, data  = df)
}




get_best_tau_from_optim <- function(df, buffer = 2, what = c("tau", "optim")){
  # We can put in some data validation checks too 
  
  what <- match.arg(what)
  
  years <- df$year 
  tau_lower <- min(years) + buffer # the lower bound 
  tau_upper <- max(years) - buffer # the upper bound
  tau_start <- (tau_upper + tau_lower) / 2 # start just in the middle 
  # (This shouldn't matter if the algorithm is robust) 
  
  get_result_from_model <- function(par, df, what = c("AIC", "BIC", "model")){
    # Again, can carry out more validation checks 
    what <- match.arg(what)
    
    tau <- par[["tau"]] # This is how parameters are packed up by optim
    model <-
      df %>% 
      arrange(year) %>% 
      mutate(delta_ex = ex - lag(ex)) %>% 
      filter(!is.na(delta_ex)) %>% 
      mutate(T_param = ifelse(year < tau, FALSE, TRUE)) %>% 
      lm(delta_ex ~ T_param, data = .)
    if (what == "model") {return(model)      }
    if (what == "BIC")   {return(BIC(model)) }
    if (what == "AIC")   {return(AIC(model)) }
    
    # NULL should never be returned! If it has something's gone wrong!
    NULL
  }
  
  optim_obj <- 
    optim(
      par = list(tau = tau_start), 
      fn = get_result_from_model, 
      method = "L-BFGS-B",
      lower = tau_lower,
      upper = tau_upper,
      df = df
    )
  # Need to make the result dependent on the what argument
  
  if (what == "tau") {return(optim_obj[["par"]])}
  else if (what == "optim"){ return(optim_obj) }
  
  # Again, the following should never be triggered
  NULL
}



get_best_tau_from_gridsearch <- function(df, buffer = 2, what = c("best", "all")){
  # We can put in some data validation checks too 
  
  what <- match.arg(what)
  
  years <- df$year 
  tau_lower <- min(years) + buffer # the lower bound 
  tau_upper <- max(years) - buffer # the upper bound
  tau_searchrange <- tau_lower:tau_upper
  get_result_from_model <- function(tau, df, what = c("AIC", "BIC", "model")){
    # Again, can carry out more validation checks 
    what <- match.arg(what)
    
    model <-
      df %>% 
      arrange(year) %>% 
      mutate(delta_ex = ex - lag(ex)) %>% 
      filter(!is.na(delta_ex)) %>% 
      mutate(T_param = ifelse(year < tau, FALSE, TRUE)) %>% 
      lm(delta_ex ~ T_param, data = .)
    if (what == "model") {return(model)      }
    if (what == "BIC")   {return(BIC(model)) }
    if (what == "AIC")   {return(AIC(model)) }
    
    # NULL should never be returned! If it has something's gone wrong!
    NULL
  }
  
  search_df <- tibble(
    tau = tau_searchrange
  ) %>% 
    mutate(aic = map_dbl(tau, get_result_from_model, df = df))
  
  if (what == "all") {return(search_df)}
  
  if (what == "best") {
    out <- 
      search_df %>% 
      filter(aic == min(aic)) %>% 
      pull(tau)
    return(out)
  }
  # Again, the following should never be triggered
  NULL
}


