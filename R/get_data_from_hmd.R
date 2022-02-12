# Extract required data from hmd 


# Extract e0

if (!file.exists(here("data", "e0.rds"))){
  message("Local version of e0 not found")
  
  message("Getting personal credentials")
  source(here("my_credentials", "credentials.R")) # Save my username and password somewhere not on github
  
  # # Work with HMD 
  all_country_codes <- getHMDcountries()
  # 
  # # Check each of the lists of items contains E0per
  # all_country_codes %>% 
  #   map(~getHMDitemavail(.x, my_username, my_password)) %>% 
  #   map_lgl(~("E0per" %in% .x))
  # 
  
  # As all contain this item we can just extract for each 
  message("Extracting e0 from HMD")
  hmd_e0 <- 
    tibble(
      code = all_country_codes
    ) %>% 
    mutate(data = map(code, ~readHMDweb(.x, "E0per", my_username, my_password))) %>% 
    unnest(data) %>% 
    pivot_longer(c("Female", "Male", "Total"), names_to = "sex", values_to = "e0") %>% 
    mutate(sex = tolower(sex))
  hmd_e0
  
  message("Saving e0 to e0.rds")
  
  saveRDS(hmd_e0, here("data", "e0.rds"))
  
} else {
  message("e0 file found, so reading locally")
  
  hmd_e0 <- read_rds(here("data", "e0.rds"))
}


# Extract e0

if (!file.exists(here("data", "mx.rds"))){
  message("Local version of mx not found")
  
  message("Getting personal credentials")
  source(here("my_credentials", "credentials.R")) # Save my username and password somewhere not on github
  
  # # Work with HMD 
  all_country_codes <- getHMDcountries()
  # 
  # # Check each of the lists of items contains E0per
  # all_country_codes %>% 
  #   map(~getHMDitemavail(.x, my_username, my_password)) %>% 
  #   map_lgl(~("E0per" %in% .x))
  # 
  
  # As all contain this item we can just extract for each 
  message("Extracting mx from HMD")
  hmd_mx <- 
    tibble(
      code = all_country_codes
    ) %>% 
    mutate(data = map(code, ~readHMDweb(.x, "Mx_1x1", my_username, my_password))) %>% 
    unnest(data) %>% 
    pivot_longer(c("Female", "Male", "Total"), names_to = "sex", values_to = "mx") %>% 
    mutate(sex = tolower(sex)) %>% 
    select(code, Year, Age, sex, mx, OpenInterval)
  
  message("Saving mx to mx.rds")
  
  saveRDS(hmd_mx, here("data", "mx.rds"))
  
} else {
  message("mx file found, so reading locally")
  
  hmd_mx <- read_rds(here("data", "mx.rds"))
}


# Do the same for lifetables

if (!file.exists(here("data", "lifetables.rds"))){
  message("Local version of lifetables not found")
  
  message("Getting personal credentials")
  source(here("my_credentials", "credentials.R")) # Save my username and password somewhere not on github
  
  # # Work with HMD 
  all_country_codes <- getHMDcountries()
  # 
  # # Check each of the lists of items contains E0per
  # all_country_codes %>% 
  #   map(~getHMDitemavail(.x, my_username, my_password)) %>% 
  #   map_lgl(~("E0per" %in% .x))
  # 
  
  # As all contain this item we can just extract for each 
  message("Extracting female lifetables from HMD")
  hmd_flt <- 
    tibble(
      code = all_country_codes
    ) %>% 
    mutate(data = map(code, ~readHMDweb(.x, "fltper_1x1", my_username, my_password))) %>% 
    unnest(data) %>% 
    mutate(sex = "female") %>% 
    select(code, Year, sex, everything())

  message("Extracting male lifetables from HMD")
  hmd_mlt <- 
    tibble(
      code = all_country_codes
    ) %>% 
    mutate(data = map(code, ~readHMDweb(.x, "mltper_1x1", my_username, my_password))) %>% 
    unnest(data) %>% 
    mutate(sex = "male") %>% 
    select(code, Year, sex, everything())
  
  hmd_lt <- bind_rows(hmd_flt, hmd_mlt)
  
  saveRDS(hmd_lt, here("data", "lifetables.rds"))
  
} else {
  message("lifetables file found, so reading locally")
  hmd_lt <- read_rds(here("data", "lifetables.rds"))
}

