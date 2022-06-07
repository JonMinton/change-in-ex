library(tidyverse)
library(here)
library(shiny)

# Define project directories 
#projdir <- here("apps", "compare_cps_and_bps")

# Define and label countries 
country_selection <- c(
  "England & Wales" = "GBRTENW", 
  "Scotland"        = "GBR_SCO",
  "Italy"           = "ITA",
  "Spain"           = "ESP",
  "Germany"         = "DEUTSYNTH",
  "France"          = "FRATNP",
  "Germany (East)"  = "DEUTE",
  "Germany (West)"  = "DEUTW"
)

# Read data 
# cp_paths <- read_rds(here(projdir, "data", "cp_paths.rds"))
# seg_models <- read_rds(here(projdir, "data", "segmented_models.rds"))
# country_data <- read_rds(here(projdir, "data", "countries_data.rds"))

cp_paths <- read_rds("data/cp_paths.rds")
seg_models <- read_rds("data/segmented_models.rds")
country_data <- read_rds("data/countries_data.rds")


# Functions (move to source when grown enough)

format_legends <- function(){
  list(
    scale_alpha_manual("BIC", values = c("1" = 1.0, "2" = 0.5, "3" = 0.25)),
      scale_linetype_manual(values = c("1" = "solid", "2" = "dashed", "3" = "dashed")), 
      scale_size_manual(values = c(1, 1.4, 1.8)), 
      guides(alpha = "none", linetype = "none")       
  )
}

# Define UI
ui <- fluidPage(

    # Application title
    titlePanel("Compare changepoint models"),

    sidebarLayout(
        sidebarPanel(
          selectInput("code_select", "Select a country", choices = country_selection),
          checkboxInput("add_obs", "Click to add data points", value = TRUE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(id = "maintab",
            tabPanel("Segmented",
              plotOutput("seg_predictions")
            ),
            tabPanel("Changepoints",
                     plotOutput("cp_plot")
            ),
            tabPanel("Both",
              list(
                plotOutput("both_plots")
              )

                     
            )
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # reactives
  get_country_data <- reactive({
    country_data %>% 
      filter(code == input$code_select) %>%
      rename(start_age = x) %>% 
      group_by(sex, start_age) %>% 
      arrange(year) %>% 
      mutate(ex_prime = ex - lag(ex)) %>% 
      ungroup() %>% 
      filter(year >= 1980)
  })
  
  get_relevant_cppaths <- reactive({
    cp_paths %>% 
      filter(code == input$code_select) %>% 
      rename(year = x) %>% 
      mutate(path_type = as.numeric(str_remove(path_type, "cp")))
  })
  

  
  get_relevant_segmods <- reactive({
    seg_models %>% 
      filter(code == input$code_select) %>% 
      group_by(code, x, sex) %>% 
      mutate(bic = map_dbl(mdl_outputs, BIC)) %>% 
      mutate(bic_rank = as.character(rank(bic))) %>% 
      mutate(
        predictions = map(
          mdl_outputs, 
          ~tibble(
            ex = predict(.x, newdata = tibble(year = 1980:2018)), 
            year = 1980:2018
          )
        )
      ) %>% 
      unnest(predictions) %>% 
      group_by(code, x, sex, mdl_outputs_id) %>% 
      arrange(year) %>% 
      mutate(ex_prime = ex - lag(ex)) %>% 
      ungroup() %>% 
      mutate(
        num_bp = case_when(
          mdl_outputs_id == "null" ~ 0, 
          mdl_outputs_id == "seg" ~ 1, 
          mdl_outputs_id == "seg2" ~ 2, 
          TRUE ~  NA_real_)
      )
  })
  
  
  
  make_title <- reactive({
    glue::glue("Change in ex for {names(country_selection[country_selection==input$code_select])}")
  })
  
  
  output$cp_plot <- renderPlot({
    gg <- 
      get_relevant_cppaths() %>% 
        ggplot() + 
        geom_path(
          aes(year, y, group  = factor(path_type), colour = factor(path_type), size = factor(path_type), linetype = bic_rank, alpha = bic_rank)
        ) +
      facet_grid(sex ~ start_age) + 
      geom_hline(yintercept = 0) +
      labs(
        x = "year", 
        y = "Annual change in ex (years)",
        title = paste( make_title(), "(CP approach)"),
        colour = "Number of breaks proposed",
        size = "Number of breaks proposed"
      ) + 
    format_legends()
    
    if( input$add_obs) {
      gg <- gg + 
        geom_point(
          aes(year, ex_prime),
          inherit.aes = FALSE,
          data = get_country_data()
        )
    }

    gg
  })
  
  output$seg_table <- renderPrint({
    get_relevant_segmods()
  })
  
  output$seg_predictions <- renderPlot({
    
    gg <- 
      get_relevant_segmods() %>% 
      ggplot(aes(year, ex_prime, group = factor(num_bp), colour = factor(num_bp), size = factor(num_bp), linetype = bic_rank, alpha = bic_rank)) +
      geom_line() +
      facet_grid(x ~ sex) +
      geom_hline(yintercept = 0) +
      labs(
        x = "year", 
        y = "Annual change in ex (years)",
        title = paste( make_title(), "(Segmented approach)"),
        colour = "Number of breaks proposed",
        size = "Number of breaks proposed"
      ) +
      format_legends()
            
      
      
    
    if (input$add_obs) {
      gg <- gg + 
      geom_point(
        aes(x = year, y = ex_prime),
        inherit.aes = FALSE,
        data = get_country_data() %>% rename(x = start_age)
      )
    }
     gg   
  })
  
  output$both_plots <- renderPlot({
    # Find the best performing model from both approaches and plot
    # cp approach 
    gg <- 
    ggplot() + 
      # cp part 
      geom_path(
        aes(year, y),
        data = get_relevant_cppaths() %>% filter(bic_rank == 1),
        inherit.aes = FALSE,
        colour = "blue"
      ) + 
      # seg part
      geom_line(
        aes(year, ex_prime),
        data = get_relevant_segmods() %>% filter(bic_rank == 1) %>% 
          rename(start_age = x),
        inherit.aes = FALSE,
        colour = "red",
        size = 1.2, 
        linetype = "dashed"
      ) + 
      facet_grid(sex ~ start_age) +
      geom_hline(yintercept = 0) + 
      labs(
        x = "Year",
        y = "Change in life expectancy", 
        title = "Comparison of best models from segmented and changepoint approach",
        subtitle = "solid blue line: Changepoint; dashed thicker red line: segmented" 
      )
    
    # get_relevant_cppaths() %>% 
    #   ggplot() + 
    #   geom_path(
    #     aes(year, y, group  = factor(path_type), colour = factor(path_type), size = factor(path_type), linetype = bic_rank, alpha = bic_rank)
    #   ) +
    #   facet_grid(sex ~ start_age) + 
    #   geom_hline(yintercept = 0) +
      
    
    if (input$add_obs) {
      gg <- gg + 
        geom_point(
          aes(x = year, y = ex_prime),
          inherit.aes = FALSE,
          data = get_country_data() #%>% rename(x = start_age)
        )
    }
    gg   
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)



