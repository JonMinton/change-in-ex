library(tidyverse)
library(here)
library(shiny)

projdir <- here("apps", "compare_cps_and_bps")

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

cp_paths <- read_rds(here(projdir, "data", "cp_paths.rds"))
seg_models <- read_rds(here(projdir, "data", "segmented_models.rds"))
country_data <- read_rds(here(projdir, "data", "countries_data.rds"))


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Compare changepoint models"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("code_select", "Select a country", choices = country_selection),
          checkboxInput("add_obs", "Click to add data points")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(id = "maintab",
            tabPanel("Changepoints",
               plotOutput("cp_plot")
            ),
            tabPanel("Segmented",
              plotOutput("seg_predictions")
            ),
            tabPanel("Both"

                     
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
      rename(year = x) 
  })
  
  get_relevant_segmods <- reactive({
    seg_models %>% 
      filter(code == input$code_select) 
  })
  
  
  
  make_title <- reactive({
    glue::glue("Change in ex for {names(country_selection[country_selection==input$code_select])}")
  })
  
  
  output$cp_plot <- renderPlot({
    gg <- 
      get_relevant_cppaths() %>% 
        ggplot() + 
        geom_path(
          aes(year, y, group  = path_type, colour = path_type, linetype = bic_rank, alpha = bic_rank)
        ) +
      facet_grid(sex ~ start_age) + 
      geom_hline(yintercept = 0) +
      labs(
        x = "year", 
        y = "Annual change in ex (years)",
        title = paste( make_title(), "(CP approach)")
      ) + 
    scale_alpha_manual("BIC", values = c("1" = 1.0, "2" = 0.5, "3" = 0.25)) +
      scale_linetype_manual(values = c("1" = "solid", "2" = "dashed", "3" = "dashed")) + 
      guides(alpha = "none", linetype = "none")   
    
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
        ggplot(aes(year, ex_prime, group = mdl_outputs_id, colour = mdl_outputs_id)) +
        geom_line() +
        facet_grid(x ~ sex) +
        geom_hline(yintercept = 0) +
        labs(
          x = "year", 
          y = "Annual change in ex (years)",
          title = paste( make_title(), "(Segmented approach)")
        ) 
      
    
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
}

# Run the application 
shinyApp(ui = ui, server = server)



