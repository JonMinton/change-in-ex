library(tidyverse, here)
library(shiny)

projdir <- here("apps", "compare_cps")

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
country_data <- read_rds(here(projdir, "data", "countries_data.rds"))


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Compare changepoint models"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("code_select", "Select a country", choices = country_selection)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("mainplot")
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
  
  make_title <- reactive({
    glue::glue("Change in ex for {names(country_selection[country_selection==input$code_select])}")
  })
  
  
  output$mainplot <- renderPlot({
      get_country_data() %>% 
      ggplot(aes(year, ex_prime)) + 
      geom_point() + 
      facet_grid(sex ~ start_age) + 
      geom_hline(yintercept = 0) +
      labs(
        x = "year", 
        y = "Annual change in ex (years)",
        title = make_title()
      ) +
      geom_path(
        aes(
          year, y, group = path_type, colour = path_type,
          linetype = bic_rank, alpha = bic_rank
        ),
        inherit.aes = FALSE,
        data = get_relevant_cppaths()
      ) + 
      scale_alpha_manual("BIC", values = c("1" = 1.0, "2" = 0.5, "3" = 0.25)) +
      scale_linetype_manual(values = c("1" = "solid", "2" = "dashed", "3" = "dashed")) + 
      guides(alpha = "none", linetype = "none") 
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



