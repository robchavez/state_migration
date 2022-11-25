# so shiny!


library(shiny)
library(tidyverse)
library(usmap)
library(cowplot)

# preload data 
all_df <- read_csv('https://www.dropbox.com/s/mbktc0etdcgqg9r/migration_dataframe.csv?dl=1')
app_state <- unique(all_df$current)
app_year <- unique(all_df$year)

# define plotting function
migmap <- function(states, years){
  
  
  map1 <- all_df %>% filter(current == states, year == years) %>% 
    rename(state = left) %>% 
    mutate(log_count = log(count))
  
  atitle <- paste("Came to", states,"from:")
  
  p1 <- plot_usmap(data = map1, values = "count", color = "gray75") + 
    scale_fill_viridis_c(option = 'inferno', direction = 1) +
    theme(legend.position = "bottom",legend.key.width = unit(1.6,"cm"),
          legend.key.height = unit(.4,"cm")) + 
    labs(title = atitle)
  
  p2 <- plot_usmap(data = map1, values = "log_count", color = "gray75") + 
    scale_fill_viridis_c(option = 'inferno', direction = 1) +
    theme(legend.position = "bottom",legend.key.width = unit(1.6,"cm"),
          legend.key.height = unit(.4,"cm"))+ 
    labs(title = paste("(log)",atitle))
  
  
  
  map2 <- all_df %>% filter(left == states, year == years) %>% 
    rename(state = current) %>% 
    mutate(log_count = log(count))
  
  ltitle <- paste("Left", states,"to:")
  p3 <- plot_usmap(data = map2, values = "count", color = "gray75") + 
    scale_fill_viridis_c(option = 'inferno', direction = 1) +
    theme(legend.position = "bottom",legend.key.width = unit(1.6,"cm"),
          legend.key.height = unit(.4,"cm")) + 
    labs(title = ltitle)
  
  p4 <- plot_usmap(data = map2, values = "log_count", color = "gray75") + 
    scale_fill_viridis_c(option = 'inferno', direction = 1) +
    theme(legend.position = "bottom",legend.key.width = unit(1.6,"cm"),
          legend.key.height = unit(.4,"cm")) + 
    labs(title = paste("(log)",ltitle))
  
  
  mp <- plot_grid(p1, p2, p3, p4, nrow = 2)
  
  return(mp)
  
}


# Define UI for application 
ui <- fluidPage(
  
  # Application title
  titlePanel("State-to-State Migration"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      selectInput("app_state", label = "select state",
                  choices = app_state),
      
      selectInput("app_year", label = "select year",
                  choices = app_year),
      
      hr(),
      helpText("")
      , width = 3),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("statePlot", height = 900, width = 900 )  
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$statePlot <- renderPlot({
      migmap(input$app_state,input$app_year) 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
