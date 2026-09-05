#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)
library(dplyr)
library(ggplot2)
library(readr)

august2019 <- read_csv("statcastAugust2019.csv")
my_pitches <- august2019
masterID <- read_csv("master.csv")

august2019 %>% group_by(pitcher) %>% summarise() %>% inner_join(masterID, by = c("pitcher" = "mlb_id")) %>%
  select(pitcher, mlb_name) %>% arrange((mlb_name)) -> possiblePitchers

# Convert df to list
possiblePitchersList <- as.list(possiblePitchers$pitcher)
names(possiblePitchersList) <- possiblePitchers$mlb_name

# Build strike zone
plate_width <- 17 + (9/pi)

k_zone_plot <- ggplot(NULL, aes(x = plate_x, y = plate_z)) +
  geom_rect(xmin = -(plate_width/2)/12,
            xmax = (plate_width/2)/12,
            ymin = 1.5,
            ymax = 3.6, color = "lavender", alpha = 0) +
  coord_equal() +
  scale_x_continuous("Horizontal location (ft.)",
                     limits = c(-2, 2)) +
  scale_y_continuous("Vertical location (ft.)",
                     limits = c(0, 5))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("Distribution of Pitch Types for Taken Pitches – August 2019"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "selectPitcher",
        label = "Select a Pitcher:",
        choices = possiblePitchersList
      ),
      
      sliderInput(
        inputId = "bins",
        label = "Number of Bins:",
        min = 1,
        max = 500,
        value = 30
      )
    ),
    
    mainPanel(
      plotOutput(outputId = "distPlot")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "types",
        label = h3("Types of Outcomes"),
        choices = list("Called Strike (S)" = "S",
                       "Ball (B)" = "B",
                       "In-Play (X)" = "X"),
        selected = c("S", "B", "X")
      )
    ),
    
    mainPanel(
      plotOutput(outputId = "pitchChart")
    )
  )
  
)

server <- function(input, output) {
  SelectedPitcherReactive <- reactive({
    my_pitches %>% filter(pitcher == input$selectPitcher)
  })
  # Pitch chart
  output$pitchChart <- renderPlot({
    df <- SelectedPitcherReactive() %>% filter(type %in% input$types)
    
    k_zone_plot %+% df +
      aes(color = pitch_name, shape = type) +
      geom_point() +
      ggtitle("Pitch Chart")
  })
  
  # Histogram
  output$distPlot <- renderPlot({
    
    taken <- SelectedPitcherReactive() %>%
      filter(description %in% c("called_strike", "ball"))
    
    taken$zFromCenter <- abs(taken$plate_z - 2.55)
    taken$xFromCenter <- abs(taken$plate_x)
    taken$distanceFromCenter <- sqrt(taken$xFromCenter^2 + taken$zFromCenter^2)
    
    ggplot(taken, aes(x = distanceFromCenter, color = description, label = pitch_type)) +
      geom_histogram(bins = input$bins) +
      ggtitle("Histogram of Pitches Taken") +
      xlab("Feet From Center of Strike Zone") +
      ylab("n")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
