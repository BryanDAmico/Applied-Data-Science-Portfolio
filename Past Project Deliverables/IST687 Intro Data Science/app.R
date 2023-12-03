library(tidyverse)
library(rMIDAS)
library(PerformanceAnalytics)
library(gghighlight)
library(ggplot2)
library(shiny)
library(imputeTS)
library(dplyr)
# devtools::install_github("tidyverse/tibble")
set.seed(123)

df <- read_csv("songs_normalize.csv")
df <- na_interpolation(df)
cl <- kmeans(df$popularity, 3, nstart = 10)
df$cluster <- as_factor(cl$cluster)


########################################################

ui <- fluidPage(
  titlePanel("Exploring Spotify Music by Popularity"),
  sidebarLayout(
    sidePanel <- sidebarPanel(
      sliderInput(
        inputId = "year",
        label = "Year",
        min = 1998,
        max = 2020,
        step = 1,
        value = c(2019, 2020),
        sep = ''
      ),
      
      selectInput(
        inputId = "x",
        label = "Variable (x):",
        choices = c(colnames(df)),
        selected = "acousticness"
      ),
      
      selectInput(
        inputId = "y",
        label = "Variable (y):",
        choices = "popularity"
      ),
      
      selectInput(
        inputId = "g",
        label = "Genre:",
        choices = unique(df['genre'])
      ),
      
    ),
    mainPanel(tabsetPanel(
      tabPanel("Scatter Plot", plotOutput("scatterplot")),
      tabPanel("Box Plot", plotOutput("boxplot")),
      tabPanel(
        "Summary",
        h2("Summary"),
        verbatimTextOutput("sum"),
        verbatimTextOutput("avg"),
        verbatimTextOutput("test")
      ),
    ))
  ),
  
  tabPanel("verticalLayout()",
           verticalLayout(dataTableOutput('table')))
)

########################################################

server <- function(input, output) {
  data <- reactive({
    df %>%
      dplyr::filter(df$year >= input$year[1], df$year <= input$year[2])
  })
  
  # output$test <- renderPrint({
  #   dplyr::filter(data(),genre == input$g)
  # })
  
  output$scatterplot <- renderPlot({
    p = ggplot(data = dplyr::filter(data(),genre == input$g),
               mapping = aes(
                 x = input$x,
                 y = input$y,
                 colour = cluster
               )) + aes_string(x =
                                 input$x, y = input$y) + geom_point() + theme_classic()
    plot(p)
  })
  
  output$boxplot <- renderPlot({
    p1 = ggplot(data = dplyr::filter(data(),genre == input$g),
                mapping = aes(
                  x = input$x,
                  y = input$y,
                  fill = cluster
                  
                )) + aes_string(x =
                                  input$x, y = input$y) + geom_boxplot() + theme_classic()
    plot(p1)
  })
  
  output$table <- renderDataTable({
    dplyr::filter(data(),genre == input$g)
  })
  
  output$avg <- renderPrint({
    dplyr::filter(data(),genre == input$g) %>%
      group_by(cluster) %>%
      summarise(
        "Mean" = mean(get(input$x)),
        "Median" = median(get(input$x)),
        "Max" = max(get(input$x)),
        "Min" = min(get(input$x))
      )
  })
}

########################################################

shinyApp(ui = ui, server = server)