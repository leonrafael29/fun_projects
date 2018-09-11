library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)
library (DT)
library(tools)
load(url("http://s3.amazonaws.com/assets.datacamp.com/production/course_4850/datasets/movies.Rdata"))

name_of_studios <- sort(unique(movies$studio))
ui<-fluidPage(
  titlePanel("Movies"),
  br(),
  br(),
  sidebarLayout(
    sidebarPanel(
      h3("Plotting"),
      
      textInput(inputId = "plot_title",
                label = "Plot title",
                placeholder = "Enter text for plot title"),
      
      sliderInput(inputId ="n",
                  label = "Alpha:",
                  min = 0,
                  max = 1,
                  value = 0.5),
      
      selectInput(inputId ="x",
                  label = "X-axis:",
                  choices = c("IMDB rating"= "imdb_rating",
                              "IMDB number of votes" = "imdb_num_votes",
                              "Critics score" = "critics_score",
                              "Audience score" = "audience_score",
                              "Runtime" = "runtime"),
                  selected = "audience_score"),
      
      selectInput(inputId ="y",
                  label = "Y-axis:",
                  choices = c("IMDB rating"= "imdb_rating",
                              "IMDB number of votes" = "imdb_num_votes",
                              "Critics score" = "critics_score",
                              "Audience score" = "audience_score"),
                  selected = "imdb_rating"),
      
      selectInput(inputId ="z",
                  label = "Color:",
                  choices = c("Title type"= "title_type",
                              "Genre" = "genre",
                              "MPAA Rating" = "mpaa_rating"),
                  selected = "audience_score"),
      
      br(),
      h3("Subplotting"),
      
      selectInput(inputId ="name_studios",
                  label = "Name of studios:",
                  choices = name_of_studios,
                  selected = "Lionsgate",
                  selectize = TRUE,
                  multiple = TRUE)
    ),
    
    mainPanel(
      h3("Plotting"),
      plotOutput(outputId = "scatterplot", hover = "plot_hover", height= 300),
      br(),
      h3("Data"),
      DT::dataTableOutput(outputId = "moviestable"),
      br(),
      h3("Correlation"),
      textOutput(outputId = "correlation")
    )
  )
)
server <- function(input, output) {
  
  movies_selected <- reactive({
    req(input$name_studios) 
    movies %>%
      filter(studio %in% input$name_studios)
  })
  
  output$scatterplot <- renderPlot ({
    ggplot(data=movies_selected(), aes_string(x = input$x, y= input$y)) + geom_point(aes_string(alpha = input$n, color= input$z)) + labs(title = input$plot_title) + theme_gray()
  })
  
  output$correlation <-renderText ({
    movies_selected <- movies %>%
      filter(studio %in% req(input$name_studios))
    r <- round(cor(movies_selected()[, input$x], movies_selected()[, input$y]), 3)
    paste0("The correlation of ", input$x, " and ", input$y, " is equal to ", r)
  })
  
  h3("Data table")
  br()
  output$moviestable <- DT::renderDataTable ({
    nearPoints(movies_selected(), input$plot_hover)%>%
      select(title, audience_score, critics_score)
  })
  
  
}
shinyApp(ui=ui, server=server)