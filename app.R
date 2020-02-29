library(readr)
voice = read.csv("voice.csv")

voice$label = as.factor(voice$label)
library(shiny)

tags$hr()
library(ggplot2)
# Define UI for application
ui = fluidPage(
  
  # Application title
  titlePanel(h1("Data Exploratory Analysis",align="center",style="color:white;background-color: #669999")),
  tags$hr(),
  tags$div(
  fluidRow(
    # Dropbox to choose a particular column or attribute
    sidebarLayout(
      sidebarPanel(
        selectInput("xcol","X-variable",choices = names(voice))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
       plotOutput("distPlot")
      )
    ),style="background-color:#BCA9F5")
  ),
  tags$br(),
  tags$div(
  fluidRow(
    # Dropbox to choose a particular column or attribute
    sidebarLayout(
      sidebarPanel(
        selectInput("x","X-variable",choices = names(voice))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("distPlot1")
      )
    )
  )
  ,style="background-color:#ffcc99")
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    # draw the histogram with the specific column
    par(bg="#BCA9F5")
    hist(voice[,input$xcol], col = 'darkgray',main = "Univariate Analysis",xlab = input$xcol,border = 'black')
    
  })
  output$distPlot1 <- renderPlot({
    
    # draw the barplot
    par(bg="#ffcc99")
    barplot(table(voice$label),voice[,input$x], col = c('red','blue'),main = "Bivariate Analysis",xlab = input$x)
    
  })
}


# Run the application 
shinyApp(ui = ui, server = server)


