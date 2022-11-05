#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("行列と固有値・固有ベクトル"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("x1",
                        h3("first value"),
                        value = 1/sqrt(2)),
            numericInput("x2",
                         h3("second value"),
                         value = 1/sqrt(2)),
            # random set
            h3("random setting"),
            actionButton("random", "Random")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("matrix_as_mapping")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # generate random value
    gen_val <- function() {
      if(sample(c(0,1), 1)) {
        return(rnorm(2, 0, 10))
      }else{
        if(sample(c(0,1), 1)) {
          # lambda=4
          return(c(1/sqrt(2), 1/sqrt(2)))
        }else{
          # lambda=1
          return(c(2/sqrt(5), -1/sqrt(5)))
        }
      }
    }
  
    # random button
    observeEvent(input$random, {
      new_val <- gen_val()
      updateNumericInput(session, "x1", value=new_val[1])
      updateNumericInput(session, "x2", value=new_val[2])
    })

    output$matrix_as_mapping <- renderPlotly({
        # input vector
        x1 <- input$x1
        x2 <- input$x2
        
        # transformed vector
        A <- matrix(c(2,2,1,3), ncol=2, byrow=T)
        Ax <- A %*% c(x1, x2)
        
        df <- data.frame(
          x1=c(0,x1),    y1=c(0,x2),
          x2=c(0,Ax[1]), y2=c(0,Ax[2])
        )
        
        # axis info
        axislim <- max(abs(c(x1,x2,Ax[1], Ax[2]))) * 1.1
        
        # draw vectors
        plot_ly(df, x=~x1, y=~y1, type="scatter", mode="lines+markers", 
                opacity=0.5,
                name="x") %>%
          add_trace(x=~x2, y=~y2, type="scatter", mode="lines+markers", 
                opacity=0.5,
                name="Ax") %>% 
          layout(
            xaxis = list(range=c(-axislim, axislim)),
            yaxis = list(range=c(-axislim, axislim))
          )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
