#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(MASS)
library(ggplot2)

# Define UI for application that draws a scatter plot
ui <- fluidPage(

    # Application title
    titlePanel("Weight versus age of chicks (in no of days) on different diets"),

    # Sidebar with a slider input for Time Range and Type of Diet to be used for plotting as well as selecting type of point to be used for plotting and choice of adding regression line
    sidebarLayout(
        sidebarPanel(
            
            radioButtons("Diet", 
                         label = "Select Type of Diet: ",
                         choices = list("1" = 1, "2" = 2, "3" = 3, "4"= 4),
                         selected = 1),
            br(),   br(),
            
            sliderInput("TimeRange",
                        "Select Time Range (Age in no of days):",
                        min = 0,
                        max = 21,
                        value=c(0, 15), step=1),
            br(),   br(),
            
            sliderInput("PlotPointType",
                        "Select type of point to be used in plotting the scatter plot:",
                        min = 0,
                        max = 25,
                        value = 3),
            br(),   br(),
            
            radioButtons("Regression", 
                         label = "Add Regression Line: ",
                         choices = list("Yes" = 1, "No" = 0),
                         selected = 0),
            br(),   br(),
            
            tags$style("body{background-color:lightgreen; color:dark grey}")
            ),

        # Show a plot of the generated scatter plot
        mainPanel(
           plotOutput("ScatterPlot")
        )
    )
)

# Define server logic required to draw a scatter plot
server <- function(input, output) {
    myFinalData <- reactive({
        #------------------------------------------------------------------
        # Select data according to selection of radio button
        mydata <- ChickWeight
        mydata$Diet <- as.numeric(as.character(mydata$Diet))
        mydata$Time <- as.numeric(mydata$Time)
        mydata <- mydata[mydata$Diet==input$Diet,]
        #------------------------------------------------------------------
        # Get data rows for selected time range
        mydata1 <- mydata[mydata$Time >= input$TimeRange[1], ]
        mydata1 <- mydata1[mydata$Time <= input$TimeRange[2], ]
        
                #------------------------------------------------------------------
        #------------------------------------------------------------------
        # Get data rows for selected time range in a data frame
        data.frame(mydata1)
        #------------------------------------------------------------------
        
    })
    
    
    output$ScatterPlot <- renderPlot({
        # generate plot based on input$TimeRange, input$Diet, input$PlotPointType and input$Regression from ui.R
        plotdata <- myFinalData()
        
        # draw the plot
        
        g <- ggplot(plotdata, aes(x = Time, y= weight)) + geom_point(shape=input$PlotPointType)
        
        if(input$Regression==1){
            g <- g + geom_smooth()
        }
        
        print(g)
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
