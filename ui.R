#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  headerPanel("Welcome to the Village"),
  
  # Sidebar with a slider input for number of bins. Create as many Inputs as you want 
  sidebarPanel(

           numericInput("Players", "Players:",
                     8),
           textInput("Name", "Name:",
                "check"),
           actionButton(inputId = "submit_loc",
                        label = "Submit"),
           actionButton(inputId = "done_loc",
                        label = "I'm Done")
  ),
    
    # Show a plot of the generated distribution
    mainPanel(uiOutput("distPlot1"),
              uiOutput("distPlot2"),
              uiOutput("distPlot3"))
  )
)