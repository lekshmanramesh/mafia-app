#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(grid)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  #victor_dummy2=reactive({ifelse(rnorm(1)<0,"wolf","villager")})
    #tags$audio(src = "clip.wav", type = "audio/wav", autoplay=T, controls = TRUE)

  output_data=reactiveValues(data=NULL)
  counter=reactiveValues(count=0)
  killed=reactiveValues(data=NULL)
  observeEvent(
    
    eventExpr=input[["submit_loc"]],
    handlerExpr = {
      
      counter$count=counter$count+1 
      if (counter$count<=input$Players){
      game_data=data.frame(names=input$Name, role=ifelse(rnorm(1)<0,"wolf","villager"), status="A")
      game_data$status=as.character(game_data$status)
      output_data$data=rbind(output_data$data, game_data)
      }
      else if ((counter$count-4)%%4==1){killed$data=input$Name}
      else if ((counter$count-4)%%4==3){killed$data=ifelse(input$Name==killed$data,NA,killed$data)
        output_data$data[output_data$data$names==killed$data,3]="D"
      }
    output$distPlot1 <- renderUI({
      if (counter$count<=4){
      paste("You are a ",game_data$role)}
      else if ((counter$count-4)%%4==1){paste0("You chose to kill ",input$Name)}
      else if ((counter$count-4)%%4==2){paste0("You chose to identify ",input$Name)}
      else if ((counter$count-4)%%4==3){paste0("You chose to save ",input$Name)}
      else {paste0("You voted out ",input$Name)}
      #"Please enter your name"
    #tags$audio(src = "clip.wav", type = "audio/wav", autoplay=T, controls = TRUE)
    
  }) 
    output$distPlot2 <- renderUI({
      #paste("You are a ",ifelse(rnorm(1)<0,"wolf","villager"))
      #"Please enter your name"
      #tags$audio(src = "clip.wav", type = "audio/wav", autoplay=T, controls = TRUE)
      
    })
    
    output$distPlot3 <- renderTable({
      #paste("You are a ",ifelse(rnorm(1)<0,"wolf","villager"))
      #"Please enter your name"
      #output_data$data
      
    })
    }  
  )

  observeEvent(
    
    eventExpr=input[["done_loc"]],
    handlerExpr = {

      #game_data=data.frame(names=input$Name, role=ifelse(rnorm(1)<0,"wolf","villager"))  
      
      #output_data$data=rbind(output_data$data, game_data)
      output$distPlot1 <- renderUI({
        #check=game_data$role
        #paste("You are a ",check)
        "Please enter your name"
        #tags$audio(src = "clip.wav", type = "audio/wav", autoplay=T, controls = TRUE)
        
      }) 
      output$distPlot2 <- renderUI({
        #paste("You are a ",ifelse(rnorm(1)<0,"wolf","villager"))
        #"Please enter your name"
        Sys.sleep(5)
        if (counter$count<3){
        
        tags$audio(src = "clip.wav", type = "audio/wav", autoplay=T)
        }
      })
      
      output$distPlot3 <- renderTable({
        #paste("You are a ",ifelse(rnorm(1)<0,"wolf","villager"))
        #"Please enter your name"
        output_data$data
        
      })
    }  
  )
  
})
