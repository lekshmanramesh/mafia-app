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

    
  Village=reactive({wolves=round((input$Players-2)/3)
  villagers=input$Players-2-wolves
  roles=c(rep("Villager",villagers),rep("Mafia",wolves),"Cop","Doctor")
  clicks=1:input$Players
  roles=sample(roles,input$Players)
  return(data.frame(clicks=clicks,role=roles))})
  
  output_data=reactiveValues(data=NULL)
  counter=reactiveValues(count=0)
  killed=reactiveValues(data=NULL)
  identified=reactiveValues(data=NULL)
  alive=reactiveValues(data=NULL)
  alive_no=reactiveValues(data=NULL)
  per_mafia=reactiveValues(data=NULL)
  voted_out=reactiveValues(data=NULL)
  

  output$distPlot2 <- renderUI({
    #paste("You are a ",ifelse(rnorm(1)<0,"wolf","villager"))
    #"Please enter your name"
    #Sys.sleep(45)
    #if (counter$count<3){
    
    tags$audio(src = "Game Begins- Final.wav", type = "audio/wav", autoplay=T, controls = TRUE)
    #tags$audio(src = "Players Box.wav", type = "audio/wav", autoplay=T, controls = TRUE)
    #}
  })
  
  
  observeEvent(
    
    eventExpr=input[["submit_loc"]],
    handlerExpr = {
      counter$count=counter$count+1

      #print(alive_no$data)
      #print(prev_count$data)
      #print(counter$count)
      #print(Village())
      if (counter$count<=input$Players){
      game_data=data.frame(names=input$Name, role=Village()$role[Village()$clicks==counter$count], status="A")
      game_data$status=as.character(game_data$status)
      output_data$data=rbind(output_data$data, game_data)
      #prev_count$data=input$Players
      }
      else {if ((counter$count-input$Players)%%alive_no$data==1){killed$data=input$Name}
      if ((counter$count-input$Players)%%alive_no$data==2){if (output_data$data[output_data$data$role=="Cop",3]=="A"){
        identified$data=output_data$data$role[output_data$data$names==input$Name]
        identified$data=ifelse(identified$data=="Mafia","Mafia","Villager")}
        else {counter$count=counter$count+1}
      }
      if ((counter$count-input$Players)%%alive_no$data==3){if (output_data$data[output_data$data$role=="Doctor",3]=="A"){
        if (input$Name!=killed$data){
          output_data$data[output_data$data$names==killed$data,3]="D"
        }
        else (killed$data="Nobody")
      }
        else {counter$count=counter$count+1}
        }
      if ((counter$count-input$Players)%%alive_no$data==0){output_data$data[output_data$data$names==input$Name,3]="D"
        voted_out$data=output_data$data$role[output_data$data$names==input$Name]
        voted_out$data=ifelse(voted_out$data=="Mafia","Mafia","Villager")
        }
      }
      
      alive$data=output_data$data[output_data$data$status=="A",]
      alive_no$data=length(unique(output_data$data$role))
      per_mafia$data=length(alive$data[alive$data$role=="Mafia",1])/nrow(alive$data)
      
      
          output$distPlot1 <- renderUI({
      if (counter$count<=input$Players){
      paste("You are a ",game_data$role)}
      else if ((counter$count-input$Players)%%alive_no$data==1){if (per_mafia$data>=0.5){"Mafia Win. Game Over!!"}
        else if (per_mafia$data==0){"Villagers Win. Game Over!!"}
        else {paste0("You chose to kill ",input$Name)}}
      else if ((counter$count-input$Players)%%alive_no$data==2){if (per_mafia$data>=0.5){"Mafia Win. Game Over!!"}
        else if (per_mafia$data==0){"Villagers Win. Game Over!!"}
        else {paste0("You chose to identify ",input$Name, " and you have identified a ", identified$data)}}
      else if ((counter$count-input$Players)%%alive_no$data==3){if (per_mafia$data>=0.5){"Mafia Win. Game Over!!"}
        else if (per_mafia$data==0){"Villagers Win. Game Over!!"}
        else {paste0("You chose to save ",input$Name)}}
      else if ((counter$count-input$Players)%%alive_no$data==0){if (per_mafia$data>=0.5){"Mafia Win. Game Over!!"}
        else if (per_mafia$data==0){"Villagers Win. Game Over!!"}
        else {paste0("You voted out ",input$Name, " and you killed a ", voted_out$data)}
      }
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
        #Sys.sleep(45)
        if (counter$count<input$Players){
        "Please enter your name"
        }
        else if ((counter$count-input$Players)%%alive_no$data==0){"Who are you killing tonight?"}
        else if ((counter$count-input$Players)%%alive_no$data==1){"Suspect someone?? Enter their name"}
        else if ((counter$count-input$Players)%%alive_no$data==2){"Save someone! Enter their name"} 
        else if ((counter$count-input$Players)%%alive_no$data==3){paste0(killed$data," is dead. Who are you voting out this evening? Let me know soon")} 
        #tags$audio(src = "clip.wav", type = "audio/wav", autoplay=T, controls = TRUE)
        
      }) 
      
      output$distPlot3 <- renderTable({
        #paste("You are a ",ifelse(rnorm(1)<0,"wolf","villager"))
        #"Please enter your name"
        output_data$data
      })
      
      output$distPlot2 <- renderUI({
        #paste("You are a ",ifelse(rnorm(1)<0,"wolf","villager"))
        #"Please enter your name"
        #Sys.sleep(45)
        #if (counter$count<3){
        Sys.sleep(30)
        if (counter$count<input$Players){
          tags$audio(src = "Next Player.wav", type = "audio/wav", autoplay=T, controls = TRUE)
        }
        else if ((counter$count-input$Players)%%alive_no$data==0){tags$audio(src = "mafia call.wav", type = "audio/wav", autoplay=T, controls = TRUE)}
        else if ((counter$count-input$Players)%%alive_no$data==1){tags$audio(src = "Cop Call.wav", type = "audio/wav", autoplay=T, controls = TRUE)}
        else if ((counter$count-input$Players)%%alive_no$data==2){tags$audio(src = "Doc Call.wav", type = "audio/wav", autoplay=T, controls = TRUE)} 
        else if ((counter$count-input$Players)%%alive_no$data==3){
          #Sys.sleep(15)
          tags$audio(src = "Its Day Time.wav", type = "audio/wav", autoplay=T, controls = TRUE)} 
        #tags$audio(src = "clip.wav", type = "audio/wav", autoplay=T, controls = TRUE)
        
        #}
      })
      

    }  
  )
  
})
