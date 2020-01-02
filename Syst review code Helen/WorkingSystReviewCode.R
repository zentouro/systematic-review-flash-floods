library(shiny)   ;  library(tidyverse)
library(DT)      ;  library(magrittr)
library(stringr) ;  library(shinyWidgets)
library(RISmed)  ;  library(bibliometrix)
library(bib2df)  ;  library(knitr)
library(rmdformats)

rm(list=ls())
setwd("~/Documents/GitHub/systematic-review-flash-floods/Syst review code Helen")
Workingfile <- "2ndScreen_Working.RData"

load(Workingfile)


data_bib$Screen1_Assessed[which(is.na(data_bib$Screen1_Assessed)==TRUE)] <- FALSE
data_bib$Screen1_Reject[which(is.na(data_bib$Screen1_Reject)==TRUE)] <- FALSE
data_bib$Screen2_Assessed[which(is.na(data_bib$Screen2_Assessed)==TRUE)] <- FALSE
data_bib <- data_bib[with(data_bib, order(Screen2_Assessed,Screen1_Reject,Screen1_Assessed)), ]


data_bib <- data_bib[order(data_bib)]
#=======================================================================
# Highlighting Rules
#=======================================================================
wordHighlightyellow <- function(SuspWord,colH = "#FFE9A8") {paste0('<span style="background-color:',colH,'">',SuspWord,'</span>')}
wordHighlightgreen <- function(SuspWord,colH = "#BEDDBA") {paste0('<span style="background-color:',colH,'">',SuspWord,'</span>')}
wordHighlightblue <- function(SuspWord,colH = "#A3C4D9") {paste0('<span style="background-color:',colH,'">',SuspWord,'</span>')}
wordHighlightred <- function(SuspWord,colH = "#CFA6B6") {paste0('<span style="background-color:',colH,'">',SuspWord,'</span>')}
wordHighlightdarkblue <- function(SuspWord,colH = "#BAB4D4") {paste0('<span style="background-color:',colH,'">',SuspWord,'</span>')}
wordHighlightgrey <- function(SuspWord,colH = grey(0.9)) {paste0('<span style="background-color:',colH,'">',SuspWord,'</span>')}


helenhighlight <- function(YourData){
  YourData %<>% str_replace_all(regex("flash ", ignore_case = TRUE), wordHighlightyellow)
  YourData %<>% str_replace_all(regex("flash-", ignore_case = TRUE), wordHighlightyellow)
  YourData %<>% str_replace_all(regex("flash", ignore_case = TRUE), wordHighlightyellow)
  YourData %<>% str_replace_all(regex("floods", ignore_case = TRUE), wordHighlightyellow)
  YourData %<>% str_replace_all(regex("flooding", ignore_case = TRUE), wordHighlightyellow)
  YourData %<>% str_replace_all(regex("flood ", ignore_case = TRUE), wordHighlightyellow)
  YourData %<>% str_replace_all(regex("flood", ignore_case = TRUE), wordHighlightyellow)
  YourData %<>% str_replace_all(regex("flood-", ignore_case = TRUE), wordHighlightyellow)
  YourData %<>% str_replace_all(regex("risk", ignore_case = TRUE), wordHighlightyellow)
  
  YourData %<>% str_replace_all(regex("exposure", ignore_case = TRUE), wordHighlightgreen)
  YourData %<>% str_replace_all(regex("vulnerability", ignore_case = TRUE), wordHighlightgreen)
  YourData %<>% str_replace_all(regex("impacting", ignore_case = TRUE), wordHighlightgreen)
  YourData %<>% str_replace_all(regex("impacts", ignore_case = TRUE), wordHighlightgreen)
  YourData %<>% str_replace_all(regex("impact", ignore_case = TRUE), wordHighlightgreen)
  YourData %<>% str_replace_all(regex("focus group", ignore_case = TRUE), wordHighlightgreen)
  YourData %<>% str_replace_all(regex("stakeholder", ignore_case = TRUE), wordHighlightgreen)
  YourData %<>% str_replace_all(regex("culture", ignore_case = TRUE), wordHighlightgreen)
  YourData %<>% str_replace_all(regex("questionnaire", ignore_case = TRUE), wordHighlightgreen)
  YourData %<>% str_replace_all(regex("killed", ignore_case = TRUE), wordHighlightgreen)
  YourData %<>% str_replace_all(regex("deaths", ignore_case = TRUE), wordHighlightgreen)
  YourData %<>% str_replace_all(regex("death", ignore_case = TRUE), wordHighlightgreen)
  YourData %<>% str_replace_all(regex("damage", ignore_case = TRUE), wordHighlightgreen)
  YourData %<>% str_replace_all(regex("disaster", ignore_case = TRUE), wordHighlightgreen)
  YourData %<>% str_replace_all(regex("economic", ignore_case = TRUE), wordHighlightgreen)
  YourData %<>% str_replace_all(regex("social ", ignore_case = TRUE), wordHighlightgreen)
  YourData %<>% str_replace_all(regex("fatalities", ignore_case = TRUE), wordHighlightgreen)
  
  YourData %<>% str_replace_all(regex("modelling", ignore_case = TRUE), wordHighlightblue)
  YourData %<>% str_replace_all(regex("modeling", ignore_case = TRUE), wordHighlightblue)
  YourData %<>% str_replace_all(regex("models", ignore_case = TRUE), wordHighlightblue)
  YourData %<>% str_replace_all(regex("model", ignore_case = TRUE), wordHighlightblue)
  YourData %<>% str_replace_all(regex("dynamical ", ignore_case = TRUE), wordHighlightblue)
  YourData %<>% str_replace_all(regex("dynamics ", ignore_case = TRUE), wordHighlightblue)
  YourData %<>% str_replace_all(regex("hydro ", ignore_case = TRUE), wordHighlightblue)
  YourData %<>% str_replace_all(regex("dynamic ", ignore_case = TRUE), wordHighlightblue)
  YourData %<>% str_replace_all(regex("gis ", ignore_case = TRUE), wordHighlightblue)
  YourData %<>% str_replace_all(regex("remote sensing", ignore_case = TRUE), wordHighlightblue)
  YourData %<>% str_replace_all(regex("hydrological ", ignore_case = TRUE), wordHighlightblue)
  YourData %<>% str_replace_all(regex("hydrometeorologial ", ignore_case = TRUE), wordHighlightblue)
  YourData %<>% str_replace_all(regex("hydro", ignore_case = TRUE), wordHighlightblue)
  YourData %<>% str_replace_all(regex("discharge", ignore_case = TRUE), wordHighlightblue)
  
  YourData %<>% str_replace_all(regex("convective ", ignore_case = TRUE), wordHighlightdarkblue)
  YourData %<>% str_replace_all(regex("convection ", ignore_case = TRUE), wordHighlightdarkblue)
  YourData %<>% str_replace_all(regex("forecast ", ignore_case = TRUE), wordHighlightdarkblue)
  YourData %<>% str_replace_all(regex("weather",ignore_case = TRUE), wordHighlightdarkblue)
  YourData %<>% str_replace_all(regex("precipitation",ignore_case = TRUE), wordHighlightdarkblue)
  YourData %<>% str_replace_all(regex("rainfall",ignore_case = TRUE), wordHighlightdarkblue)
  YourData %<>% str_replace_all(regex(" rain",ignore_case = TRUE), wordHighlightdarkblue)
  YourData %<>% str_replace_all(regex("radar ",ignore_case = TRUE), wordHighlightdarkblue)
  
  YourData %<>% str_replace_all(regex(" compilation",ignore_case = TRUE), wordHighlightred)
  YourData %<>% str_replace_all(regex("atlas",ignore_case = TRUE), wordHighlightred)
  YourData %<>% str_replace_all(regex(month.name[1],ignore_case = TRUE), wordHighlightred)
  YourData %<>% str_replace_all(regex(month.name[2],ignore_case = TRUE), wordHighlightred)
  YourData %<>% str_replace_all(regex(month.name[3],ignore_case = TRUE), wordHighlightred)
  YourData %<>% str_replace_all(regex(month.name[4],ignore_case = TRUE), wordHighlightred)
  YourData %<>% str_replace_all(regex(month.name[6],ignore_case = TRUE), wordHighlightred)
  YourData %<>% str_replace_all(regex(month.name[7],ignore_case = TRUE), wordHighlightred)
  YourData %<>% str_replace_all(regex(month.name[8],ignore_case = TRUE), wordHighlightred)
  YourData %<>% str_replace_all(regex(month.name[9],ignore_case = TRUE), wordHighlightred)
  YourData %<>% str_replace_all(regex(month.name[10],ignore_case = TRUE), wordHighlightred)
  YourData %<>% str_replace_all(regex(month.name[11],ignore_case = TRUE), wordHighlightred)
  YourData %<>% str_replace_all(regex(month.name[12],ignore_case = TRUE), wordHighlightred)
  YourData %<>% str_replace_all(regex("190",ignore_case = TRUE), wordHighlightred)
  YourData %<>% str_replace_all(regex("191",ignore_case = TRUE), wordHighlightred)
  YourData %<>% str_replace_all(regex("192",ignore_case = TRUE), wordHighlightred)
  YourData %<>% str_replace_all(regex("193",ignore_case = TRUE), wordHighlightred)
  YourData %<>% str_replace_all(regex("194",ignore_case = TRUE), wordHighlightred)
  YourData %<>% str_replace_all(regex("195",ignore_case = TRUE), wordHighlightred)
  YourData %<>% str_replace_all(regex("196",ignore_case = TRUE), wordHighlightred)
  YourData %<>% str_replace_all(regex("197",ignore_case = TRUE), wordHighlightred)
  YourData %<>% str_replace_all(regex("198",ignore_case = TRUE), wordHighlightred)
  YourData %<>% str_replace_all(regex("199",ignore_case = TRUE), wordHighlightred)
  YourData %<>% str_replace_all(regex("200",ignore_case = TRUE), wordHighlightred)
  YourData %<>% str_replace_all(regex("201",ignore_case = TRUE), wordHighlightred)
  YourData %<>% str_replace_all(regex("202",ignore_case = TRUE), wordHighlightred)
}

#=======================================================================
# GUI function
#=======================================================================
ui <- fluidPage(
  #--------------------------------------------------------------------
  # This creates a shiny sidebar.  There are other options
  sidebarLayout(
    sidebarPanel(
      
      #--------------------------------------------------------------------
      # Token text followed by a blank line
      tags$header(tags$p("Choose a flood data classification")),
      hr(),
      
      #--------------------------------------------------------------------
      # The individual check boxes
      fluidRow(materialSwitch(inputId="discardButton", label="Not Relevant",value=FALSE,width="100%",status="danger")),
      fluidRow(materialSwitch(inputId="rainButton", label="Rainfall paper", value=FALSE,width="100%",status="danger")),
      fluidRow(materialSwitch(inputId="modelButton", label="Model/Forecast/Maps", value=FALSE,width="100%",status="danger")),
      fluidRow(materialSwitch(inputId="socialButton", label="Social", value=FALSE,width="100%",status="danger")),
      hr(),
      fluidRow(materialSwitch(inputId="eventButton", label="Event", value=FALSE,width="100%",status="danger")),
      
      #--------------------------------------------------------------------
      # The next button
      hr()#,
      #fluidRow(actionButton("nextButton", "Next!"))
    ),
    
    #--------------------------------------------------------------------
    # Where the main data resides. 
    # The table itself and whether it has been pre-screened
    mainPanel(
      DT::dataTableOutput("table"),
      hr(),
      column(3,offset=1,htmlOutput("covidence")),
      column(3,offset=1,htmlOutput("reject"   ))
    ), 
    
    #--------------------------------------------------------------------
    # Where the sidebar sits (left or right). 
    position="right"
  )
)

#=======================================================================
# Server
#=======================================================================
server <-  function(input,output,session){
  
  #--------------------------------------------------------------------
  # Create a "reactive value" which allows us to play with the output of a button click
  values <- reactiveValues(); values$count <- 0


  #--------------------------------------------------------------------
  # At the same time on a next click (bloody shiny), 
  # select the row you care about and highlight it
  highlighter <- eventReactive(
    {#input$nextButton 
     input$discardButton 
     input$rainButton
     input$modelButton
     input$socialButton
      },
    {
      updateMaterialSwitch(session=session, inputId="discardButton",value=FALSE)
      updateMaterialSwitch(session=session, inputId="rainButton",value=FALSE)
      updateMaterialSwitch(session=session, inputId="modelButton",value=FALSE)
      updateMaterialSwitch(session=session, inputId="socialButton",value=FALSE)
      updateMaterialSwitch(session=session, inputId="eventButton",value=FALSE)
      save(list="data_bib",file=Workingfile)

      
      #-----------------------------------------------------
      # If the row number is not at the end, increment up
      # THIS IS *REALLY BAD CODING*, ADDED IN BECAUSE IT WANTS TO RECALCULATE THE VALUE.
      if(sum(c(input$discardButton,input$rainButton,input$modelButton,input$socialButton))>0){
        if(values$count != nrow(data_bib)){
          #-----------------------------------------------------
          # move to the next row
           values$count <- values$count + 1
        
           #-----------------------------------------------------
           # choose that row in the table
           YourData <- data_bib[values$count,c("TI","AB")]
           YourData2 <- helenhighlight(YourData)
           
           #-----------------------------------------------------
           # Output to data_bib         
           data_bib$Screen2_Assessed[values$count-1] <<- TRUE
           data_bib$Screen2_Reject  [values$count-1] <<- input$discardButton
           data_bib$Screen2_Event   [values$count-1] <<- input$eventButton
           data_bib$Screen2_Model   [values$count-1] <<- input$modelButton
           data_bib$Screen2_Precip  [values$count-1] <<- input$rainButton
           data_bib$Screen2_Social  [values$count-1] <<- input$socialButton
           return(YourData2)
        }
        #-----------------------------------------------------
        # Or put the final row
        else{
          YourData <- data_bib[ nrow(data_bib),c("TI","AB")]
          YourData2 <- helenhighlight(YourData)
          
          #-----------------------------------------------------
          # Output to data_bib         
          data_bib$Screen2_Assessed[nrow(data_bib)] <<- TRUE
          data_bib$Screen2_Reject  [nrow(data_bib)] <<- input$discardButton
          data_bib$Screen2_Event   [nrow(data_bib)] <<- input$eventButton
          data_bib$Screen2_Model   [nrow(data_bib)] <<- input$modelButton
          data_bib$Screen2_Precip  [nrow(data_bib)] <<- input$rainButton
          data_bib$Screen2_Social  [nrow(data_bib)] <<- input$socialButton
          return(YourData2)
        }
      }else{
        if(values$count != nrow(data_bib)){
          #-----------------------------------------------------
          # choose that row in the table
          YourData <- data_bib[values$count,c("TI","AB")]
          YourData2 <- helenhighlight(YourData)
          return(YourData2)
        }
        #-----------------------------------------------------
        # Or put the final row
        else{
          YourData <- data_bib[ nrow(data_bib),c("TI","AB")]
          YourData2 <- helenhighlight(YourData)
          return(YourData2)
        }
      }
    })  
  
  #--------------------------------------------------------------------
  # Highlight whether it has been screened or not in covidence
  highlightCovidence <- reactive({
    YourText2 = as.character(data_bib$Screen1_Assessed[values$count])
    if(YourText2=="FALSE"){
      YourText3 <- "Covidence: NO"
      YourText3 %<>% str_replace_all(regex("Covidence: NO",ignore_case = TRUE), wordHighlightgrey)
    }else{
      YourText3 <- "Covidence: YES"
      YourText3 %<>% str_replace_all(regex("Covidence: YES",ignore_case = TRUE), wordHighlightgreen)
    }
    return(YourText3)
  })   
  
  #--------------------------------------------------------------------
  # Highlight whether it has been rejected
  highlightExclude <- reactive({
    YourText2 = as.character(data_bib$Screen1_Reject[values$count])
    if(YourText2=="FALSE"){
      YourText3 <- "Reject: NO"
      YourText3 %<>% str_replace_all(regex("Reject: NO",ignore_case = TRUE), wordHighlightgrey)
    }else{
      YourText3 <- as.character(data_bib$Screen1_Reject[values$count])#"Reject: YES"
      YourText3 %<>% str_replace_all(regex("Reject: YES",ignore_case = TRUE), wordHighlightred)
    }
    return(YourText3)
  }) 
  
  #--------------------------------------------------------------------
  # Output the table to the GUI

  output$table <- DT::renderDataTable({ data <- highlighter()}, escape = FALSE,options = list(dom = 't',bSort=FALSE)) 
  output$covidence <- renderUI({HTML(paste(highlightCovidence(), sep = '<br/>'))})
  output$reject <- renderUI({ HTML(paste(highlightExclude(), sep = '<br/>'))})

}

#=======================================================================
# Run the server
#=======================================================================
shinyApp(ui = ui, server = server)

