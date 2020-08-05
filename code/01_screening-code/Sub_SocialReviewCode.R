library(shiny)   ;  library(tidyverse)
library(DT)      ;  library(magrittr)
library(stringr) ;  library(shinyWidgets)
library(RISmed)  ;  library(bibliometrix)
library(bib2df)  ;  library(knitr)
library(shinythemes)
library(rmdformats)

#=======================================================================
# THIS IS THE SOCIAL REVIEW CODE.  IT IS SET TO NOT SAVE ANY DATA TO FILE
# WHILE WE BUILD IT.
#=======================================================================


# THIS CODE OVERWRITES YOUR INPUT DATA. MAKE A COPY BEFORE YOU START 
# (or even better - immedietly after you finish screening)
# Place backup in data/03_backup-screening-data 
# title backup file 'XXXX-screeningData-YY'
# XXXX - month and day 
# YY - initials

#=======================================================================
# How to run
# Press Ctrl-A to select all, then run.  Shiny should start up
# Click "next" once to precede to screening. 
# When you get bored close it down and everything is already saved - then upload to git
# To start again, just select all and run again.
#=======================================================================

rm(list=ls())
# adjust this to the appropriate folder on your system
setwd("~/Documents/GitHub/systematic-review-flash-floods")
# if you're on a Windows PC try: 
# setwd("C:\\Documents\\GitHub\\systematic-review-flash-floods"")
# may need to adjust to your specific machine
# if you're on a MacOS:
#setwd("~/Documents/GitHub/systematic-review-flash-floods")
Workingfile <- "data/screeningData.RData"
load(Workingfile)

#=======================================================================
# Sort so screened data is at the bottom
#=======================================================================
data_bib$Screen1_Assessed[which(is.na(data_bib$Screen1_Assessed)==TRUE)] <- FALSE
data_bib$Reject[which(is.na(data_bib$Reject)==TRUE)] <- FALSE
data_bib$Screen2_Assessed[which(is.na(data_bib$Screen2_Assessed)==TRUE)] <- FALSE
data_bib <- data_bib[with(data_bib, order(Screen2_Assessed,Reject,Screen1_Assessed)), ]

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
  YourData %<>% str_replace_all(regex("landslide", ignore_case = TRUE), wordHighlightyellow)
  YourData %<>% str_replace_all(regex("landslides", ignore_case = TRUE), wordHighlightyellow)
  YourData %<>% str_replace_all(regex("mudslide", ignore_case = TRUE), wordHighlightyellow)
  YourData %<>% str_replace_all(regex("mudslides", ignore_case = TRUE), wordHighlightyellow)
  
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
  # Feel free to change the theme to serve your preferences, you'll be staring at this for a while
  # so might as well make it look good.
  theme = shinytheme("cosmo"),
  #--------------------------------------------------------------------
  # This creates a shiny sidebar.  There are other options
  sidebarLayout(
    sidebarPanel(
      
      #--------------------------------------------------------------------
      # Not Relevent, we made a mistake
      fluidRow(
        materialSwitch(inputId="discardButton", label="Not Relevant",value=FALSE,width="100%",status="danger")),
      
      #--------------------------------------------------------------------
      # Meta-analysis
      ## TO DO - format this better
      fluidRow(
        checkboxGroupInput("metaGroup", 
                           label = h5("Meta-analysis"), 
                           choices = list("Climate change" = 1, 
                                          "Long term impact" = 2, 
                                          "Land cover" = 3,
                                          "Policy" = 4),
                           selected = NULL),
        #--------------------------------------------------------------------
        # Assessment Type
        selectInput("assessmentSelect", 
                    label = h5("Assessment Type"), 
                    choices = list("Risk Assessment" = 1, 
                                   "Vulnerability Assessment" = 2, 
                                   "Risk Perception" = 3), 
                    selected = NULL),
        
        #--------------------------------------------------------------------
        # "Directly" before a flood
        checkboxGroupInput("beforeGroup", 
                           label = h5("'Directly' before a flood"), 
                           choices = list("Forecasting (predicting a flood)" = 1, 
                                          "Early Warning System (letting people know)" = 2, 
                                          "Anticipatory response" = 3),
                            selected = NULL),
        
        #--------------------------------------------------------------------
        # During the flood
        checkboxGroupInput("duringGroup", 
                           label = h5("'During' a flood"), 
                           choices = list("Flood detection (is the flood happening right now?)" = 1, 
                                          "Emergency management (what do the 'experts' do?)" = 2, 
                                          "Community actions (what did people do?)" = 3),
                           selected = NULL),
        #--------------------------------------------------------------------
        # Impact
        ## TO DO - ADD multi-select? Fatalities, Economic, Health, Psychological, Community, Infrastructure (e.g. water treatment plants, roads, etc), Other [NOTE?]
        checkboxGroupInput("impactGroup", 
                           label = h5("Impact"), 
                           choices = list("Fatalities" = 1, 
                                          "Economic" = 2, 
                                          "Health" = 3,
                                          "Psychological" = 4,
                                          "Community" = 5,
                                          "Infrastructure"= 6,
                                          "Other [leave a note]" = 7),
                           selected = NULL),
        
        #--------------------------------------------------------------------
        # Methods
        ## TO DO - ADD check box? dropdown? Remote sensing/Weather modelling, Machine learning, Mapping/GIS, Simulation/scenarios, Community guidance/tools information, Interviews, Social media/crowd sourcing
        
        #--------------------------------------------------------------------
        # Geography
        ## TO DO - ADD droppdown? Urban, Rural, Indigenous/"Global South"
        

        #--------------------------------------------------------------------
        # Flood Type
        selectInput("floodSelect", 
                    label = h5("Select Flood Type"), 
                    choices = list("Rainfall runoff" = 1, 
                                    "Cloudburst" = 2, 
                                    "Dam/levee breach" = 3,
                                    "Speedy river" = 4,
                                    "Landslide/Mudslide" = 5,
                                    "Snowmelt" = 6), 
                    selected = NULL)),
      
      #--------------------------------------------------------------------
      # Notes
      hr(),
      fluidRow(textInput(inputId = "notesField", label = "Notes", value = "")),
      
      #--------------------------------------------------------------------
      # The next button
      hr(),
      fluidRow(actionButton("nextButton", "Next"))
    ),
    
    #--------------------------------------------------------------------
    # Where the main data resides. 
    # The table itself and whether it has been pre-screened
    # Additional information for screening
    mainPanel(
      DT::dataTableOutput("table"),
      hr(),
      # More info on screening classifications
      verbatimTextOutput("moreInfo"),
      htmlOutput("count"),
      htmlOutput("total")
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
    {input$nextButton 
      },
    {
      updateMaterialSwitch(session=session, inputId="discardButton",value=FALSE)
      updateMaterialSwitch(session=session, inputId="rainButton",value=FALSE)
      updateMaterialSwitch(session=session, inputId="modelButton",value=FALSE)
      updateMaterialSwitch(session=session, inputId="socialButton",value=FALSE)
      updateMaterialSwitch(session=session, inputId="eventButton",value=FALSE)
      updateMaterialSwitch(session=session, inputId="databaseButton",value=FALSE)
      updateMaterialSwitch(session=session, inputId="floodTypeButton",value=FALSE)
      updateTextInput(session=session, inputId="notesField", value = "")
######## SWITCHED OFF      save(list="data_bib",file=Workingfile)

      
      #-----------------------------------------------------
      # If the row number is not at the end, increment up
      # THIS IS *REALLY BAD CODING*, ADDED IN BECAUSE IT WANTS TO RECALCULATE THE VALUE.
     # if(sum(c(input$discardButton,input$rainButton,input$modelButton,input$socialButton))>0){
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
           data_bib$Screen2_Notes   [values$count-1] <<- input$notesField
           data_bib$Screen2_FlashFloodDatabase[values$count-1] <<- input$databaseButton
           data_bib$Screen2_typeID  [values$count-1] <<- input$floodTypeButton
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
          data_bib$Screen2_Notes   [nrow(data_bib)] <<- input$notesField
          data_bib$Screen2_FlashFloodDatabase [nrow(data_bib)] <<- input$databaseButton
          data_bib$Screen2_typeID  [nrow(data_bib)] <<- input$floodTypeButton
          return(YourData2)
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
    YourText2 = as.character(data_bib$Reject[values$count])
    if(YourText2=="FALSE"){
      YourText3 <- "Reject: NO"
      YourText3 %<>% str_replace_all(regex("Reject: NO",ignore_case = TRUE), wordHighlightgrey)
    }else{
      YourText3 <- as.character(data_bib$Reject[values$count])#"Reject: YES"
      YourText3 %<>% str_replace_all(regex("Reject: YES",ignore_case = TRUE), wordHighlightred)
    }
    return(YourText3)
  }) 

  
  #--------------------------------------------------------------------
  # Output the table to the GUI

  output$table <- DT::renderDataTable({ data <- highlighter()}, escape = FALSE,options = list(dom = 't',bSort=FALSE)) 
  output$covidence <- renderUI({HTML(paste(highlightCovidence(), sep = '<br/>'))})
  output$reject <- renderUI({ HTML(paste(highlightExclude(), sep = '<br/>'))})
  
  #--------------------------------------------------------------------
  # More info about the screening selections
  hr()
  output$moreInfo <- renderText({
    paste("TK WILL FILL IN LATER",
          sep="\n")
  })
  hr()
  output$count <- renderUI({ HTML(paste("You have reviewed", (values$count - 2) ,"papers in this session")) })
  output$total <- renderUI({HTML(paste("In total, we have reviewed", (sum(data_bib$Screen2_Assessed, na.rm = TRUE)), "of", (length(data_bib$Screen2_Assessed))))})
}

#=======================================================================
# Run the server
#=======================================================================
shinyApp(ui = ui, server = server)

