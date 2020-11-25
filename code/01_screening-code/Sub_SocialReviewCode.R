library(shiny)   ;  library(tidyverse)
library(DT)      ;  library(magrittr)
library(stringr) ;  library(shinyWidgets)
library(RISmed)  ;  library(bibliometrix)
library(bib2df)  ;  library(knitr)
library(shinythemes)
library(rmdformats)



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
Workingfile <- "~/Documents/GitHub/systematic-review-flash-floods/data/screeningSocialData.RData"
load(Workingfile)

#=======================================================================
# Sort so screened data is at the bottom
#=======================================================================
data_bib$Screen1_Assessed[which(is.na(data_bib$Screen1_Assessed)==TRUE)] <- FALSE   # extra check to make sure no NAs - everything is either true or false
data_bib$Reject[which(is.na(data_bib$Reject)==TRUE)] <- FALSE
data_bib$Screen2_Assessed[which(is.na(data_bib$Screen2_Assessed)==TRUE)] <- FALSE
data_bib$Screen3_Assessed[which(is.na(data_bib$Screen3_Assessed)==TRUE)] <- FALSE

data_bib <- data_bib[with(data_bib, order(Screen3_Assessed)), ]

## Do we want to change any of the highlighting rules? 
#=======================================================================
# Highlighting Rules
#=======================================================================
wordHighlightyellow <- function(SuspWord,colH = "#FFE9A8") {paste0('<span style="background-color:',colH,'">',SuspWord,'</span>')}
wordHighlightgreen <- function(SuspWord,colH = "#BEDDBA") {paste0('<span style="background-color:',colH,'">',SuspWord,'</span>')}
wordHighlightblue <- function(SuspWord,colH = "#A3C4D9") {paste0('<span style="background-color:',colH,'">',SuspWord,'</span>')}
wordHighlightred <- function(SuspWord,colH = "#CFA6B6") {paste0('<span style="background-color:',colH,'">',SuspWord,'</span>')}
wordHighlightdarkblue <- function(SuspWord,colH = "#BAB4D4") {paste0('<span style="background-color:',colH,'">',SuspWord,'</span>')}
wordHighlightgrey <- function(SuspWord,colH = grey(0.9)) {paste0('<span style="background-color:',colH,'">',SuspWord,'</span>')}
wordHighlightpurple <- function(SuspWord,colH = "#D4B9DA") {paste0('<span style="background-color:',colH,'">',SuspWord,'</span>')}



helenhighlight <- function(YourData){
  
  #Meta - GREY
  YourData %<>% str_replace_all(regex("policy", ignore_case = TRUE), wordHighlightgrey)
  YourData %<>% str_replace_all(regex("climate", ignore_case = TRUE), wordHighlightgrey)
  YourData %<>% str_replace_all(regex("climate change", ignore_case = TRUE), wordHighlightgrey)
  
  #Assessment Type - YELLOW
  YourData %<>% str_replace_all(regex("risk perception", ignore_case = TRUE), wordHighlightyellow)
  YourData %<>% str_replace_all(regex("risk assessment", ignore_case = TRUE), wordHighlightyellow)
  YourData %<>% str_replace_all(regex("perception", ignore_case = TRUE), wordHighlightyellow)
  YourData %<>% str_replace_all(regex("assessment", ignore_case = TRUE), wordHighlightyellow)
  YourData %<>% str_replace_all(regex("vulnerability", ignore_case = TRUE), wordHighlightyellow)
  
  #Impacts - GREEN
  YourData %<>% str_replace_all(regex("impacting", ignore_case = TRUE), wordHighlightgreen)
  YourData %<>% str_replace_all(regex("impacts", ignore_case = TRUE), wordHighlightgreen)
  YourData %<>% str_replace_all(regex("impact", ignore_case = TRUE), wordHighlightgreen)
  YourData %<>% str_replace_all(regex("killed", ignore_case = TRUE), wordHighlightgreen)
  YourData %<>% str_replace_all(regex("deaths", ignore_case = TRUE), wordHighlightgreen)
  YourData %<>% str_replace_all(regex("death", ignore_case = TRUE), wordHighlightgreen)
  YourData %<>% str_replace_all(regex("damage", ignore_case = TRUE), wordHighlightgreen)
  YourData %<>% str_replace_all(regex("disaster", ignore_case = TRUE), wordHighlightgreen)
  YourData %<>% str_replace_all(regex("economic", ignore_case = TRUE), wordHighlightgreen)
  YourData %<>% str_replace_all(regex("fatalities", ignore_case = TRUE), wordHighlightgreen)
  YourData %<>% str_replace_all(regex("casualties", ignore_case = TRUE), wordHighlightgreen)
  YourData %<>% str_replace_all(regex("community", ignore_case = TRUE), wordHighlightgreen)
  YourData %<>% str_replace_all(regex("infrastructure", ignore_case = TRUE), wordHighlightgreen)
  YourData %<>% str_replace_all(regex("psych", ignore_case = TRUE), wordHighlightgreen)
  YourData %<>% str_replace_all(regex("health", ignore_case = TRUE), wordHighlightgreen)
  
  #Method - BLUE
  YourData %<>% str_replace_all(regex("modelling", ignore_case = TRUE), wordHighlightblue)
  YourData %<>% str_replace_all(regex("modeling", ignore_case = TRUE), wordHighlightblue)
  YourData %<>% str_replace_all(regex("models", ignore_case = TRUE), wordHighlightblue)
  YourData %<>% str_replace_all(regex("model", ignore_case = TRUE), wordHighlightblue)
  YourData %<>% str_replace_all(regex("gis ", ignore_case = TRUE), wordHighlightblue)
  YourData %<>% str_replace_all(regex("remote sensing", ignore_case = TRUE), wordHighlightblue)
  YourData %<>% str_replace_all(regex("focus group", ignore_case = TRUE), wordHighlightblue)
  YourData %<>% str_replace_all(regex("simulation", ignore_case = TRUE), wordHighlightblue)
  YourData %<>% str_replace_all(regex("machine learning", ignore_case = TRUE), wordHighlightblue)
  YourData %<>% str_replace_all(regex("interview", ignore_case = TRUE), wordHighlightblue)
  YourData %<>% str_replace_all(regex("survey", ignore_case = TRUE), wordHighlightblue)
  YourData %<>% str_replace_all(regex("media", ignore_case = TRUE), wordHighlightblue)
  YourData %<>% str_replace_all(regex("twitter", ignore_case = TRUE), wordHighlightblue)
  YourData %<>% str_replace_all(regex("crowd", ignore_case = TRUE), wordHighlightblue)
  YourData %<>% str_replace_all(regex("facebook", ignore_case = TRUE), wordHighlightblue)
  YourData %<>% str_replace_all(regex("tweet", ignore_case = TRUE), wordHighlightblue)
  
  #Type of Flash Flood
  YourData %<>% str_replace_all(regex("landslide", ignore_case = TRUE), wordHighlightred)
  YourData %<>% str_replace_all(regex("landslides", ignore_case = TRUE), wordHighlightred)
  YourData %<>% str_replace_all(regex("mudslide", ignore_case = TRUE), wordHighlightred)
  YourData %<>% str_replace_all(regex("mudslides", ignore_case = TRUE), wordHighlightred)
  YourData %<>% str_replace_all(regex("river", ignore_case = TRUE), wordHighlightred)
  YourData %<>% str_replace_all(regex("dam ", ignore_case = TRUE), wordHighlightred)
  YourData %<>% str_replace_all(regex("snow", ignore_case = TRUE), wordHighlightred)
  YourData %<>% str_replace_all(regex("pluvial", ignore_case = TRUE), wordHighlightred)
  YourData %<>% str_replace_all(regex("cloudburst", ignore_case = TRUE), wordHighlightred)
  YourData %<>% str_replace_all(regex("cloud", ignore_case = TRUE), wordHighlightred)
  # YourData %<>% str_replace_all(regex("rainfall",ignore_case = TRUE), wordHighlightred)
  # YourData %<>% str_replace_all(regex(" rain",ignore_case = TRUE), wordHighlightred)
  
  
  #Timing 
  YourData %<>% str_replace_all(regex("forecast", ignore_case = TRUE), wordHighlightdarkblue)
  YourData %<>% str_replace_all(regex("ad hoc", ignore_case = TRUE), wordHighlightdarkblue)
  YourData %<>% str_replace_all(regex("post hoc", ignore_case = TRUE), wordHighlightdarkblue)
  YourData %<>% str_replace_all(regex("early", ignore_case = TRUE), wordHighlightdarkblue)
  YourData %<>% str_replace_all(regex("warning", ignore_case = TRUE), wordHighlightdarkblue)
  YourData %<>% str_replace_all(regex("EWS", ignore_case = TRUE), wordHighlightdarkblue)
  YourData %<>% str_replace_all(regex("response", ignore_case = TRUE), wordHighlightdarkblue)
  YourData %<>% str_replace_all(regex("detection", ignore_case = TRUE), wordHighlightdarkblue)
  YourData %<>% str_replace_all(regex("emergency", ignore_case = TRUE), wordHighlightdarkblue)
  YourData %<>% str_replace_all(regex("risk management", ignore_case = TRUE), wordHighlightdarkblue)
  YourData %<>% str_replace_all(regex("management", ignore_case = TRUE), wordHighlightdarkblue)
  YourData %<>% str_replace_all(regex("action", ignore_case = TRUE), wordHighlightdarkblue)
  

  #Geography
  YourData %<>% str_replace_all(regex("urban", ignore_case = TRUE), wordHighlightpurple)
  YourData %<>% str_replace_all(regex("rural", ignore_case = TRUE), wordHighlightpurple)
  YourData %<>% str_replace_all(regex("indigenous", ignore_case = TRUE), wordHighlightpurple)

  
}

#=======================================================================
# GUI function
#=======================================================================
ui <- fluidPage(
  tags$head(tags$style(
    HTML('
         #sidebar {
            background-color: #5c5c5f;
         }
         
         #table {
            background-color: #5c5c5f;
         }
        '))),
  #--------------------------------------------------------------------
  # Feel free to change the theme to serve your preferences, you'll be staring at this for a while
  # so might as well make it look good.
  #theme = shinytheme("flatly"),
  theme = 'bootstrap.css',
  #--------------------------------------------------------------------
  # This creates a shiny sidebar.  There are other options
  sidebarLayout(
    sidebarPanel(id = "sidebar",
      
      #--------------------------------------------------------------------
      # Not Relevent, we made a mistake
      fluidRow(
        materialSwitch(inputId="discardButton", label="Not Relevant",value=FALSE,width="100%",status="danger")),
      
      #--------------------------------------------------------------------
      # Meta-analysis
      ## TO DO - format this better
      fluidRow(
        checkboxGroupButtons(inputId = "metaGroup", 
                           label = h5("Meta-analysis"), 
                           choices = c("Climate change" = 1, 
                                       "Long term impact" = 2, 
                                       "Land cover" = 3,
                                       "Policy" = 4),
                           checkIcon = list(
                             yes = tags$i(class = "fa fa-circle", 
                                          style = "color: steelblue"),
                             no = tags$i(class = "fa fa-circle-o", 
                                         style = "color: steelblue")),
                           status = "info",
                           selected = NULL)),
        #--------------------------------------------------------------------
        # Assessment Type
      fluidRow(
        column(width = 6,
        selectInput("assessmentSelect", 
                    label = h5("Assessment Type"), 
                    choices = list("Risk Assessment" = 1, 
                                   "Vulnerability Assessment" = 2, 
                                   "Risk Perception" = 3), 
                    selected = NULL)),
        checkboxGroupButtons(inputId="genButton", 
                             label = h5("General"),
                             choices = list("Planning" = 1, 
                                            "Science" = 2),
                             status = "info",
                             selected = NULL)), 
        
        #--------------------------------------------------------------------
        # "Directly" before a flood
      fluidRow(
        checkboxGroupButtons("beforeGroup", 
                           label = h5("'Directly' before a flood"), 
                           choices = list("Forecasting" = 1, 
                                          "Early Warning System" = 2, 
                                          "Anticipatory response" = 3),
                           status = "info",
                           selected = NULL)),
        
        #--------------------------------------------------------------------
        # During the flood
      fluidRow(
        checkboxGroupButtons("duringGroup", 
                           label = h5("'During' a flood"), 
                           choices = list("Flood detection" = 1, 
                                          "Emergency management" = 2, 
                                          "Community actions" = 3),
                           status = "info",
                           selected = NULL),
        #--------------------------------------------------------------------
        # Impact
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
        checkboxGroupInput("methodsGroup", 
                           label = h5("Methods"), 
                           choices = list("Remote sensing & weather modelling" = 1, 
                                          "Machine learning" = 2, 
                                          "Mapping & GIS" = 3,
                                          "Simulations or scenarios" = 4,
                                          "Community guidance & tools" = 5,
                                          "Interviews/surveys"= 6,
                                          "Social media or crowd sourcing" = 7),
                           selected = NULL)),
        
        #--------------------------------------------------------------------
        # Geography
        fluidRow(
          column(width = 6,
          selectInput("geoSelect", 
                    label = h5("Geography"), 
                    choices = list("Urban" = 1, 
                                   "Rural" = 2, 
                                   "Indigenous/'Global South'" = 3), 
                    selected = NULL)),        

        #--------------------------------------------------------------------
        # Flood Type
        selectInput("floodSelect", 
                    label = h5("Select Flood Type"), 
                    choices = list("Not specified" = 1, 
                                    "Rainfall" = 2, 
                                    "Dam/levee breach" = 3,
                                    "Speedy river" = 4,
                                    "Landslide/Mudslide" = 5,
                                    "Snowmelt" = 6), 
                    selected = 1)),
      
      #--------------------------------------------------------------------
      # Notes
      hr(),
      fluidRow(textInput(inputId = "notesField", label = "Notes", value = "")),
      
      #--------------------------------------------------------------------
      # The next button
      hr(),
      fluidRow(actionButton("nextButton", "Next", width = "100px", 
                            style="color: #fff; background-color: #4bbf73; border-color: #2e6da4"))
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
    # not sure if the problems with the update button have to do with eventReactive, but most of my googling for the error
    # lead me to conversations about ignoreNull and 
    # some of these buttons will update from the database if there is a value there (others will not because i couldnt
    # figure out these update buttons)
    {input$nextButton}, #ignoreNULL = FALSE, ignoreInit = FALSE,
    {
      updateMaterialSwitch(session=session, inputId="discardButton",value=FALSE)
      updateCheckboxGroupButtons(session=session, inputId="genButton", selected = character(0))
      updateCheckboxGroupButtons(session=session, inputId="metaGroup", selected = character(0))
      updateSelectInput(session=session, inputId="assessmentSelect", 
                        selected = as.numeric(data_bib$Screen3_assessment)[values$count+1])
      updateCheckboxGroupButtons(session=session, inputId="duringGroup", selected = character(0))
      updateCheckboxGroupButtons(session=session, inputId="beforeGroup", selected = character(0))
      updateCheckboxGroupInput(session=session, inputId="impactGroup", selected = 0) 
      updateCheckboxGroupInput(session=session, inputId="methodsGroup", selected = 0)
      updateSelectInput(session=session, inputId="geoSelect", selected = as.numeric(data_bib$Screen3_geo)[values$count+1])
      updateSelectInput(session=session, inputId="floodSelect", selected = as.numeric(data_bib$Screen3_flood)[values$count+1])
      updateTextInput(session=session, inputId="notesField", value = "")
      
      save(list="data_bib",file=Workingfile)

      #-----------------------------------------------------
      # If the row number is not at the end, increment up
      # THIS IS *REALLY BAD CODING*, ADDED IN BECAUSE IT WANTS TO RECALCULATE THE VALUE.
      # if(sum(c(input$discardButton,input$rainButton,input$modelButton, input$socialButton))>0){
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
           data_bib$Screen3_Assessed    [values$count-1] <<- TRUE
           data_bib$Screen3_Reject      [values$count-1] <<- input$discardButton
           
           #data_bib$Screen3_Planning <- as.character(data_bib$Screen3_Planning)
           if(length(input$genButton) <= 0){
             data_bib$Screen3_Planning    [values$count-1] <<- 0
           }
           else {
             data_bib$Screen3_Planning[values$count-1] <<- str_c(input$genButton, collapse = '_')
             #data_bib$Screen3_Planning    [values$count-1] <<- input$genButton
           }
           
           if(length(input$metaGroup) <= 0){
              data_bib$Screen3_meta     [values$count-1] <<- 0
           } else{
              data_bib$Screen3_meta     [values$count-1] <<- str_c(input$metaGroup, collapse = '_')
           }
           
           if(length(input$assessmentSelect) <= 0){t
             data_bib$Screen3_assessment[values$count-1] <<- 0
           } else{
             data_bib$Screen3_assessment[values$count-1] <<- input$assessmentSelect
           }
           
           if(length(input$beforeGroup) <= 0){
             data_bib$Screen3_before    [values$count-1] <<- 0
           } else {
             data_bib$Screen3_before    [values$count-1] <<- str_c(input$beforeGroup, collapse = '_')
           }
           
           if(length(input$duringGroup) <= 0){
             data_bib$Screen3_during    [values$count-1] <<- 0
           } else {
             data_bib$Screen3_during    [values$count-1] <<- str_c(input$duringGroup, collapse = '_')
           }
           
           if(length(input$impactGroup) <= 0){
             data_bib$Screen3_impact    [values$count-1] <<- 0
           } else {
             data_bib$Screen3_impact    [values$count-1] <<- str_c(input$impactGroup, collapse = '_')
           }
        
           if(length(input$methodsGroup) <= 0){
             data_bib$Screen3_methods   [values$count-1] <<- 0
           } else {
             data_bib$Screen3_methods   [values$count-1] <<- str_c(input$methodsGroup, collapse = '_')
           }
           
           if(length(input$geoSelect) <= 0){
             data_bib$Screen3_geo       [values$count-1] <<- 0
           } else {
             data_bib$Screen3_geo       [values$count-1] <<- input$geoSelect
           }
           
           if(length(input$floodSelect) <= 0){
             data_bib$Screen3_flood     [values$count-1] <<- 0
           } else {
             data_bib$Screen3_flood     [values$count-1] <<- input$floodSelect
           }
           
           data_bib$Screen3_Notes       [values$count-1] <<- input$notesField
           return(YourData2)
        }
        #-----------------------------------------------------
        # Or put the final row
        else{
          YourData <- data_bib[ nrow(data_bib),c("TI","AB")]
          YourData2 <- helenhighlight(YourData)
          
          #-----------------------------------------------------
          # Output to data_bib         
          data_bib$Screen3_Assessed   [nrow(data_bib)] <<- TRUE
          data_bib$Screen3_Reject     [nrow(data_bib)] <<- input$discardButton
          data_bib$Screen3_Planning   [nrow(data_bib)] <<- str_c(input$genButton, collapse = '_')
          data_bib$Screen3_meta       [nrow(data_bib)] <<- str_c(input$metaGroup, collapse = '_')
          data_bib$Screen3_assessment [nrow(data_bib)] <<- input$assessmentSelect
          data_bib$Screen3_before     [nrow(data_bib)] <<- str_c(input$beforeGroup, collapse = '_')
          data_bib$Screen3_during     [nrow(data_bib)] <<- str_c(input$duringGroup, collapse = '_')
          data_bib$Screen3_impact     [nrow(data_bib)] <<- str_c(input$impactGroup, collapse = '_')
          data_bib$Screen3_methods    [nrow(data_bib)] <<- str_c(input$methodsGroup, collapse = '_')
          data_bib$Screen3_geo        [nrow(data_bib)] <<- input$geoSelect
          data_bib$Screen3_flood      [nrow(data_bib)] <<- input$floodSelect
          data_bib$Screen3_Notes      [nrow(data_bib)] <<- input$notesField
          return(YourData2)
        }
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
    paste("Assessment Definitions:
          Risk Assessment - including creating risk (or susceptibility) maps, risk analysis, and resilience assessments 
          Vulnerability  - Vulnerability assessments, creating vulnerability maps
          Risk Perception - e.g. interviews about individuals perspectives on flash flood risks, risk cognition.
          ", 
          "Flash Flood Type Definitions: 
          Not specified - flood type not definied or explicitly stated, unclear
          Rainfall - runoff, cloudburst, pluvial, caused by heavy precipitation (no river involved)
          Dam/levee breach - anything to do with dams or levees
          Speedy river - river height changes rapidly, fast onset riverine flood
          Landslide/mudslide - explicitly mentions landslide/mudslide or debris in water
          Snowmelt - caused by melting snow
          ",
          "General Planning - select if the paper is not explicitly related to a single event and is about preparing or planning for future events",
          "If the paper is about impacts in general, not related to a specific event, make sure 'general' is clicked before selecting the impacts",
          "General Science - select if paper is about science-based general planning - ie developing geophysical risk maps",
          sep="\n")
  })
  hr()
  output$count <- renderUI({ HTML(paste("You have reviewed", (values$count - 2) ,"papers in this session")) })
  output$total <- renderUI({HTML(paste("In total, we have reviewed", (sum(data_bib$Screen3_Assessed, na.rm = TRUE)), "of", (length(data_bib$Screen3_Assessed))))})
}

#=======================================================================
# Run the server
#=======================================================================
shinyApp(ui = ui, server = server)

