#=======================================================================
# HLG Nov 2019
# OK, so far, this code pulls together the covidence data and WOS output
# Then adds in a column to show whether it was screened or rejected by covidence
# Then it opens an R-shiny server.  It will print out one page for each 
# row of the dataframe, highlighting relevant terms  (dark blue for rainfall stuff,
# medium blue for hydrological models, green for social, yellow for "flood", red for "events)
# You can use the slider to move between rows
#
# TO DO
# Before the shiny server starts, order the dataframe by "has it been screened or not"
# Replace the slider with a "next button"
# Set up the reactive part so that:
#     1. There are buttons that do "Reject" and the other three options
#        That update on the screen within shiny
#        Then when you click next, update the data.frame itself
#        I'm currently stuck on this, but fingers crossed someone answers my help
#        question: https://community.rstudio.com/t/iteratively-save-shiny-changes-into-input-dataframe/45870
#=======================================================================

library(shiny)
library(tidyverse)
library(DT)
library(magrittr)
library(shinyWidgets)
library(bibliometrix)
library(stringr)
library(RISmed)
library(bib2df)
library(knitr)
library(rmdformats)

#-----------------------------------------------------------------------------------
# Initial stuff cause I am lazy
#-----------------------------------------------------------------------------------
setwd("~/Documents/GitHub/systematic-review-flash-floods")
#biblioshiny() # worth exploring

#-----------------------------------------------------------------------------------
# Using "bibliometrix" to convert the .bib files from Web of Science into a dataframe. 
# dbsource = "isi" indicates from which database the collection has been downloaded (in this case Web of Science)
#-----------------------------------------------------------------------------------

datafile_bib <- readFiles("Web_Of_Science_Downloads/savedrecs_1-500.bib",
                          "Web_Of_Science_Downloads/savedrecs_501-1000.bib",
                          "Web_Of_Science_Downloads/savedrecs_1001-1500.bib",
                          "Web_Of_Science_Downloads/savedrecs_1501_2000.bib",
                          "Web_Of_Science_Downloads/savedrecs_2001-2500.bib",
                          "Web_Of_Science_Downloads/savedrecs_2501-3000.bib",
                          "Web_Of_Science_Downloads/savedrecs_3001-3190.bib")

data_bib    <- convert2df(datafile_bib, dbsource = "isi", format = "bibtex")  
data_bib$AB <- str_to_sentence(data_bib$AB)
data_bib$AU <- str_to_title(data_bib$AU)
data_bib$TI <- str_to_sentence(data_bib$TI)


#-----------------------------------------------------------------------------------
# First read in the ones that we initially read into covidence and rejected
#-----------------------------------------------------------------------------------
Screenedfiles_irrelevant <- readFiles("~/Documents/GitHub/systematic-review-flash-floods/Covidence/Screened-Papers-No/review_58723_irrelevant_mendeley_20191023234131.ris")

Screenedfiles_irrelevant_TI <- Screenedfiles_irrelevant[grep("T1  -", Screenedfiles_irrelevant)]
Screenedfiles_irrelevant_TI <- substr(Screenedfiles_irrelevant_TI,7,nchar(Screenedfiles_irrelevant_TI))
FullMatches <- which(is.na(match(toupper(data_bib$TI),toupper(Screenedfiles_irrelevant_TI)))==FALSE)

data_bib$Screen1_Assessed <- NA
data_bib$Screen1_Assessed[FullMatches]  <- TRUE
data_bib$Screen1_Accept <- NA
data_bib$Screen1_Accept[FullMatches]  <- FALSE
data_bib$Reject <- NA
data_bib$Reject[FullMatches]  <- TRUE

#-----------------------------------------------------------------------------------
# First read in the ones that we initially read into covidence and accepted
#-----------------------------------------------------------------------------------
Screenedfiles_accept <- readFiles("/Users/hgreatrex/Documents/GitHub/systematic-review-flash-floods/Covidence/Screened-Papers-Yes-Maybe/review_58723_select_mendeley_20191003141113.ris")

Screenedfiles_accept_TI <- Screenedfiles_accept[grep("T1  -", Screenedfiles_accept)]
Screenedfiles_accept_TI <- substr(Screenedfiles_accept_TI,7,nchar(Screenedfiles_accept_TI))
FullMatchesAc <- which(is.na(match(toupper(data_bib$TI),toupper(Screenedfiles_accept_TI)))==FALSE)

data_bib$Screen1_Assessed[FullMatchesAc]  <- TRUE
data_bib$Screen1_Accept[FullMatchesAc]  <- TRUE


#-----------------------------------------------------------------------------------
# Now set up the screen 2 columns
#-----------------------------------------------------------------------------------
data_bib$Screen2_Assessed <- NA
data_bib$Screen2_Reject <- NA

test <- data_bib[1:10,]

data_bib$AB <- as.character(data_bib$AB)
data_bib$Shiny_Screen <- NA




#=======================================================================
# Highlighting Rules
#=======================================================================
wordHighlightyellow <- function(SuspWord,colH = "#FFE9A8") {paste0('<span style="background-color:',colH,'">',SuspWord,'</span>')}
wordHighlightgreen <- function(SuspWord,colH = "#BEDDBA") {paste0('<span style="background-color:',colH,'">',SuspWord,'</span>')}
wordHighlightblue <- function(SuspWord,colH = "#A3C4D9") {paste0('<span style="background-color:',colH,'">',SuspWord,'</span>')}
wordHighlightred <- function(SuspWord,colH = "#CFA6B6") {paste0('<span style="background-color:',colH,'">',SuspWord,'</span>')}
wordHighlightdarkblue <- function(SuspWord,colH = "#BAB4D4") {paste0('<span style="background-color:',colH,'">',SuspWord,'</span>')}
wordHighlightgrey <- function(SuspWord,colH = grey(0.9)) {paste0('<span style="background-color:',colH,'">',SuspWord,'</span>')}
  
#=======================================================================
# Set up outputs
#=======================================================================
  ui <- fluidPage(
    
    hr(),
    
   # title("test"),
   

    # Plot the row of the table
    DT::dataTableOutput("table"),
    
    hr(),
    
    # Now add whether you have screened it or not
    fluidRow(
      column(3,offset=1,
             htmlOutput("covidence")
      ),
      column(3,offset=1,
             htmlOutput("reject")
      ),
      column(3,offset=1,
             htmlOutput("screen2")
      )         
    ),
    
    hr(),
    
    
    fluidRow(
      checkboxGroupButtons(
      inputId = "somevalue", label = "Make a choice :", 
      choices = c("Choice A", "Choice B"),  justified = TRUE, status = "primary",individual=TRUE,width="400px",
      checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
    )),
    
   fluidRow(
     column(3,
            h4("Diamonds Explorer"),
            sliderInput(inputId = "row",
                        label = "Row Number:",
                        min = 1,
                        max = 1000,
                        value = 16),
            br(),
            checkboxInput('jitter', 'Jitter'),
            checkboxInput('smooth', 'Smooth')
     )
   )

  )
  
  
  
 #=======================================================================
 # This part does the highlighting for a given row of the table
 #=======================================================================
  server <- function(input, output) {

    #--------------------------------------------------------------------
    # Highlight the table
    #--------------------------------------------------------------------
    YourData <- data_bib[,c("TI","AB")] #title and abstract
    highlightData <- reactive({
        YourData2 = YourData[input$row,]
        YourData2 %<>% str_replace_all(regex("flash ", ignore_case = TRUE), wordHighlightyellow)
        YourData2 %<>% str_replace_all(regex("flash-", ignore_case = TRUE), wordHighlightyellow)
        YourData2 %<>% str_replace_all(regex("flash", ignore_case = TRUE), wordHighlightyellow)
        YourData2 %<>% str_replace_all(regex("floods", ignore_case = TRUE), wordHighlightyellow)
        YourData2 %<>% str_replace_all(regex("flooding", ignore_case = TRUE), wordHighlightyellow)
        YourData2 %<>% str_replace_all(regex("flood ", ignore_case = TRUE), wordHighlightyellow)
        YourData2 %<>% str_replace_all(regex("flood", ignore_case = TRUE), wordHighlightyellow)
        YourData2 %<>% str_replace_all(regex("flood-", ignore_case = TRUE), wordHighlightyellow)
        YourData2 %<>% str_replace_all(regex("risk", ignore_case = TRUE), wordHighlightyellow)
        
        YourData2 %<>% str_replace_all(regex("exposure", ignore_case = TRUE), wordHighlightgreen)
        YourData2 %<>% str_replace_all(regex("vulnerability", ignore_case = TRUE), wordHighlightgreen)
        YourData2 %<>% str_replace_all(regex("impacting", ignore_case = TRUE), wordHighlightgreen)
        YourData2 %<>% str_replace_all(regex("impacts", ignore_case = TRUE), wordHighlightgreen)
        YourData2 %<>% str_replace_all(regex("impact", ignore_case = TRUE), wordHighlightgreen)
        YourData2 %<>% str_replace_all(regex("focus group", ignore_case = TRUE), wordHighlightgreen)
        YourData2 %<>% str_replace_all(regex("stakeholder", ignore_case = TRUE), wordHighlightgreen)
        YourData2 %<>% str_replace_all(regex("culture", ignore_case = TRUE), wordHighlightgreen)
        YourData2 %<>% str_replace_all(regex("questionnaire", ignore_case = TRUE), wordHighlightgreen)
        YourData2 %<>% str_replace_all(regex("killed", ignore_case = TRUE), wordHighlightgreen)
        YourData2 %<>% str_replace_all(regex("deaths", ignore_case = TRUE), wordHighlightgreen)
        YourData2 %<>% str_replace_all(regex("death", ignore_case = TRUE), wordHighlightgreen)
        YourData2 %<>% str_replace_all(regex("damage", ignore_case = TRUE), wordHighlightgreen)
        YourData2 %<>% str_replace_all(regex("disaster", ignore_case = TRUE), wordHighlightgreen)
        YourData2 %<>% str_replace_all(regex("economic", ignore_case = TRUE), wordHighlightgreen)
        YourData2 %<>% str_replace_all(regex("social ", ignore_case = TRUE), wordHighlightgreen)
        YourData2 %<>% str_replace_all(regex("fatalities", ignore_case = TRUE), wordHighlightgreen)
        
        YourData2 %<>% str_replace_all(regex("modelling", ignore_case = TRUE), wordHighlightblue)
        YourData2 %<>% str_replace_all(regex("modeling", ignore_case = TRUE), wordHighlightblue)
        YourData2 %<>% str_replace_all(regex("models", ignore_case = TRUE), wordHighlightblue)
        YourData2 %<>% str_replace_all(regex("model", ignore_case = TRUE), wordHighlightblue)
        YourData2 %<>% str_replace_all(regex("dynamical ", ignore_case = TRUE), wordHighlightblue)
        YourData2 %<>% str_replace_all(regex("dynamics ", ignore_case = TRUE), wordHighlightblue)
        YourData2 %<>% str_replace_all(regex("hydro ", ignore_case = TRUE), wordHighlightblue)
        YourData2 %<>% str_replace_all(regex("dynamic ", ignore_case = TRUE), wordHighlightblue)
        YourData2 %<>% str_replace_all(regex("gis ", ignore_case = TRUE), wordHighlightblue)
        YourData2 %<>% str_replace_all(regex("remote sensing", ignore_case = TRUE), wordHighlightblue)
        YourData2 %<>% str_replace_all(regex("hydrological ", ignore_case = TRUE), wordHighlightblue)
        YourData2 %<>% str_replace_all(regex("hydrometeorologial ", ignore_case = TRUE), wordHighlightblue)
        YourData2 %<>% str_replace_all(regex("hydro", ignore_case = TRUE), wordHighlightblue)
        YourData2 %<>% str_replace_all(regex("discharge", ignore_case = TRUE), wordHighlightblue)
        
        YourData2 %<>% str_replace_all(regex("convective ", ignore_case = TRUE), wordHighlightdarkblue)
        YourData2 %<>% str_replace_all(regex("convection ", ignore_case = TRUE), wordHighlightdarkblue)
        YourData2 %<>% str_replace_all(regex("forecast ", ignore_case = TRUE), wordHighlightdarkblue)
        YourData2 %<>% str_replace_all(regex("weather",ignore_case = TRUE), wordHighlightdarkblue)
        YourData2 %<>% str_replace_all(regex("precipitation",ignore_case = TRUE), wordHighlightdarkblue)
        YourData2 %<>% str_replace_all(regex("rainfall",ignore_case = TRUE), wordHighlightdarkblue)
        YourData2 %<>% str_replace_all(regex(" rain",ignore_case = TRUE), wordHighlightdarkblue)
        YourData2 %<>% str_replace_all(regex("radar ",ignore_case = TRUE), wordHighlightdarkblue)
        
        YourData2 %<>% str_replace_all(regex(" compilation",ignore_case = TRUE), wordHighlightred)
        YourData2 %<>% str_replace_all(regex("atlas",ignore_case = TRUE), wordHighlightred)
        for(n in 1:12){
                YourData2 %<>% str_replace_all(regex(month.name[n],ignore_case = TRUE), wordHighlightred)
        }
        
       # YourData2 <- datatable(YourData2, rownames = NULL, colnames = NULL, options = list(dom = 'b', ordering = F)) 
        colnames(YourData2) <- paste0('<span style="color:',c("white"),'">',colnames(YourData2),'</span>')
        return(YourData2)
     })
    
    #--------------------------------------------------------------------
    # Highlight whether it has been screened or not in covidence
    #--------------------------------------------------------------------
    highlightCovidence <- reactive({
      YourText2 = as.character(data_bib$Screen1_Assessed[input$row])
       if(is.na(YourText2)){
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
    #--------------------------------------------------------------------
    highlightExclude <- reactive({
      YourText2 = as.character(data_bib$Reject[input$row])
      if(is.na(YourText2)){
        YourText3 <- "Reject: NO"
        YourText3 %<>% str_replace_all(regex("Reject: NO",ignore_case = TRUE), wordHighlightgrey)
      }else{
        YourText3 <- "Reject: YES"
        YourText3 %<>% str_replace_all(regex("Reject: YES",ignore_case = TRUE), wordHighlightred)
      }
      return(YourText3)
    }) 

    #--------------------------------------------------------------------
    # Highlight whether it has been screened in shiny
    #--------------------------------------------------------------------
    highlightScreen2 <- reactive({
      YourText2 = as.character(data_bib$Shiny_Screen[input$row])
      if(is.na(YourText2)){
        YourText3 <- "Shiny-Screen: NO"
        YourText3 %<>% str_replace_all(regex("Shiny-Screen: NO",ignore_case = TRUE), wordHighlightgrey)
      }else{
        YourText3 <- "Shiny-Screen: YES"
        YourText3 %<>% str_replace_all(regex("Shiny Screen: YES",ignore_case = TRUE), wordHighlightgreen)
      }
      return(YourText3)
    }) 
           
    #--------------------------------------------------------------------
    # Then render them for plotting in the shiny interface
    #--------------------------------------------------------------------
    
    output$table <- DT::renderDataTable({
      data <- highlightData() 
    }, escape = FALSE,options = list(dom = 't',bSort=FALSE)) #sDom  = '<"top">lrt<"bottom">ip','bPaginate:false'))
    
    output$covidence <- renderUI({
      HTML(paste(highlightCovidence(), sep = '<br/>'))
    })
    
    output$reject <- renderUI({
      HTML(paste(highlightExclude(), sep = '<br/>'))
    })
    
    output$screen2 <- renderUI({
      HTML(paste(highlightScreen2(), sep = '<br/>'))
    })
  }

  shinyApp(ui = ui, server = server)
  
  
  
  
  
  