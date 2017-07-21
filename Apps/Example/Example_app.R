# ------------------
#  ShinySurvey
#  Implimented in Shiny as ShinySurvey by Nathaniel D. Phillips
# 
#  http://ndphillips.github.io, Nathaniel.D.Phillips.is@gmail.com
#
#   CODE SECTIONS
#
#   0: Load libraries and R code
#   A: Setup Game
#     A1: Game parameters
#     A2: Data saving
#     A3: Reformat data
#     A4: Miscellaneous code
#   B: Overall layout
#   C: Reactive values
#   D: Page layouts
#   E: Event (button) actions
#     E1: Page navigation buttons
#     E2: Event tracking
#   F: Save data
# ------------------


# --------------------------
# Section 0: Load libraries
# --------------------------
library(shiny)
library(rdrop2)
library(digest)
library(shinyjs)
library(yarrr)

source("save_data.R")
source("prepare_page_list.R")
source("helper.R")
source("check_ID.R")
source("create_page.R")
# Define surveys


# --------------------------
# Section A: Setup Survey
# --------------------------

# Section A2: DATA SAVING
saveDataLocation <- "dropbox"           # Either dropbox, email, or local
outputDir <- "nphillips/ShinySurvey/data"  # Directory to save data

if(saveDataLocation == "dropbox") {droptoken <- readRDS("droptoken.rds")}        # Reads in authentication for dropbox

# Section A4: Miscellaneous code

# Calculate study completion code
completion.code <- paste("EP-SURVEY", sample(100:999, size = 1), 
                         sample(100:999, size = 1),
                         sample(100:999, size = 1), sep = "-")

example.survey <- preparePageList(defaultName = "Example")

ind <- substr(example.survey$id, start = nchar(example.survey$id) - 1,
              stop = nchar(example.survey$id)) != "NA"
obsExampleIds <- example.survey$id[ind]

instructions.list <- preparePageList(defaultName = "Instructions")

# --------------------------------
# Section B: Define overall layout
# --------------------------------

ui <- fixedPage(
  
  title = "ShinySurvey",      # App title
  uiOutput("MainAction"),    
  useShinyjs()# For Shinyjs functions

)

server <- function(input, output, session) {
  
  output$MainAction <- renderUI( {
    PageLayouts()
  
})

# --------------------------------
# Section C: Define Reactive Values
#   These store the main values in the study
# --------------------------------

# CurrentValues stores scalers representing the latest game outcomes
CurrentValues <- reactiveValues(page = "instructions",     # Current page
                                example.num = 1,     # Current question number (for surveys)
                                instructions.num = 1,
                                proceed = 0,
                                selected = 0)

# --------------------------------
# Section D: Page Layouts
# --------------------------------

PageLayouts <- reactive({

if (CurrentValues$page == "instructions") {
  
  return(
    createPage(pageList = instructions.list,
               pageNumber = CurrentValues$instructions.num,
               globId = "Instructions", contrlVals = CurrentValues)
  )}
  
# P3) REI
if (CurrentValues$page == "example") {
  
  return(
    createPage(pageList = example.survey, pageNumber = CurrentValues$example.num,
               globId = "Example", contrlVals = CurrentValues)
  )}
  
# P4) demographics
if (CurrentValues$page == "demographics") {
  
  return(
    list(
      h3("Please answer the following demographic questions"),
      numericInput(inputId = "age", value = NULL, label = "What is your Age?", min = 18, max = 100, step = 1),
      radioButtons(inputId = "sex", choices = list("Male", "Female", "Other"), label = "What is your Sex?", selected = NA),
      actionButton(inputId = "gt_goodbye", label = "Continue")
      
    )
  )}  
  
# P5) Goodbye
if (CurrentValues$page == "goodbye") {
  
  return(list(
    h3("Thank you for your participation!"),
    p("Here is your study completion code. Please write it down as a confirmation of your participation"),
    h2(completion.code),
    h3("What was this research about?"),
    p("The purpose of this research is to try and understand how people make decisions"),
    p("If you have any questions about this study, you may contact us at EconPsychBasel@gmail.com. If you do contact us, please include your study completion code."),
    p("Once you have recorded your code, you may close this window"),
    tags$img(src = "thankyou.jpg", width = "300px")
  ))
}
  
})
  
  
# --------------------------------
# Section F: Event (e.g.; button) actions
# --------------------------------

# Section F1: Page Navigation Buttons
observeEvent(input$gt_instructions, {CurrentValues$page <- "instructions"})
observeEvent(input$gt_example, {CurrentValues$page <- "example"})

observeEvent(input[["Instructions_next"]],{
  if(CurrentValues$page == "instructions"){
    CurrentValues$instructions.num <- CurrentValues$instructions.num + CurrentValues$proceed
    CurrentValues$proceed <- 0
    
    if(CurrentValues$instructions.num > max(instructions.list$page,
                                       na.rm = TRUE)) {
      
      CurrentValues$page <- "example"
      
    }
  }
})

observeEvent(input[["Example_next"]], {
  
  # Recode rei responses
  if(CurrentValues$page == "example") {
    
    CurrentValues$example.num <- CurrentValues$example.num + CurrentValues$proceed
    CurrentValues$proceed <- 0
    
    if(CurrentValues$example.num > max(example.survey$page,
                                       na.rm = TRUE)) {
      
      CurrentValues$page <- "demographics"
      
    }
    
  }
  
})

# Make sure answers are selected in rei survey
observeEvent(c(input[[obsExampleIds[1]]],
               input[[obsExampleIds[2]]],
               input[[obsExampleIds[3]]],
               input[[obsExampleIds[4]]],
               input[[obsExampleIds[5]]],
               input[[obsExampleIds[6]]],
               input[[obsExampleIds[7]]],
               input[[obsExampleIds[8]]],
               input[[obsExampleIds[9]]],
               input[[obsExampleIds[10]]]),{
  if(CurrentValues$page == "example") {
     checkInput <- example.survey$id[example.survey$page == CurrentValues$example.num &
                                       example.survey$id %in% obsExampleIds]
    if(mean(unlist(lapply(checkInput, function(x, inp) {
      !is.null(inp[[x]])}, input)), na.rm = TRUE) == 1) {enable("Example_next")}
  }
})

observeEvent(input[["Instructions_workerid"]], {
  if(CurrentValues$page == "instructions"){
    if (nchar(input[["Instructions_workerid"]]) >= 4){
      enable("Instructions_next")
    }
  }
})

observeEvent(input[["Instructions_checkConsent"]], {
  if(CurrentValues$page == "instructions"){
    if (!is.null(input[["Instructions_checkConsent"]]) &&
        isTRUE(input[["Instructions_checkConsent"]])){
      enable("Instructions_next")
    }
  }
})
  

# --------------------------------
# Section G: Save data
# --------------------------------
observeEvent(input$gt_goodbye, {(
  
# Create progress message   
withProgress(message = "Saving data...", value = 0, {
  
  incProgress(.25)

  # Calculate REI score
  example.values <- unlist(lapply(obsExampleIds, function(x, inp) {
    as.numeric(inp[[x]])}
    , inp = input))
  
  quReVals <- example.survey$reverse[example.survey$id %in% obsExampleIds]
  maxVals <- unlist(unname(lapply(example.survey$choices[example.survey$id %in% obsExampleIds],
                                  max, na.rm = TRUE)))

    
  # Reverse coding
  example.values[quReVals == 1] <- maxVals[quReVals == 1] - example.values[quReVals == 1]
  
  
  # Calculate final mean
  example.score <- mean(example.values)
  
  # Create a list to save data
  data.list <- list(c("workerid" = input$Instructions_workerid,
                      "example" = paste(example.values, collapse = ","),
                      "example.score" = example.score,
                      "sex" = input$sex,
                      "age" = input$age,
                      "completion.code" = completion.code))
  # save Data
  saveData(data.list, location = saveDataLocation, outputDir = outputDir,
           droptoken = droptoken, partId = data.list$workerid)
    
  CurrentValues$page <- "goodbye"
    
  })
  
)})
  
}

# Create app!
shinyApp(ui = ui, server = server)