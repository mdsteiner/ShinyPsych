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
source("next_page.R")
source("on_input_enable.R")
source("create_ctrl_list.R")
source("change_page_variable.R")
source("get_values.R")
# Define surveys


# --------------------------
# Section A: Setup Survey
# --------------------------

# Section A2: DATA SAVING

outputDir <- "nphillips/ShinySurvey/data"  # Directory to save data

# Section A4: Miscellaneous code



# --------------------------------
# Section B: Define overall layout
# --------------------------------

ui <- fixedPage(
  
  title = "ShinySurvey",      # App title
  uiOutput("MainAction"),    
  useShinyjs(),# For Shinyjs functions
  includeCSS("style.css")

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
id.vec <- c("Instructions", "Example", "Demographics", "Goodbye")
CurrentValues <- createCtrlList("instructions", globIds = id.vec,
                                complCode = TRUE, complName = "EP-Survey")

example.survey <- preparePageList(defaultName = "Example", randomize = TRUE)

instructions.list <- preparePageList(defaultName = "Instructions")

demographics.list <- preparePageList(defaultName = "Demographics")

goodbye.list <- preparePageList(defaultName = "Goodbye")

# --------------------------------
# Section D: Page Layouts
# --------------------------------

PageLayouts <- reactive({

  goodbye.list <- changePageVariable(pageList = goodbye.list, variable = "text",
                                     oldLabel = "completion.code",
                                     newLabel = CurrentValues$completion.code)
  
if (CurrentValues$page == "instructions") {
  
  return(
    createPage(pageList = instructions.list,
               pageNumber = CurrentValues$Instructions.num,
               globId = "Instructions", ctrlVals = CurrentValues)
  )}
  
# P3) REI
if (CurrentValues$page == "example") {
  
  return(
    createPage(pageList = example.survey, pageNumber = CurrentValues$Example.num,
               globId = "Example", ctrlVals = CurrentValues)
  )}
  
  
  
  
# P4) demographics
if (CurrentValues$page == "demographics") {
  
  return(
    createPage(pageList = demographics.list, pageNumber = CurrentValues$Demographics.num,
               globId = "Demographics", ctrlVals = CurrentValues)
  )}  
  
# P5) Goodbye
if (CurrentValues$page == "goodbye") {
  
  return(
    createPage(pageList = goodbye.list, pageNumber = CurrentValues$Goodbye.num,
               globId = "Goodbye", ctrlVals = CurrentValues, continueButton = FALSE)
  )}
  
})
  
  
# --------------------------------
# Section F: Event (e.g.; button) actions
# --------------------------------

# Section F1: Page Navigation Buttons


observeEvent(input[["Instructions_next"]],{
  nextPage(pageId = "instructions", ctrlVals = CurrentValues, nextPageId = "example",
          pageList = instructions.list, globId = "Instructions")
})

observeEvent(input[["Example_next"]], {
  nextPage(pageId = "example", ctrlVals = CurrentValues, nextPageId = "demographics",
          pageList = example.survey, globId = "Example")
})


# Make sure answers are selected
observeEvent(reactiveValuesToList(input),{     
  
  onInputEnable(pageId = "example", ctrlVals = CurrentValues,
                pageList = example.survey, globId = "Example",
                inputList = input)
  
  onInputEnable(pageId = "instructions", ctrlVals = CurrentValues,
                pageList = instructions.list, globId = "Instructions",
                inputList = input, charNum = 4)
  
  onInputEnable(pageId = "demographics", ctrlVals = CurrentValues,
                pageList = demographics.list, globId = "Demographics",
                inputList = input)
  
})


# --------------------------------
# Section G: Save data
# --------------------------------
observeEvent(input[["Demographics_next"]], {(
  
# Create progress message   
withProgress(message = "Saving data...", value = 0, {
  
  incProgress(.25)
  # Calculate REI score
  example.values <- getValues(pageList = example.survey, nmrc = TRUE,
                              inputList = input)

  # Calculate final mean
  example.score <- mean(example.values)
  
  # Create a list to save data
  data.list <- list(  "workerid" = input$Instructions_workerid,
                      "example" = paste(example.values, collapse = ","),
                      "example.score" = example.score,
                      "sex" = input$sex,
                      "age" = input$age,
                      "completion.code" = CurrentValues$completion.code)

  # save Data
  saveData(data.list, location = "dropbox", outputDir = outputDir,
           partId = data.list$workerid)

  CurrentValues$page <- "goodbye"
    
  })
  
)})
  
}

# Create app!
shinyApp(ui = ui, server = server)