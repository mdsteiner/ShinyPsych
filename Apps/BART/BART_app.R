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
source("create_bart_page.R")
source("create_bart_list.R")
source("create_task_ctrl_list.R")
source("include_script_files.R")
source("on_bart_action.R")
# Define surveys


# --------------------------
# Section A: Setup Survey
# --------------------------

# Section A2: DATA SAVING

outputDir <- "msteiner/ShinyPsych/Bart"  # Directory to save data

# Section A4: Miscellaneous code



# --------------------------------
# Section B: Define overall layout
# --------------------------------

ui <- fixedPage(
  
  title = "ShinyPsychBart",      # App title
  uiOutput("MainAction"),    
  useShinyjs(),# For Shinyjs functions
  includeScriptFiles(fileList = "bart")

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
id.vec <- c("Instructions", "Goodbye")



CurrentValues <- createCtrlList("instructions", globIds = id.vec,
                                complCode = TRUE, complName = "EP-Survey",
                                task = "bart")

instructions.list <- preparePageList(defaultName = "Instructions")

goodbye.list <- preparePageList(defaultName = "Goodbye")

GameData <- createTaskCtrlList(task = "bart")

bartDistList <- list("diffBalloons" = 2,
                     "distributionType" = rep("unif", 2),
                     "min" = c(1, 1),
                     "max" = c(10, 15),
                     "nBalloons" = c(5, 5),
                     "balloonColor" = c("blue", "grey"),
                     "max.pop" = c(10, 15))

bartContainer <- createBartList(distList = bartDistList, randomize = TRUE)



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
  
if (CurrentValues$page == "game") {
  
  return(
  createBartPage(ctrlVals = CurrentValues, session = session,
                 balloonList = bartContainer, RDrawBoundary = TRUE)
  )
}
  
if (CurrentValues$page == "gameend") {
  
  return(list(h3("You finished the game!"),
              p(paste("You earned", CurrentValues$points, "points in the game.")),
              p("Please click continue to complete a short survey"),
              actionButton(inputId = "gt_goodbye", 
                           label = "Continue")))
}
  
  
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
  nextPage(pageId = "instructions", ctrlVals = CurrentValues, nextPageId = "game",
          pageList = instructions.list, globId = "Instructions")
})


# After a pop, start next balloon
observeEvent({input[["nextballoon"]]}, {
  
  onBartAction(id = "nextballoon", ctrlVals = CurrentValues, input = input,
               bartCtrlList = GameData, balloonList = bartContainer)
  
})

# Look for final balloon -> Go to gameend
observeEvent({CurrentValues[["balloon"]]}, {
  
  onBartAction(id = "next_page", ctrlVals = CurrentValues, input = input,
               bartCtrlList = GameData, balloonList = bartContainer,
               nextPageId = "gameend")
  
})


# What to do if the balloon popped:
observeEvent(input[["popped"]], {
  
  onBartAction(id = "popped", ctrlVals = CurrentValues, input = input,
               bartCtrlList = GameData, balloonList = bartContainer)
}) 


# saveballoon button
observeEvent(input[["saveballoon"]], {
  
  onBartAction(id = "saveballoon", ctrlVals = CurrentValues, input = input,
               bartCtrlList = GameData, balloonList = bartContainer)
  
})

# Make sure answers are selected
observeEvent(reactiveValuesToList(input),{

  onInputEnable(pageId = "instructions", ctrlVals = CurrentValues,
                pageList = instructions.list, globId = "Instructions",
                inputList = input, charNum = 4)
  
})


# --------------------------------
# Section G: Save data
# --------------------------------
observeEvent(input[["gt_goodbye"]], {(
  
# Create progress message   
withProgress(message = "Saving data...", value = 0, {
  
  incProgress(.25)

  # Create a list to save data
  data.list <- list(  "workerid" = input$Instructions_workerid,
                      "balloon" = GameData$balloon,
                      "time" = GameData$time,
                      "action" = GameData$action,
                      "pop" = GameData$pop,
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