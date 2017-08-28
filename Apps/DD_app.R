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
source("create_task_ctrl_list.R")
source("include_script_files.R")
source("append_dd_values.R")
source("create_dd_list.R")
source("dd_page.R")
source("three_opt_dfd_page.R")
source("four_opt_dfd_page.R")
source("dfe_custom_message.R")
source("multi_opts_dfd_page.R")
source("create_not_allowed_page.R")

# Define surveys


# --------------------------
# Section A: Setup Survey
# --------------------------

# Section A2: DATA SAVING

outputDir <- "msteiner/ShinyPsych/DD"  # Directory to save data

# Section A4: Miscellaneous code



# --------------------------------
# Section B: Define overall layout
# --------------------------------

ui <- fixedPage(
  
  title = "ShinySurvey",      # App title
  uiOutput("MainAction"),    
  useShinyjs(),# For Shinyjs functions
  includeScriptFiles(fileList = "dd")
  
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
                                  task = "dd")
  
  instructions.list <- preparePageList(defaultName = "Instructions")
  
  goodbye.list <- preparePageList(defaultName = "Goodbye")
  
  GameData <- createTaskCtrlList(task = "dd")
  
  ddContainer <- createDdList(defaultList = TRUE, fileName = "DDExample",
                                withPracticeTrial = TRUE, outcomeCurrency = "$")
  
  
  
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
        ddPage(ctrlVals = CurrentValues, container = ddContainer,
               nTrials = ddContainer$nTrials - 1)
  )}
  
  
if (CurrentValues$page == "postPractice"){
  return(
    div(class = "gameInfo", checked = NA,
        list(
          tags$br(), tags$br(),
          h2("Finished with Practice Game", class = "firstRow"),
          p(paste("You finished the practice game. In the real games afterward each time you make a decision, a value is chosen from that box and stored.")),
          p("On the next pages, you'll start playing the first real game that may count towards your bonus!"),
            tags$li("The point values in the boxes", strong("do not change over time."), " Each time you choose and option, the point value you see is always returned to the box.")
          ),
          p(strong("On the next page the first real game will start. Click to continue when you are ready.")),
          tags$br(),
          actionButton(inputId = "gt_game", 
                       label = "Start Gamble 1", class = "continueButtons") 
        )
    )
}
  
  # 4) END OF GAME PAGE
if (CurrentValues$page == "endGame") {
  return(
    div(class = "gameInfo", checked = NA,
        list(
          tags$br(), tags$br(),
          p(paste("You ended Gamble", CurrentValues$dfeGamble - 2)),
          p("A value has been drawn from your chosen distribution and was stored."),
          tags$br(),
          actionButton(inputId = "gt_games", 
                       label = paste0("Start Gamble ", CurrentValues$dfeGamble - 1), class = "continueButtons"))))
}
  
if (CurrentValues$page == "lastEndGame") {
  
  return(
    div(class = "gameInfo", checked = NA,
        list(
          tags$br(), tags$br(),
          h3("You finished all gambles!", class = "firstRow"),
          p("Click the \"Continue\" button to get your completion code."),
          tags$br(),
          actionButton(inputId = "gt_goodbye", 
                                label = "Continue", class = "continueButtons"))))
}
  
  
# P5) Goodbye
if (CurrentValues$page == "goodbye") {
  
  return(
    createPage(pageList = goodbye.list, pageNumber = CurrentValues$Goodbye.num,
               globId = "Goodbye", ctrlVals = CurrentValues, continueButton = FALSE)
  )}
    
if (CurrentValues$page == "not allowed"){
  return(createNotAllowedPage(input, "Instructions"))
}
  
})
  
  
# --------------------------------
# Section F: Event (e.g.; button) actions
# --------------------------------

# game control
observeEvent(input[["trialNr"]], {
  appendDdValues(ctrlVals = CurrentValues, container = ddContainer,
                     input = input, gameData = GameData)
})

# Section F1: Page Navigation Buttons


observeEvent(input[["Instructions_next"]],{
  nextPage(pageId = "instructions", ctrlVals = CurrentValues, nextPageId = "game",
          pageList = instructions.list, globId = "Instructions",
          checkAllowed = TRUE, checkAllowedPage = 1,
          checkIdVar = "workerid", checkLocation = "local",
          checkSep = ",", checkHeader = TRUE, checkFileName = "Id_Database.txt",
          checkNotAllowedId = "not allowed", inputList = input)
})


observeEvent(input[["gt_game"]], {
  CurrentValues$page <- "game"
})

observeEvent(input[["gt_games"]], {
    CurrentValues$page <- "game"
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
                      "time" = GameData$time,
                      "selected" = GameData$selected,
                      "completion.code" = CurrentValues$completion.code,
                      "trial.order" = GameData$trial.order,
                      "trial" = 1:ddContainer$nTrials)
  print(data.list)
  print(lapply(data.list, length))
  # save Data
  saveData(data.list, location = "dropbox", outputDir = outputDir,
           partId = data.list$workerid)

  CurrentValues$page <- "goodbye"
    
  })
  
)})
  
}

# Create app!
shinyApp(ui = ui, server = server)