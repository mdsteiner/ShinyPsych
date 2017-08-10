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
source("append_bandit_values.R")
source("create_task_ctrl_list.R")
source("create_bandit_list.R")
source("multi_armed_bandit_page.R")
source("next_bandit_page.R")
source("two_armed_bandit_page.R")
source("three_armed_bandit_page.R")
source("bandit_custom_message.R")
source("four_armed_bandit_page.R")
source("five_armed_bandit_page.R")
source("six_armed_bandit_page.R")
source("include_script_files.R")
# Define surveys


# --------------------------
# Section A: Setup Survey
# --------------------------

# Section A2: DATA SAVING

outputDir <- "msteiner/ShinyPsych/Bandit"  # Directory to save data

# Section A4: Miscellaneous code



# --------------------------------
# Section B: Define overall layout
# --------------------------------

ui <- fixedPage(
  
  title = "ShinySurvey",      # App title
  uiOutput("MainAction"),    
  useShinyjs(),# For Shinyjs functions
  includeScriptFiles(fileList = "bandit", nArms = 4)
  
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
                                  task = "bandit")
  print(isolate(reactiveValuesToList(CurrentValues)))
  instructions.list <- preparePageList(defaultName = "Instructions")
  
  goodbye.list <- preparePageList(defaultName = "Goodbye")
  
  GameData <- createTaskCtrlList(task = "bandit")
  
  banditDistList <- list("nTrials" = c(5, rep(5, 4)),
                         "distributionType" = "normal",
                         "mean" = matrix(c(4, 4, 4, 4, rep(c(2, 4, 3, 4), 4)), ncol = 4, byrow = TRUE),
                         "sd" = matrix(c(3, 3, 3, 3, rep(c(2.5, 11, 5, 7), 4)), ncol = 4, byrow = TRUE))
  
  banditContainer <- createBanditList(nArms = 4, roundDigits = 1,
                                      distList = banditDistList)
  
  
  
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
        multiArmedBanditPage(ctrlVals = CurrentValues, nArms = 4, distList = banditDistList,
                             session = session, container = banditContainer, roundDigits = 1,
                             nTrials = banditDistList$nTrials[CurrentValues$banditGame],
                             nGames = length(banditDistList$nTrials) - 1, withPracticeGame = TRUE)
      )}
    
    
if (CurrentValues$page == "postPractice"){
  return(
    div(class = "gameInfo", checked = NA,
        list(
          tags$br(), tags$br(),
          h2("Finished with Practice Game", class = "firstRow"),
          p(paste("You finished the practice game with", GameData$points.cum[length(GameData$points.cum)], "points. If this was a real game, you would have earned", round(GameData$points.cum[length(GameData$points.cum)] / 10, 0), "cents for this game.")),
          p("On the next pages, you'll start playing the first of 10 real games that will count towards your bonus!"),
          p("Here are a few additional notes and reminders about the game:"),
          tags$ul(
            tags$li("You will play 10 games in total. Your final bonus will be the sum of the bonuses you earn across all games. You will earn a bonus of 1 cent for every 10 points"),
            tags$li("The boxes are the same in each game. However, the", strong("locations of the boxes will be randomly determined"), "at the start of each game. The boxes might be in the same location, or different locations, in each game."),
            tags$li("The point values in the boxes", strong("do not change over time."), " Each time you choose and option, the point value you see is always returned to the box.")
          ),
          p(strong("On the next page the first real game will start. Click to continue when you are ready.")),
          tags$br(),
          actionButton(inputId = "gt_game", 
                       label = "Start Game 1", class = "continueButtons") 
        )
    )
  )
}
  
  # 4) END OF GAME PAGE
if (CurrentValues$page == "endGame") {
  return(
    div(class = "gameInfo", checked = NA,
        list(
          tags$br(), tags$br(),
          p(paste("You ended Game", CurrentValues$game - 2, "with", GameData$points.cum[length(GameData$points.cum)], "points.")),
          
          h3(paste("You earned", round(GameData$points.cum[length(GameData$points.cum)] / 10, 0), "cents for this game!")),
          
          p("Click the button below to start the next game."),
          p("Remember that all games have the same boxes, however, the positions of the boxes will be randomly determined when the game starts."),
          tags$br(),
          actionButton(inputId = "gt_games", 
                       label = paste0("Start Game ", CurrentValues$banditGame - 1), class = "continueButtons"))))
}
  
if (CurrentValues$page == "lastEndGame") {
  
  return(
    div(class = "gameInfo", checked = NA,
        list(
          tags$br(), tags$br(),
          h3("You finished all games!", class = "firstRow"),
          p(paste("You earned", GameData$points.cum[length(GameData$points.cum)], "points in the game.")),
          p("You have now finished playing all 10 games. The points you have earned across all 10 games have been recorded."),
          p("You have earned", sum(GameData$points), "points over all games. Thus you earn a total monetary bonus of", round(sum(GameData$points) / 10, 0), "cents."),
          tags$br(),
          h3("Please answer the following question about the two options."),
          radioButtons("which.high.ev",
                       label = "You may have noticed that in each game, there was one option with a larger variability of outcomes, and one option with a smaller variability. Do you think one of these options had, on average, better points than the other?",
                       choices = list("I think the option with the higher point variability also had higher values on average." = 1,
                                      "I think the option with the lower point variability had higher values on average." = 2,
                                      "I think both options gave the same number of points on average." = 3),
                       selected = character(0),
                       width = "800px"),
          tags$br(),
          disabled(actionButton(inputId = "gt_goodbye", 
                                label = "Continue", class = "continueButtons")))))
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

# game control
observeEvent(input[["gameNr"]], {
  appendBanditValues(ctrlVals = CurrentValues, distList = banditDistList,
                     input = input, gameData = GameData)
})

# Section F1: Page Navigation Buttons


observeEvent(input[["Instructions_next"]],{
  nextPage(pageId = "instructions", ctrlVals = CurrentValues, nextPageId = "game",
          pageList = instructions.list, globId = "Instructions")
})

observeEvent(input[["continueBandit"]], {
  nextBanditPage(ctrlValues = CurrentValues, distList = banditDistList,
                 gameData = GameData, withPracticeGame = TRUE)
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
  
  if (CurrentValues$page == "lastEndGame"){
    if(!is.null(input[["which.high.ev"]])){
      enable("gt_goodbye")
    }
  }
  
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
                      "which.high.ev" = input$which.high.ev,
                      "trial" = GameData$trial,
                      "time" = GameData$time,
                      "selection" = GameData$selection,
                      "outcomes" = GameData$outcome,
                      "points.cum" = GameData$points.cum,
                      "game" = GameData$game,
                      "completion.code" = CurrentValues$completion.code,
                      "option.order" = banditContainer$option.order)

  # save Data
  saveData(data.list, location = "dropbox", outputDir = outputDir,
           partId = data.list$workerid)

  CurrentValues$page <- "goodbye"
    
  })
  
)})
  
}

# Create app!
shinyApp(ui = ui, server = server)