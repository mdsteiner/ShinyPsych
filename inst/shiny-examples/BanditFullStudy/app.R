# Example of a two armed bandit task using the ShinyPsych package
#
# Code sections:
#   - Section 0: Load Libraries
#   - Section A: assign external values
#   - Section B: Define overall layout
#   - Section C: Define reactive values
#   - Section D: Page layouts
#   - Section F: Event (e.g. button) actions
#       - Section F1: Page navigation button
#       - Section F2: Event Control
#   - Section G: Save Data

# Section 0: Load Libraries ====================================================

library(shiny)
library(shinyjs)
library(ShinyPsych)


# Section A: assign external values ============================================

# Dropbox directory to save data
outputDir <- "ShinyPsych/Bandit"

# Vector with page ids used to later access objects
idsVec <- c("Instructions", "Survey", "Goodbye")

# create page lists for the instructions and the last page
instructions.list <- createPageList(fileName = "Instructions_BanditFullStudy",
                                    globId = "Instructions")
survey.list <- createPageList(fileName = "Survey_BanditFullStudy",
                              globId = "Survey")
goodbye.list <- createPageList(fileName = "Goodbye_BanditFullStudy",
                               globId = "Goodbye")

# prepare a list with game parameters
banditDistList <- list("nTrials" = c(10, rep(20, 10)),  # trials for practice trial and game trials
                       "distributionType" = "normal",
                       "mean" = matrix(c(4, 4, rep(c(4, 2.5), 10)), # arguments for normal dist
                                       ncol = 2, byrow = TRUE),
                       "sd" = matrix(c(2, 2, rep(c(2.5, 11), 10)), # arguments for normal dist
                                     ncol = 2, byrow = TRUE))

# create the outcome lists for the bandit, rounded to whole numbers
banditContainer <- createBanditList(nArms = 2, roundDigits = 0,
                                    distList = banditDistList,
                                    differentDists = FALSE)

# Section B: Define overall layout =============================================

ui <- fixedPage(

  title = "ShinyBandit",      # App title
  uiOutput("MainAction"),
  useShinyjs(),# For Shinyjs functions
  includeScriptFiles(fileList = "bandit", nArms = 2) # include appropriate css and js scripts

)

server <- function(input, output, session) {

  output$MainAction <- renderUI( {
    PageLayouts()

  })

  # Section C: Define Reactive Values ==========================================

  # CurrentValues controls page setting such as which page to display
  CurrentValues <- createCtrlList(firstPage = "instructions", # id of the first page
                                  globIds = idsVec,           # ids of pages for createPage
                                  complCode = TRUE,           # create a completion code
                                  complName = "EP-Bandit",    # first element of completion code
                                  task = "bandit")            # the task(s) used in the app

  # GameData controls task settings and is used to store the task data
  GameData <- createTaskCtrlList(task = "bandit")

  # Section D: Page Layouts ====================================================

  PageLayouts <- reactive({

    # insert created completion code that it can later be displayed
    goodbye.list <- changePageVariable(pageList = goodbye.list, variable = "text",
                                       oldLabel = "completion.code",
                                       newLabel = CurrentValues$completion.code)

    # display instructions page
    if (CurrentValues$page == "instructions") {

      return(
        # create html logic of instructions page
        createPage(pageList = instructions.list,
                   pageNumber = CurrentValues$Instructions.num,
                   globId = "Instructions", ctrlVals = CurrentValues)
      )}

    if (CurrentValues$page == "not allowed"){
      return(
        createNotAllowedPage(input, "Instructions")
      )
    }

    # display task page
    if (CurrentValues$page == "game") {

      return(
        # create html logic of task page and handle client side communications
        multiArmedBanditPage(ctrlVals = CurrentValues, nArms = 2, distList = banditDistList,
                             session = session, container = banditContainer, roundDigits = 1,
                             nTrials = banditDistList$nTrials[CurrentValues$banditGame],
                             nGames = length(banditDistList$nTrials) - 1, withPracticeGame = TRUE)
      )}


  if (CurrentValues$page == "postPractice"){
    return(
        list(
          tags$br(), tags$br(), tags$br(),
          h2("Finished with Practice Game", class = "firstRow"),
          p(paste("You finished the practice game with",
                  GameData$points.cum[length(GameData$points.cum)],
                  "points.")),
          p("On the next pages, you'll start playing the first of 3 real games!"),
          p("Here are a few additional notes and reminders about the game:"),
          tags$ul(
            tags$li("You will play 10 games in total. Your final bonus will be the sum of the bonuses you earn across all games. You will earn a bonus of 1 cent for every 10 points"),
            tags$li("The boxes are the same in each game. However, the",
                    strong("locations of the boxes will be randomly determined"),
                    "at the start of each game. The boxes might be in the same location, or different locations, in each game."),
            tags$li("The point values in the boxes",
                    strong("do not change over time."),
                    " Each time you choose and option, the point value you see is always returned to the box.")
          ),
          p(strong("On the next page the first real game will start. Click to continue when you are ready.")),
          tags$br(),
          actionButton(inputId = "gt_game",
                       label = "Start Game 1", class = "continueButtons")
        )
    )
  }

  # 4) END OF GAME PAGE
  if (CurrentValues$page == "endGame") {
    return(
      div(class = "gameInfo", checked = NA,
          list(
            tags$br(), tags$br(),tags$br(),
            p(paste("You ended Game", CurrentValues$game - 2, "with",
                    GameData$points.cum[length(GameData$points.cum)], "points.")),
            p("Click the button below to start the next game."),
            p("Remember that all games have the same boxes, however, the positions of the boxes will be randomly determined when the game starts."),
            tags$br(),
            actionButton(inputId = "gt_games",
                         label = paste0("Start Game ", CurrentValues$banditGame - 1),
                         class = "continueButtons"))))
  }

  if (CurrentValues$page == "lastEndGame") {

    return(
      div(class = "gameInfo", checked = NA,
          list(
            tags$br(), tags$br(),tags$br(),
            h3("You finished all games!", class = "firstRow"),
            p(paste("You earned", GameData$points.cum[length(GameData$points.cum)],
                    "points in the game.")),
            p("You have now finished playing all 10 games. The points you have earned across all 10 games have been recorded."),
            p("You have earned", CurrentValues$totalPoints,
              "points over all games. Thus you earn a total monetary bonus of",
              round(CurrentValues$totalPoints / 10, 0), "cents."),
            p("Click 'Continue' to start with part 2 of the study."),
            tags$br(),
            actionButton(inputId = "gt_survey",
                         label = "Continue", class = "continueButtons"))))
  }

  if (CurrentValues$page == "survey"){

    return(
      createPage(pageList = survey.list, pageNumber = CurrentValues$Survey.num,
                 globId = "Survey", ctrlVals = CurrentValues)
    )}


# P5) Goodbye
  if (CurrentValues$page == "goodbye") {

    return(
      createPage(pageList = goodbye.list, pageNumber = CurrentValues$Goodbye.num,
                 globId = "Goodbye", ctrlVals = CurrentValues, continueButton = FALSE)
    )}

  })


# Section F: Event (e.g.; button) actions ======================================

# Section F1: Page Navigation Buttons ----------------------


observeEvent(input[["Instructions_next"]],{
  nextPage(pageId = "instructions", ctrlVals = CurrentValues, nextPageId = "game",
          pageList = instructions.list, globId = "Instructions",
          checkAllowed = TRUE, checkAllowedPage = 1,
          checkIdVar = "workerid", checkLocation = "local",
          checkSep = ",", checkHeader = TRUE, checkFileName = "Id_Database.txt",
          checkNotAllowedId = "not allowed", inputList = input)
})

observeEvent(input[["Survey_next"]],{
  nextPage(pageId = "survey", ctrlVals = CurrentValues, nextPageId = "goodbye",
           pageList = survey.list, globId = "Survey")
})

observeEvent(input[["continueBandit"]], {
  nextBanditPage(ctrlVals = CurrentValues, distList = banditDistList,
                 gameData = GameData, withPracticeGame = TRUE)
})

observeEvent(input[["gt_game"]], {
  CurrentValues$page <- "game"
})

observeEvent(input[["gt_games"]], {
  CurrentValues$page <- "game"
  })


# Section F2: Event Control ----------------------

# game control
observeEvent(input[["gameNr"]], {
  appendBanditValues(ctrlVals = CurrentValues, distList = banditDistList,
                     input = input, gameData = GameData)
})

# Make sure answers are selected
observeEvent(reactiveValuesToList(input),{

  onInputEnable(pageId = "instructions", ctrlVals = CurrentValues,
                pageList = instructions.list, globId = "Instructions",
                inputList = input, charNum = 4)

  onInputEnable(pageId = "survey", ctrlVals = CurrentValues,
                pageList = survey.list, globId = "Survey",
                inputList = input, charNum = 2)

})

# Section G: Save data =========================================================
observeEvent(input[["gt_survey"]], {

  # Create progress message
  withProgress(message = "Saving data...", value = 0, {

    incProgress(.25)

    # Create a list to save data
    data.list <- list(  "id" = input$Instructions_workerid,
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
               partId = data.list$id, suffix = "_g")

    CurrentValues$page <- "survey"

  })
})

observeEvent(input[["Survey_next"]], {(
if (CurrentValues$Survey.num >= 5){
  # Create progress message
  withProgress(message = "Saving data...", value = 0, {

    incProgress(.25)

    # Create a list to save data
    data.list <- list(  "id" = input$Instructions_workerid,
                        "completion.code" = CurrentValues$completion.code,
                        "whichHighEv" = input$Survey_whichHighEv,
                        "gameDifficulty" = input$Survey_gameDifficulty,
                        "strategy" = input$Survey_strategy,
                        "strategyChange" = input$Survey_strategyChange,
                        "whichStrategy" = input$Survey_whichStrategy,
                        "similarTask" = input$Survey_similarTask,
                        "comments" = input$Survey_comments,
                        "instructionsClear" = input$Survey_instructionsClear,
                        "notUnderstood" = input$Survey_notUnderstood,
                        "errorOrBugs" = input$Survey_errorOrBugs,
                        "describeError" = input$Survey_describeError,
                        "gaveUp" = input$Survey_gaveUp,
                        "tookNotes" = input$Survey_tookNotes,
                        "usedCalculator" = input$Survey_usedCalculator,
                        "gotHelp" = input$Survey_gotHelp,
                        "age" = input$Survey_age,
                        "sex" = input$Survey_sex,
                        "interesting" = input$Survey_interesting,
                        "education" = input$Survey_education,
                        "trustData" = input$Survey_trustData)

    # save Data
      saveData(data.list, location = "dropbox", outputDir = outputDir,
               partId = data.list$id, suffix = "_s")

    CurrentValues$page <- "goodbye"

    })
}

)})

}

# Create app!
shinyApp(ui = ui, server = server)
