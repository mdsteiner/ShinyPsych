# Example of the BART using the ShinyPsych package
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
outputDir <- "ShinyPsych/Bart"

# Vector with page ids used to later access objects
idsVec <- c("Instructions", "Demographics", "Goodbye")

# create page lists for the instructions and the last page
instructions.list <- createPageList(fileName = "Instructions_Bart",
                                    globId = "Instructions")
demographics.list <- createPageList(fileName = "Demographics")
goodbye.list <- createPageList(fileName = "Goodbye")

# prepare a list with game parameters
bartDistList <- list("diffBalloons" = 2,
                     "balloonIds" = c(1, 2),
                     "distributionType" = c("unif", "normal"),
                     "min" = c(1, NA),
                     "max" = c(10, NA),
                     "mean" = c(NA, 12),
                     "sd" = c(NA, 3),
                     "nBalloons" = c(5, 5),
                     "balloonColor" = c("blue", "grey"),
                     "max.pop" = c(10, 15),
                     "min.pop" = c(2, 2))

# create the popped values lists for the bart
bartContainer <- createBartList(distList = bartDistList, randomize = TRUE)

# Section B: Define overall layout =============================================

ui <- fixedPage(

  title = "ShinyBart",      # App title
  uiOutput("MainAction"),
  useShinyjs(),# For Shinyjs functions
  includeScriptFiles(fileList = "bart") # include appropriate css and js scripts

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
                                  complName = "EP-Bart",      # first element of completion code
                                  task = "bart")              # the task(s) used in the app

  # GameData controls task settings and is used to store the task data
  GameData <- createTaskCtrlList(task = "bart")

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

    # display task page
    if (CurrentValues$page == "game") {

      return(
        # create html logic of task page and handle client side communications
        createBartPage(ctrlVals = CurrentValues, session = session,
                       balloonList = bartContainer, RDrawBoundary = TRUE)
      )}


    if (CurrentValues$page == "gameend") {

      return(list(tags$br(), tags$br(), tags$br(),
        h3("You finished the game!"),
                  p(paste("You earned", CurrentValues$points, "points in the game.")),
                  p("Please click continue to complete a short survey"),
                  actionButton(inputId = "gt_demographics",
                               label = "Continue")))
    }

    if (CurrentValues$page == "demographics"){

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


  # Section F: Event (e.g.; button) actions ======================================

  # Section F1: Page Navigation Buttons ----------------------


  observeEvent(input[["Instructions_next"]],{
    nextPage(pageId = "instructions", ctrlVals = CurrentValues, nextPageId = "game",
             pageList = instructions.list, globId = "Instructions")
  })

  # Look for final balloon -> Go to gameend
  observeEvent({CurrentValues[["balloon"]]}, {

    onBartAction(id = "next_page", ctrlVals = CurrentValues, input = input,
                 bartCtrlList = GameData, balloonList = bartContainer,
                 nextPageId = "gameend")

  })


  observeEvent(input[["gt_demographics"]], {
    CurrentValues$page <- "demographics"
  })

  # Section F2: Event Control ----------------------

  # game control

  # After a pop, start next balloon
  observeEvent({input[["nextballoon"]]}, {

    onBartAction(id = "nextballoon", ctrlVals = CurrentValues, input = input,
                 bartCtrlList = GameData, balloonList = bartContainer)

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

    onInputEnable(pageId = "demographics", ctrlVals = CurrentValues,
                  pageList = demographics.list, globId = "Demographics",
                  inputList = input)

  })

  # Section G: Save data =========================================================

  observeEvent(input[["Demographics_next"]], {(

    # Create progress message
    withProgress(message = "Saving data...", value = 0, {

      incProgress(.25)

      # Create a list to save data
      data.list <- list(  "id" = input$Instructions_workerid,
                          "balloon" = GameData$balloon,
                          "time" = GameData$time,
                          "action" = GameData$action,
                          "pop" = GameData$pop,
                          "completion.code" = CurrentValues$completion.code,
                          "balloonIds" = bartContainer$balloonIds[GameData$balloon],
                          "age" = input$Demographics_age,
                          "sex" = input$Demographics_sex)

      # save Data
      if (!is.null(input$Instructions_mail) &&
          nchar(input$Instructions_mail) > 4){
        saveData(data.list, location = "mail", outputDir = outputDir,
                 partId = data.list$id, suffix = "_g",
                 mailSender = "shinypsych@gmail.com",
                 mailReceiver = input$Instructions_mail,
                 mailBody = "Your data sent by the ShinyPsych app demo.",
                 mailSubject = paste("ShinyPsych data for id", data.list$id))
      } else {
        saveData(data.list, location = "dropbox", outputDir = outputDir,
                 partId = data.list$id, suffix = "_g")
      }

      CurrentValues$page <- "goodbye"

    })

  )})

}

# Create app!
shinyApp(ui = ui, server = server)
