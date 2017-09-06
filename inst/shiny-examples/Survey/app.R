# Example of a Survey using the ShinyPsych package
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
outputDir <- "ShinyPsych/Survey"

# Vector with page ids used to later access objects
idsVec <- c("Instructions", "Survey", "Demographics", "Goodbye")

# create page lists for the instructions and the last page
instructions.list <- createPageList(fileName = "Instructions_Survey",
                                    globId = "Instructions")
survey.list <- createPageList(fileName = "Survey_Example",
                              globId = "Survey")
demographics.list <- createPageList(fileName = "Demographics")
goodbye.list <- createPageList(fileName = "Goodbye")


# Section B: Define overall layout =============================================

ui <- fixedPage(

  # App title
  title = "ShinySurvey",
  uiOutput("MainAction"),

  # For Shinyjs functions
  useShinyjs(),

  # include appropriate css and js scripts
  includeScriptFiles()

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
                                  complName = "EP-Survey")    # first element of completion code

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

    # display survey page
    if (CurrentValues$page == "survey") {

      return(
        # create html logic of instructions page
        createPage(pageList = survey.list,
                   pageNumber = CurrentValues$Survey.num,
                   globId = "Survey", ctrlVals = CurrentValues)
      )}


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
    nextPage(pageId = "instructions", ctrlVals = CurrentValues, nextPageId = "survey",
             pageList = instructions.list, globId = "Instructions")
  })

  observeEvent(input[["Survey_next"]],{
    nextPage(pageId = "survey", ctrlVals = CurrentValues,
             nextPageId = "demographics", pageList = survey.list,
             globId = "Survey")
  })


  # Section F2: Event Control ----------------------


  # Make sure answers are selected
  observeEvent(reactiveValuesToList(input),{

    onInputEnable(pageId = "instructions", ctrlVals = CurrentValues,
                  pageList = instructions.list, globId = "Instructions",
                  inputList = input, charNum = 4)

    onInputEnable(pageId = "survey", ctrlVals = CurrentValues,
                  pageList = survey.list, globId = "Survey",
                  inputList = input)

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
                          "color" = input$Survey_qu1,
                          "capital" = input$Survey_qu2,
                          "swallow" = input$Survey_qu3,
                          "like.survey" = input$Survey_qu4,
                          "age" = input$Demographics_age,
                          "sex" = input$Demographics_sex,
                          "input.order" = survey.list$id.order)

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
                 partId = data.list$id, suffix = "_s")
      }

      CurrentValues$page <- "goodbye"

    })

  )})

}

# Create app!
shinyApp(ui = ui, server = server)
