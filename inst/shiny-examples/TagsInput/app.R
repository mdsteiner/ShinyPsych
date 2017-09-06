# Example of the different Inputs and Tags using the ShinyPsych package
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
outputDir <- "ShinyPsych/TagsInput"

# Vector with page ids used to later access objects
idsVec <- c("Instructions", "Survey", "Goodbye")

# create page lists for the instructions and the last page
instructions.list <- createPageList(fileName = "Instructions_TagsInput",
                                    globId = "Instructions")
survey.list <- createPageList(fileName = "TagsInput_Example",
                              globId = "Survey")
goodbye.list <- createPageList(fileName = "Goodbye")


# Section B: Define overall layout =============================================

ui <- fixedPage(

  # App title
  title = "ShinyTagsInput",
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
                                  complName = "EP-TagsInput") # first element of completion code

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

    # display survey page
    if (CurrentValues$page == "savepage") {

      return(
        # create html logic of instructions page
        list(
          tags$br(), tags$br(), tags$br(),
          h2("These were the possible inputs"),
          p("Click continue to save the data."),
          tags$br(),
          actionButton(inputId = "gt_goodbye",
                       label = "Continue")
        )
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
    nextPage(pageId = "survey", ctrlVals = CurrentValues, nextPageId = "savepage",
             pageList = survey.list, globId = "Survey")
  })


  # Section F2: Event Control ----------------------


  # Make sure answers are selected
  observeEvent(reactiveValuesToList(input),{

    onInputEnable(pageId = "survey", ctrlVals = CurrentValues,
                  pageList = survey.list, globId = "Survey",
                  inputList = input, charNum = 4)

    onInputEnable(pageId = "demographics", ctrlVals = CurrentValues,
                  pageList = demographics.list, globId = "Demographics",
                  inputList = input)

  })

  # Section G: Save data =========================================================

  observeEvent(input[["gt_goodbye"]], {(

    # Create progress message
    withProgress(message = "Saving data...", value = 0, {

      incProgress(.25)

      # Create a list to save data
      data.list <- list(  "password" = input$Survey_passInput,
                          "checkbox" = input$Survey_checkBox,
                          "checkbox.group" = input$Survey_checkBoxGroup,
                          "numericInput" = input$Survey_nmrcInput,
                          "radioButton" = input$Survey_radioButton,
                          "selInput" = input$Survey_selInput,
                          "slidInput" = input$Survey_slidInput,
                          "txtInput" = input$Survey_txtInput,
                          "txtAreaInput" = input$Survey_txtAreaInput)

      # save Data
      if (!is.null(input$Instructions_mail) &&
          nchar(input$Instructions_mail) > 4){
        saveData(data.list, location = "mail", outputDir = outputDir,
                 partId = data.list$id, suffix = "_s",
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
