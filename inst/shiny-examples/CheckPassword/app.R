# Example of the CheckPassword function using the ShinyPsych package
#
# Code sections:
#   - Section 0: Load Libraries
#   - Section A: assign external values
#   - Section B: Define overall layout
#   - Section C: Define reactive values
#   - Section D: Page layouts
#   - Section F: Event (e.g. button) actions
#   - Section G: Save Data

# Section 0: Load Libraries ====================================================

library(shiny)
library(shinyjs)
library(ShinyPsych)


# Section A: assign external values ============================================

# Vector with page ids used to later access objects
idsVec <- c("Instructions", "Goodbye")

# create page lists for the instructions and the last page
instructions.list <- createPageList(fileName = "Instructions_CheckPassword",
                                    globId = "Instructions")
goodbye.list <- createPageList(fileName = "Goodbye")


# Section B: Define overall layout =============================================

ui <- fixedPage(

  # App title
  title = "ShinyCheckPassword",
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
  CurrentValues <- createCtrlList(firstPage = "Instructions", # id of the first page
                                  globIds = idsVec,           # ids of pages for createPage
                                  complCode = TRUE,           # create a completion code
                                  complName = "EP-CheckPassword") # first element of completion code
  # Section D: Page Layouts ====================================================

  PageLayouts <- reactive({

    # insert created completion code so it can later be displayed
    goodbye.list <- changePageVariable(pageList = goodbye.list,
                                       variable = "text",
                                       oldLabel = "completion.code",
                                       newLabel = CurrentValues$completion.code)

    # display instructions page
    if (CurrentValues$page == "Instructions") {

      return(
        # create html logic of instructions page
        createPage(pageList = instructions.list,
                   pageNumber = CurrentValues$Instructions.num,
                   globId = "Instructions",
                   ctrlVals = CurrentValues)
      )}

    # Goodbye page
    if (CurrentValues$page == "goodbye") {

      return(
        createPage(pageList = goodbye.list, pageNumber = CurrentValues$Goodbye.num,
                   globId = "Goodbye", ctrlVals = CurrentValues, continueButton = FALSE)
      )}

    # If the wrong password has been entered
    if (CurrentValues$page == "wrong password"){
      return(
        createWrongPasswordPage()
      )
    }
  })


  # Section F: Event (e.g.; button) actions ======================================

  observeEvent(input[["Instructions_next"]],{
    nextPage(pageId = "Instructions", ctrlVals = CurrentValues, nextPageId = "goodbye",
             pageList = instructions.list, globId = "Instructions",
             checkAllowed = "password", checkAllowedPage = 1,
             checkIdVar = "pwInput", checkLocation = "local",
             checkFileName = "Password_Database.txt", checkNotAllowedId = "wrong password",
             inputList = input)
  })

  observeEvent(input[["wrongPWBack"]],{
    CurrentValues$Instructions.num <- 1
    CurrentValues$page = "Instructions"
  })
} # End of server function

# Create app!
shinyApp(ui = ui, server = server)
