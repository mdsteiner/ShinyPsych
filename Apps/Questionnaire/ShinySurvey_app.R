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

# Define surveys

# Rational Experiential Inventory (Epstein, Pacini & Denes-Raj, 1996), 10-item version
rei.survey <- list(
  "index" = c(1:10),
  "text" = c("I don't like to have to do a lot of thinking",
             "I try to avoid situations that require thinking in depth about something",
             "I prefer to do something that challenges my thinking abilities rather than something that requires little thought",
             "I prefer complex to simple problems",
             "Thinking hard and for a long time about something gives me little satisfaction",
             "I trust my initial feelings about people",
             "I believe in trusting my hunches",
             "My initial impressions of people are almost always right",
             "When it comes to trusting people, I can usually rely on my 'gut feelings'",
             "I can usually feel when a person is right or wrong even if I can't explain how I know"),
  "reverse" = c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0),
  "values" = c(1:5),
  "randomize" = TRUE,
  "labels" = c(1:5),
  "n" = 10,
  "description" = "1 = Disagree, 5 = Agree")

# --------------------------
# Section A: Setup Survey
# --------------------------

# Section A1: Survey randomization
if(rei.survey$randomize == TRUE) {
  
  rei.order <- sample(1:rei.survey$n, size = rei.survey$n)
  rei.survey$text <- rei.survey$text[rei.order]
  
} else {rei.order <- 1:rei.survey$n}


# Section A2: DATA SAVING
saveDataLocation <- "dropbox"           # Either dropbox, email, or local
outputDir <- "nphillips/ShinySurvey/data"  # Directory to save data

if(saveDataLocation == "dropbox") {droptoken <- readRDS("droptoken.rds")}        # Reads in authentication for dropbox

# Section A4: Miscellaneous code

# Calculate study completion code
completion.code <- paste("EP-SURVEY", sample(100:999, size = 1), 
                         sample(100:999, size = 1),
                         sample(100:999, size = 1), sep = "-")

# --------------------------------
# Section B: Define overall layout
# --------------------------------

ui <- fixedPage(
  
  title = "ShinySurvey",      # App title
  uiOutput("MainAction"),    
  useShinyjs(),# For Shinyjs functions
  tags$script("
    Shiny.addCustomMessageHandler('resetValue', function(variableName) {
              Shiny.onInputChange(variableName, null);
              });
              ")

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
CurrentValues <- reactiveValues(page = "welcome",     # Current page
                                question.num = 1,     # Current question number (for surveys)
                                selected = 0,
                                rei = c())

# --------------------------------
# Section D: Page Layouts
# --------------------------------

PageLayouts <- reactive({
  
# P1) Welcome
if (CurrentValues$page == "welcome") {
  
  return(
    list(
      h2("Decision Making Survey"),
      p("The purpose of this study is to understand how people make decisions"),
      p("There are no health risks or personal identifying information associated with your participation."),
      p("This study is founded by the chair of the department of Economic Psychology at the University of Basel."),
      p("Your responses will be anonymous and in a group and your individual responses will not be published."),
      p("If you consent to participating in this study, please enter a unique ID that no one else would use and click Continue."),
      textInput(inputId = "workerid", 
                label = "Please enter a unique ID that no one else would use", 
                value = "", 
                placeholder = "e.g.; Cat57Door"),
      # This displays the action putton Next.
      actionButton(inputId = "gt_instructions", 
                   label = "Continue") 
    )
  )}

# P2) Instructions
if (CurrentValues$page == "instructions") {
  
  return(
    list(
      h2("Surveys"),
      p("On the next pages, you will complete a few short surveys. Please answer the questions as honestly and openly as you can."),
      actionButton(inputId = "gt_rei", 
                   label = "Continue") 
    )
  )}
  
# P3) REI
if (CurrentValues$page == "rei") {
  
  return(
    list(
      h3(paste(rei.survey$text[CurrentValues$question.num])),
      em(paste(rei.survey$description)),
      radioButtons(inputId = "rei_survey", 
                  choices = rei.survey$labels, 
                  selected = character(0), 
                  inline = TRUE,
                  label = ""),
     # h3(CurrentValues$selected),
      disabled(actionButton(inputId = "nextq", label = "Continue"))
     
    )
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
observeEvent(input$gt_rei, {CurrentValues$page <- "rei"})

observeEvent(input$nextq, {
  
  CurrentValues$selected <- character(0)
  
  # Recode rei responses
  if(CurrentValues$page == "rei") {
    
    CurrentValues$question.num <- CurrentValues$question.num + 1
    CurrentValues$rei <- c(CurrentValues$rei, input$rei_survey)
    
    if(CurrentValues$question.num > length(rei.survey$text)) {
      
      CurrentValues$page <- "demographics"
      
    }
    
  }
  
})

# Make sure answers are selected in rei survey
observeEvent(input$rei_survey, {
  CurrentValues$selected <- input$rei_survey
  if(CurrentValues$page == "rei") {
  if(!is.null(input$rei_survey)) {enable("nextq")}
  }
})

# Reset value of radio buttons
observeEvent(input$nextq, {
  updateRadioButtons(session, "rei_survey", selected = character(0))
  session$sendCustomMessage(type = "resetValue", message = "rei_survey")
})



# --------------------------------
# Section G: Save data
# --------------------------------
observeEvent(input$gt_goodbye, {(
  
# Create progress message   
withProgress(message = "Saving data...", value = 0, {
  
  incProgress(.25)

  # Calculate REI score
  rei.values <- as.numeric(CurrentValues$rei)
  
  # Get original order back
  rei.values <- rei.values[rei.order]
  
  # Reverse coding
  rei.values[rei.survey$reverse == 1] <- 6 - rei.values[rei.survey$reverse == 1]
  
  # Calculate final mean
  rei.score <- mean(rei.values)
  
  # Create a list to save data
  data.list <- list(c("workerid" = input$workerid,
                      "rei" = paste(CurrentValues$rei, collapse = ","),
                      "rei.order" = paste(rei.order, collapse = ","),
                      "rei.score" = rei.score,
                      "sex" = input$sex,
                      "age" = input$age,
                      "completion.code" = completion.code))
  # save Data
  callModule(saveData, "surveyData", data.list, saveDataLocation = saveDataLocation, outputDir = outputDir, droptoken = droptoken)
    
  CurrentValues$page <- "goodbye"
    
  })
  
)})
  
}

# Create app!
shinyApp(ui = ui, server = server)