
createPage <- function(pageList, pageNumber, globId, ctrlVals, continueButton = TRUE){
  # create pagelayout with all elements from a given page number of pageList
  
  index <- which(.subset2(pageList, "page") %in% c(0, pageNumber))

  thisPage <- lapply(index, callTag, pageList = pageList)
  
  if (isTRUE(continueButton)){
    if (any(.subset2(pageList, "disabled")[index] == 1)){
      thisPage <- list(shiny::br(), shiny::br(), shiny::br(), thisPage, shiny::br(), 
                    shinyjs::disabled(shiny::actionButton(inputId = paste0(globId, "_next"),
                                          label = "Continue")))
    } else {
      thisPage <- list(shiny::br(), shiny::br(), shiny::br(), thisPage, shiny::br(),
                    shiny::actionButton(inputId = paste0(globId, "_next"),
                                 label = "Continue"))
    }
  } else {
    thisPage <- list(shiny::br(), shiny::br(), shiny::br(), thisPage)
  }
  
  ctrlVals$proceed <- 1

  thisPage
  
}
