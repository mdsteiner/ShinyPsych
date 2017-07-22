library("shiny")
library("shinyjs")

createPage <- function(pageList, pageNumber, globId, contrlVals){
  # create pagelayout with all elements from a given page number of pageList
  
  index <- which(.subset2(pageList, "page") %in% c(0, pageNumber))
  
  thisPage <- lapply(index, callTag, pageList = pageList)
  
  if (any(.subset2(pageList, "disabled")[index] == 1)){
    thisPage <- list(br(), br(), br(), thisPage, br(), 
                  disabled(actionButton(inputId = paste0(globId, "_next"),
                                        label = "Continue")))
  } else {
    thisPage <- list(br(), br(), br(), thisPage, br(),
                  actionButton(inputId = paste0(globId, "_next"),
                               label = "Continue"))
  }
  
  contrlVals$proceed <- 1
  
  thisPage
  
}
