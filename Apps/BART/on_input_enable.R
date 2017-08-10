source("helper.R")

onInputEnable <- function(pageId, ctrlVals, pageList, globId,
                          inputList, charNum = NULL){

  if(ctrlVals$page == pageId){
    
    checkInput <- pageList$id[pageList$page == ctrlVals[[paste0(globId, ".num")]] &
                                pageList$id %in% pageList$obIds]
    checkTypeTemp <- pageList$checkType[pageList$page == ctrlVals[[paste0(globId, ".num")]] &
                               pageList$id %in% pageList$obIds]
    
    if(mean(unlist(lapply(seq_along(checkInput), checkInputFun, inList = inputList,
                          checkType = checkTypeTemp, charNum = charNum, checkInput = checkInput)), na.rm = TRUE) == 1){
      
      shinyjs::enable(paste0(globId, "_next"))
      
      }
  }

}