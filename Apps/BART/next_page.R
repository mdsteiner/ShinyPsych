
nextPage <- function(pageId, ctrlVals, nextPageId, pageList, globId ){
  if(ctrlVals$page == pageId) {
    
    # create index for page number
    tempIndex <- paste0(globId, ".num")

    # add 1 to the pagenumber
    ctrlVals[[tempIndex]] <- ctrlVals[[tempIndex]] + ctrlVals$proceed

    # set proceed value to 0 so that multiple clicking doesn't lead to
    # skip pages
    ctrlVals$proceed <- 0

    if(ctrlVals[[tempIndex]] > max(pageList$page,
                                 na.rm = TRUE)) {

      # if no more pages are in pageList, proceed to next pageList
      ctrlVals$page <- nextPageId
    }

  }
  
}