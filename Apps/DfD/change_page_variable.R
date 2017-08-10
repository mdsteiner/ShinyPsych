changePageVariable <- function(pageList, variable, oldLabel, newLabel){
  
  pageList[[variable]][pageList[[variable]] == oldLabel] <- newLabel
  
  pageList
  
}