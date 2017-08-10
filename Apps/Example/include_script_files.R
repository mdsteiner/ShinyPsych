library("shiny")


includeScriptFiles <- function(fileList, globalScript = TRUE){
  
  if (globalScript) {includeCSS("inst/globalStyle.css")}
  
  if (any(fileList == "bandit")){
    includeCSS("inst/banditStyle.css")
    includeScript("inst/bartScript.js")
  }
  
  
}