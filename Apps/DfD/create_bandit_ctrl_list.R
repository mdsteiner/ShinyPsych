createBanditCtrlList <- function(oVar = NULL, oVarVals = NULL){
  
  GameData <- shiny::reactiveValues(trial = c(),
                                    time = c(),
                                    selection = c(),
                                    outcome = c(),
                                    points.cum = c(),
                                    game = c())
  
  if (!is.null(oVar)){
    for (oV in seq_along(oVar)){
      GameData[[oVar[oV]]] <- oVarVals[oV]
    }
  }
  
  GameData
}