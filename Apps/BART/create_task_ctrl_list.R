
createTaskCtrlList <- function(task, oVar = NULL, oVarVals = NULL){
  
  if (task == "bandit"){
    
    taskList <- shiny::reactiveValues(trial = c(),
                                      time = c(),
                                      selection = c(),
                                      outcome = c(),
                                      points.cum = c(),
                                      game = c())
    
  } else if (task == "bart"){
    
    taskList <- shiny::reactiveValues(balloon = c(),
                                      time = c(),
                                      pumps = c(),
                                      action = c(),
                                      pop = c())
    
  }
  
  
  if (!is.null(oVar)){
    for (oV in seq_along(oVar)){
      taskList[[oVar[oV]]] <- oVarVals[oV]
    }
  }
  
  taskList
  
}