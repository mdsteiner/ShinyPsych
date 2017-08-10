
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
    
  } else if (task == "dfe"){
    
    taskList <- shiny::reactiveValues(trial = c(),
                                      time = c(),
                                      samples = c(),
                                      selected = c(),
                                      finalOutcome = c(),
                                      outcome = c(),
                                      gamble = c(),
                                      option.order = c(),
                                      gamble.order = c())
  }
  
  
  if (!is.null(oVar)){
    for (oV in seq_along(oVar)){
      taskList[[oVar[oV]]] <- oVarVals[oV]
    }
  }
  
  taskList
  
}