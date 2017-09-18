#' Set up HTML Logic for the Delay Discounting Task
#'
#' Create html page from a list created by \code{\link{createDdList}} and
#' control client side communication.
#' @param ctrlVals list of reactive values. Controls the flow through the
#'  Experiment. Can be created with \code{\link{createCtrlList}}.
#' @param container list. Containins the outcome time pairs and other
#'  information on how to set up the tasks. Can be created with
#'  \code{\link{createDdList}}
#' @param trialTitle string. Is displayed as title of the trial, e.g. if it is
#'  set to "Trial" the displayed title in trial 2 of 10 will be "Trial 2 of 10".
#'  Only needed if displayTitle is TRUE.
#' @param trialTitlePractice string. Is displayed as title of the practice
#'  trial. Only needed if displayTitle is TRUE.
#' @param withPracticeTrial logical. If TRUE (default is FALSE) a practice
#'  trial has to be provided and is included.
#' @param displayTitle logical. If TRUE (default) a title is displayed.
#' @param nTrials integer. The number of trials played WITHOUT the practice
#'  trial.
#'
#' @return An html page that can be displayed by the shiny app.
#' @export
ddPage <- function(ctrlVals, container, trialTitle = "Trial",
                   trialTitlePractice = "Practice Trial",
                   withPracticeTrial = FALSE, displayTitle = TRUE,
                   nTrials = 10){

  dInd <- ctrlVals$ddTrial

  if (isTRUE(withPracticeTrial)){

    list(
      shiny::fixedRow(

        # call the neGame() javascript function to prepare objects
        shiny::tag("script", paste0('newDdGame(', dInd, ');')),

        # set up written info displayed above the boxes
        shiny::column(12,
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::column(12, align = "left",
                                                    shiny::h2(ifelse(dInd == 1,
                                                                     trialTitlePractice,
                                                                     paste(trialTitle,
                                                                           dInd - 1,
                                                                           "of", nTrials))))),
                      shiny::fixedRow(shiny::br()),

                      # set up logic to update the boxes via javascript. Arguments passed to js come from
                      # newGame() js function and the sendCustomMessage called just before this function
                      shiny::fixedRow(
                        shiny::column(1, align="center",
                                      shiny::HTML('<h1 id="emptySpace1" class="emptySpace">Place</h1>')),
                        shiny::column(5, align="center",
                                      shiny::HTML(paste(
                                        '<div class = "box"><div class = "options" id="opt1"
                                        onclick="onOptionClick(\'opt1\', 1, t, respTime, clickEnabled,
                                        trialNr)"><table>', container$outcomes[[dInd]][[1]],
                                        "</table></div></div>"))),
                        shiny::column(5, align="center",
                                      shiny::HTML(paste(
                                        '<div class = "box"><div class = "options" id="opt2"
                                        onclick="onOptionClick(\'opt2\', 2, t, respTime, clickEnabled,
                                        trialNr)"><table>', container$outcomes[[dInd]][[2]],
                                        "</table></div></div>"))),
                        shiny::column(1, align="center",
                                      shiny::HTML('<h1 id="emptySpace2" class="emptySpace">Place</h1>'))))))
  } else if (isTRUE(displayTitle)) {
    list(
      shiny::fixedRow(

        # call the neGame() javascript function to prepare objects
        shiny::tag("script", paste0('newDdGame(', dInd, ');')),

        # set up written info displayed above the boxes
        shiny::column(12,
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::column(12, align = "left",
                                                    shiny::h2(paste(trialTitle,
                                                                    dInd,
                                                                    "of", nTrials)))),
                      shiny::fixedRow(shiny::br()),

                      # set up logic to update the boxes via javascript. Arguments passed to js come from
                      # newGame() js function and the sendCustomMessage called just before this function
                      shiny::fixedRow(
                        shiny::column(1, align="center",
                                      shiny::HTML('<h1 id="emptySpace1" class="emptySpace">Place</h1>')),
                        shiny::column(5, align="center",
                                      shiny::HTML(paste(
                                        '<div class = "box"><div class = "options" id="opt1"
                                        onclick="onOptionClick(\'opt1\', 1, t, respTime, clickEnabled,
                                        trialNr)"><table>', container$outcomes[[dInd]][[1]],
                                        "</table></div></div>"))),
                        shiny::column(5, align="center",
                                      shiny::HTML(paste(
                                        '<div class = "box"><div class = "options" id="opt2"
                                        onclick="onOptionClick(\'opt2\', 2, t, respTime, clickEnabled,
                                        trialNr)"><table>', container$outcomes[[dInd]][[2]],
                                        "</table></div></div>"))),
                        shiny::column(1, align="center",
                                      shiny::HTML('<h1 id="emptySpace2" class="emptySpace">Place</h1>'))))))
  } else {
    list(
      shiny::fixedRow(

        # call the neGame() javascript function to prepare objects
        shiny::tag("script", paste0('newDdGame(', dInd, ');')),

        # set up written info displayed above the boxes
        shiny::column(12,
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),

                      # set up logic to update the boxes via javascript. Arguments passed to js come from
                      # newGame() js function and the sendCustomMessage called just before this function
                      shiny::fixedRow(
                        shiny::column(1, align="center",
                                      shiny::HTML('<h1 id="emptySpace1" class="emptySpace">Place</h1>')),
                        shiny::column(5, align="center",
                                      shiny::HTML(paste(
                                        '<div class = "box"><div class = "options" id="opt1"
                                        onclick="onOptionClick(\'opt1\', 1, t, respTime, clickEnabled,
                                        trialNr)"><table>', container$outcomes[[dInd]][[1]],
                                        "</table></div></div>"))),
                        shiny::column(5, align="center",
                                      shiny::HTML(paste(
                                        '<div class = "box"><div class = "options" id="opt2"
                                        onclick="onOptionClick(\'opt2\', 2, t, respTime, clickEnabled,
                                        trialNr)"><table>', container$outcomes[[dInd]][[2]],
                                        "</table></div></div>"))),
                        shiny::column(1, align="center",
                                      shiny::HTML('<h1 id="emptySpace2" class="emptySpace">Place</h1>'))))))
  }

}

