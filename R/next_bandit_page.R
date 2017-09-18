#' Go to Next Page After a Game of the Bandit Task
#'
#' If a game of the bandit task is finished this function will (if properly set
#' up, this has to be handled in the app) be called if the continue button is
#' pressed.
#' @param ctrlVals list of reactive values. Controls the flow through the
#'  Experiment. Can be created with \code{\link{createCtrlList}}.
#' @param distList list. The one used to create the outcome lists with
#'  \code{\link{createBanditList}}.
#' @param gameData list of reactive values. Used for task control. Created with
#'  \code{\link{createTaskCtrlList}}
#' @param withPracticeGame logical. If TRUE (default) the page after the first
#'  game will be afterPracticePage, else it will be afterGamePage.
#' @param afterPracticePage string. The name of the page to be shown
#'  after the practice trial. Only needs to be specified if withPracticeGamble
#'  is set to TRUE.
#' @param afterGamePage string. The name of the page to be shown
#'  after the trials up to the second to last trial.
#' @param afterLastGamePage string. The name of the page to be shown
#'  after the last trial.
#'
#' @return An updated version of ctrlVals. Does not have to be assigned, since
#'  it is a reactive value.
#' @export
nextBanditPage <- function(ctrlVals, distList, gameData,
                           withPracticeGame = TRUE,
                           afterPracticePage = "postPractice",
                           afterGamePage = "endGame",
                           afterLastGamePage = "lastEndGame"){

  if (isTRUE(withPracticeGame)){

    if (ctrlVals$banditGame == 1){

      ctrlVals$page <- afterPracticePage

      ctrlVals$banditGame <- unique(gameData$game)[length(unique(gameData$game))] + 1

    } else if (any(c(2:(length(distList$nTrials) - 1)) == ctrlVals$banditGame)){

      ctrlVals$page <- afterGamePage

      ctrlVals$banditGame <- unique(gameData$game)[length(unique(gameData$game))] + 1
    } else {

      ctrlVals$page <- afterLastGamePage

      ctrlVals$banditGame <- unique(gameData$game)[length(unique(gameData$game))] + 1
    }
  } else{

    if (any(seq_len(length(distList$nTrials) - 1) == ctrlVals$banditGame)){

      ctrlVals$page <- afterGamePage

      ctrlVals$banditGame <- unique(gameData$game)[length(unique(gameData$game))] + 1
    } else {

      ctrlVals$page <- afterLastGamePage

      ctrlVals$banditGame <- unique(gameData$game)[length(unique(gameData$game))] + 1

    }
  }
}
