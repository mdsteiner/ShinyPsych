#' Create a List of Outcome Values to Use in the BART Task
#'
#' Create a list of pop values (integer value indicating the number of pumps
#' at which the balloon will pop) drawn from distributions with the specified
#' parameter values.
#'
#' A valid list must have the following options:
#' An integer number of different balloons that can have differing colors and
#' distributions named "diffBalloons".
#' An integer vector named "balloonIds" containing the identifier for your
#' balloons. Must have length specified in "diffBalloons". Ids must be integers
#' because they are later used for indexing.
#' A vector named "distributionType" containing the distribution indicators, one
#' fo each balloon specified at "diffBalloons". Valid inputs are "normal", "exp",
#' "unif", "beta" or "exgauss".
#' For each distribution type a vector of the lenth of number of different
#' balloons named the same as the arguments the specific distributions take.
#' The names of these arguments must match the argument names from the
#' distribution documentations. If different distributions are used the length
#' of the vectors must still match the number of different balloons, use
#' placeholders for the positions where a specific distribution is not used
#' (see example).
#' A vector named "nBalloons" specifying how many balloons of each of the
#' different balloons you want to have, i.e. if you have two different balloons
#' and want each to be played 5 times, set this to c(5, 5) as in the example.
#' A vector named "balloonColors" specifying the color of each of the different
#' balloons.
#' A vector named "max.pop" with integer values specifying the maximum number
#' of pumps that should be possible, regardless of the distribution type. Any
#' values drawn from the distributions that are larger than the respective
#' max.pop value is set to max.pop.
#' @param distList list. Contains information about the distributions from which
#'  the values should be drawn.
#' @param randomize logical. If TRUE balloon order is randomized.
#' @param loadList logical. If TRUE (default is FALSE) the list specifying
#'  the distribution will be loaded from a local file.
#' @param fileName string. The name and path of the rds file(s) with the
#'  distribution info. Has to be specified if loadList is set to TRUE. Can be
#'  a vector of different files that will be read in and merged.
#' @importFrom stats rbeta rexp rnorm runif
#'
#' @return A list containing a list of pop values, the balloon colors, the
#'  number of balloons, the maximum possible pop values and the balloon ids.
#' @export
#'
#' @examples
#'
#' ### Example with input list
#' # create a list containing distribution info
#' bartDistList <- list("diffBalloons" = 2,
#'                      "balloonIds" = c(1, 2),
#'                      "distributionType" = c("unif", "normal"),
#'                      "min" = c(1, NA),
#'                      "max" = c(10, NA),
#'                      "mean" = c(NA, 12),
#'                      "sd" = c(NA, 3),
#'                      "nBalloons" = c(5, 5),
#'                      "balloonColor" = c("blue", "grey"),
#'                      "max.pop" = c(10, 15),
#'                      "min.pop" = c(2, 2))
#'
#' # call createBartList to draw pop values from the distributions
#' bartContainer <- createBartList(distList = bartDistList, randomize = TRUE)
#'
#' bartContainer
#'
#' rm(bartDistList)
#' rm(bartContainer)
#'
#' ### Example with loaded lists
#' # file directory of one file, containing mixed distributions for 2 balloons
#' containerDir <- system.file("shiny-examples", "BART", "mixedPopVals.RDS",
#'                             package = "ShinyPsych")
#' newContainer <- createBartList(randomize = TRUE, fileName = containerDir,
#'                                loadList = TRUE)
#' newContainer
#'
#' # file directories of two files containing one balloon type each
#' containerDirVec <- c(system.file("shiny-examples", "BART", "unifPopVals.RDS",
#'                                  package = "ShinyPsych"),
#'                      system.file("shiny-examples", "BART", "normalPopVals.RDS",
#'                                  package = "ShinyPsych"))
#'
#' newContainer <- createBartList(randomize = TRUE, fileName = containerDirVec,
#'                               loadList = TRUE)
#' newContainer
#'
#' rm(containerDir)
#' rm(containerDirVec)
#' rm(newContainer)
createBartList <- function(distList, randomize, loadList = FALSE,
                           fileName = NULL){

  if (isTRUE(loadList)){

    if (length(fileName) > 1){
      balloonList <- readRDS(fileName[1])

      for (ii in 2:length(fileName)){
        temp.list <- readRDS(fileName[ii])

        balloonList$PopVals <- c(balloonList$PopVals, temp.list$PopVals)
        balloonList$balloonColors <- c(balloonList$balloonColors,
                                       temp.list$balloonColors)
        balloonList$nBalloons <- c(balloonList$nBalloons, temp.list$nBalloons)
        balloonList$max.pop <- c(balloonList$max.pop, temp.list$max.pop)
        balloonList$balloonIds <- c(balloonList$balloonIds,
                                    temp.list$balloonIds)
      }

      balloonList$nBalloons <- sum(balloonList$nBalloons)

    } else {

      balloonList <- readRDS(fileName)
    }

    if (isTRUE(randomize)){

      reInd <- sample(length(balloonList$PopVals))

      balloonList$PopVals <- balloonList$PopVals[reInd]

      balloonList$balloonColors <- balloonList$balloonColors[reInd]

      balloonList$balloonIds <- balloonList$balloonIds[reInd]

    }

  } else {

    PopVals <- unlist(lapply(seq_len(distList$diffBalloons), .getPopVals,
                      distributionList = distList))

    if (any(distList$balloonColor == "red" || distList$balloonColor == "green")) {
      warning("Your distList$balloonColor contained \"red\" or \"green\". These colors will already be used when the balloons are   saved or pop. Thus you might want to change your color values.")
    }

    balloonColors <- rep(distList$balloonColor, distList$nBalloons)
    balloonIds <- rep(distList$balloonIds, distList$nBalloons)

    if (isTRUE(randomize)){

      reInd <- sample(length(PopVals))

      PopVals <- PopVals[reInd]

      balloonColors <- balloonColors[reInd]

      balloonIds <- balloonIds[reInd]

    }

    balloonList <- list("PopVals" = PopVals,
                        "balloonColors" = balloonColors,
                        "nBalloons" = sum(distList$nBalloons),
                        "max.pop" = distList$max.pop,
                        "balloonIds" = balloonIds)
  }

  balloonList

}

