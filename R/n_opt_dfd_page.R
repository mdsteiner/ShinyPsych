#' Create an HTML Page to Display and Play a Decisions From Description Task
#'
#' Create an html page and send information to javascript if an option is
#' clicked. Needs prepared lists of gambles created by
#' \code{\link{createDfdList}} or of the same structure and control variables
#' created with \code{\link{createCtrlList}}.
#'
#' These functions are called from within \code{\link{multiOptsDfdPage}}
#' because this also handles the appropriate custom message. So when you need
#' this function just call \code{\link{multiOptsDfdPage}}.
#' @param ctrlValsList list of reactive values. Controls the flow through the
#'  Experiment. Can be created with \code{\link{createCtrlList}}.
#' @param containerOb list. Contains the gambles. Can be created by calling
#'  \code{\link{multiOptsDfdPage}}.
#' @param gambleTitle string. Is displayed as title of the trial, e.g. if it is
#'  set to "Gamble" the displayed title in game 2 of 10  will be "Gamble 2 of
#'  10".
#' @param gambleTitlePractice string. Is displayed as title of the practice
#'  gamble.
#' @param withPracticeGamble logical. If TRUE (default) a practice
#'  gamble must be provided and is played. Only needed if displayTitle is TRUE.
#' @param displayTitle logical. If TRUE (default) a title is displayed.
#' @param nGambles integer. The number of gambles played WITHOUT the practice
#'  game.
#'
#' @return An html page that can be displayed by a shiny app.
#' @export
twoOptDfdPage <- function(ctrlValsList, containerOb,
                          gambleTitle = "Gamble",
                          gambleTitlePractice = "Practice Gamble",
                          withPracticeGamble = TRUE, displayTitle = TRUE,
                          nGambles = 10){

  gInd <- ctrlValsList$dfdGamble

  if (isTRUE(withPracticeGamble)){

    list(
      shiny::fixedRow(

        # call the neGame() javascript function to prepare objects
        shiny::tag("script", paste0('newDfdGame(', gInd, ');')),

        # set up written info displayed above the boxes
        shiny::column(12,
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::column(12, align = "left",
                                                    shiny::h2(
                                                      ifelse(gInd == 1,
                                                             gambleTitlePractice,
                                                             paste(gambleTitle,
                                                                   gInd - 1,
                                                                   "of", nGambles))))),
                      shiny::fixedRow(shiny::br()),

                      # set up logic to update the boxes via javascript. Arguments passed to js come from
                      # newGame() js function and the sendCustomMessage called just before this function
                      shiny::fixedRow(
                        shiny::column(1, align="center",
                                      shiny::HTML('<h1 id="emptySpace1" class="emptySpace">Place</h1>')),
                        shiny::column(5, align="center",
                                      shiny::HTML(paste(
                                        '<div class = "box"><div class = "gambles" id="gamble1"
                                        onclick="onGambleClick(\'gamble1\', 1, t, respTime, clickEnabled,
                                        gameNr)"><table>', containerOb$outcomes[[gInd]][[1]],
                                        "</table></div></div>"))),
                        shiny::column(5, align="center",
                                      shiny::HTML(paste(
                                        '<div class = "box"><div class = "gambles" id="gamble2"
                                        onclick="onGambleClick(\'gamble2\', 2, t, respTime, clickEnabled,
                                        gameNr)"><table>', containerOb$outcomes[[gInd]][[2]],
                                        "</table></div></div>"))),
                        shiny::column(1, align="center",
                                      shiny::HTML('<h1 id="emptySpace2" class="emptySpace">Place</h1>'))))))
  } else if (isTRUE(displayTitle)) {
    list(
      shiny::fixedRow(

        # call the neGame() javascript function to prepare objects
        shiny::tag("script", paste0('newDfdGame(', gInd, ');')),

        # set up written info displayed above the boxes
        shiny::column(12,
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::column(12, align = "left",
                                                    shiny::h2(
                                                      paste(gambleTitle,
                                                            gInd - 1,
                                                            "of", nGambles)))),
                      shiny::fixedRow(shiny::br()),

                      # set up logic to update the boxes via javascript. Arguments passed to js come from
                      # newGame() js function and the sendCustomMessage called just before this function
                      shiny::fixedRow(
                        shiny::column(1, align="center",
                                      shiny::HTML('<h1 id="emptySpace1" class="emptySpace">Place</h1>')),
                        shiny::column(5, align="center",
                                      shiny::HTML(paste(
                                        '<div class = "box"><div class = "gambles" id="gamble1"
                                        onclick="onGambleClick(\'gamble1\', 1, t, respTime, clickEnabled,
                                        gameNr)"><table>', containerOb$outcomes[[gInd]][[1]],
                                        "</table></div></div>"))),
                        shiny::column(5, align="center",
                                      shiny::HTML(paste(
                                        '<div class = "box"><div class = "gambles" id="gamble2"
                                        onclick="onGambleClick(\'gamble2\', 2, t, respTime, clickEnabled,
                                        gameNr)"><table>', containerOb$outcomes[[gInd]][[2]],
                                        "</table></div></div>"))),
                        shiny::column(1, align="center",
                                      shiny::HTML('<h1 id="emptySpace2" class="emptySpace">Place</h1>'))))))
  } else {
    list(
      shiny::fixedRow(

        # call the neGame() javascript function to prepare objects
        shiny::tag("script", paste0('newDfdGame(', gInd, ');')),

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
                                        '<div class = "box"><div class = "gambles" id="gamble1"
                                        onclick="onGambleClick(\'gamble1\', 1, t, respTime, clickEnabled,
                                        gameNr)"><table>', containerOb$outcomes[[gInd]][[1]],
                                        "</table></div></div>"))),
                        shiny::column(5, align="center",
                                      shiny::HTML(paste(
                                        '<div class = "box"><div class = "gambles" id="gamble2"
                                        onclick="onGambleClick(\'gamble2\', 2, t, respTime, clickEnabled,
                                        gameNr)"><table>', containerOb$outcomes[[gInd]][[2]],
                                        "</table></div></div>"))),
                        shiny::column(1, align="center",
                                      shiny::HTML('<h1 id="emptySpace2" class="emptySpace">Place</h1>'))))))
  }

}

# threeOptDfePage =====================================

#' @describeIn twoOptDfdPage A three option gamble version
#'
threeOptDfdPage <- function(ctrlValsList, containerOb,
                            gambleTitle = "Gamble",
                            gambleTitlePractice = "Practice Gamble",
                            withPracticeGamble = TRUE, displayTitle = TRUE,
                            nGambles = 10){

  gInd <- ctrlValsList$dfdGamble

  if (isTRUE(withPracticeGamble)){

    list(
      shiny::fixedRow(

        # call the neGame() javascript function to prepare objects
        shiny::tag("script", paste0('newDfdGame(', gInd, ');')),

        # set up written info displayed above the boxes
        shiny::column(12,
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::column(12, align = "left",
                                                    shiny::h2(
                                                      ifelse(gInd == 1,
                                                             gambleTitlePractice,
                                                             paste(gambleTitle,
                                                                   gInd - 1,
                                                                   "of", nGambles))))),
                      shiny::fixedRow(shiny::br()),

                      # set up logic to update the boxes via javascript. Arguments passed to js come from
                      # newGame() js function and the sendCustomMessage called just before this function
                      shiny::fixedRow(
                        shiny::column(4, align="center",
                                      shiny::HTML(paste(
                                        '<div class = "box"><div class = "gambles" id="gamble1"
                                        onclick="onGambleClick(\'gamble1\', 1, t, respTime, clickEnabled,
                                        gameNr)"><table>', containerOb$outcomes[[gInd]][[1]],
                                        "</table></div></div>"))),
                        shiny::column(4, align="center",
                                      shiny::HTML(paste(
                                        '<div class = "box"><div class = "gambles" id="gamble2"
                                        onclick="onGambleClick(\'gamble2\', 2, t, respTime, clickEnabled,
                                        gameNr)"><table>', containerOb$outcomes[[gInd]][[2]],
                                        "</table></div></div>"))),
                        shiny::column(4, align="center",
                                      shiny::HTML(paste(
                                        '<div class = "box"><div class = "gambles" id="gamble3"
                                        onclick="onGambleClick(\'gamble3\', 3, t, respTime, clickEnabled,
                                        gameNr)"><table>', containerOb$outcomes[[gInd]][[3]],
                                        "</table></div></div>")))
    ))))
  } else if (isTRUE(displayTitle)) {
    list(
      shiny::fixedRow(

        # call the neGame() javascript function to prepare objects
        shiny::tag("script", paste0('newDfdGame(', gInd, ');')),

        # set up written info displayed above the boxes
        shiny::column(12,
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::column(12, align = "left",
                                                    shiny::h2(
                                                      paste(gambleTitle,
                                                            gInd - 1,
                                                            "of", nGambles)))),
                      shiny::fixedRow(shiny::br()),

                      # set up logic to update the boxes via javascript. Arguments passed to js come from
                      # newGame() js function and the sendCustomMessage called just before this function
                      shiny::fixedRow(
                        shiny::column(4, align="center",
                                      shiny::HTML(paste(
                                        '<div class = "box"><div class = "gambles" id="gamble1"
                                        onclick="onGambleClick(\'gamble1\', 1, t, respTime, clickEnabled,
                                        gameNr)"><table>', containerOb$outcomes[[gInd]][[1]],
                                        "</table></div></div>"))),
                        shiny::column(4, align="center",
                                      shiny::HTML(paste(
                                        '<div class = "box"><div class = "gambles" id="gamble2"
                                        onclick="onGambleClick(\'gamble2\', 2, t, respTime, clickEnabled,
                                        gameNr)"><table>', containerOb$outcomes[[gInd]][[2]],
                                        "</table></div></div>"))),
                        shiny::column(4, align="center",
                                      shiny::HTML(paste(
                                        '<div class = "box"><div class = "gambles" id="gamble3"
                                        onclick="onGambleClick(\'gamble3\', 3, t, respTime, clickEnabled,
                                        gameNr)"><table>', containerOb$outcomes[[gInd]][[3]],
                                        "</table></div></div>")))
    ))))
  } else {
    list(
      shiny::fixedRow(

        # call the neGame() javascript function to prepare objects
        shiny::tag("script", paste0('newDfdGame(', gInd, ');')),

        # set up written info displayed above the boxes
        shiny::column(12,
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),

                      # set up logic to update the boxes via javascript. Arguments passed to js come from
                      # newGame() js function and the sendCustomMessage called just before this function
                      shiny::fixedRow(
                        shiny::column(4, align="center",
                                      shiny::HTML(paste(
                                        '<div class = "box"><div class = "gambles" id="gamble1"
                                        onclick="onGambleClick(\'gamble1\', 1, t, respTime, clickEnabled,
                                        gameNr)"><table>', containerOb$outcomes[[gInd]][[1]],
                                        "</table></div></div>"))),
                        shiny::column(4, align="center",
                                      shiny::HTML(paste(
                                        '<div class = "box"><div class = "gambles" id="gamble2"
                                        onclick="onGambleClick(\'gamble2\', 2, t, respTime, clickEnabled,
                                        gameNr)"><table>', containerOb$outcomes[[gInd]][[2]],
                                        "</table></div></div>"))),
                        shiny::column(4, align="center",
                                      shiny::HTML(paste(
                                        '<div class = "box"><div class = "gambles" id="gamble3"
                                        onclick="onGambleClick(\'gamble3\', 3, t, respTime, clickEnabled,
                                        gameNr)"><table>', containerOb$outcomes[[gInd]][[3]],
                                        "</table></div></div>")))
                      ))))
  }

}


# fourOptDfePage =====================================

#' @describeIn twoOptDfdPage A four option gamble version
fourOptDfdPage <- function(ctrlValsList, containerOb,
                           gambleTitle = "Gamble",
                           gambleTitlePractice = "Practice Gamble",
                           withPracticeGamble = TRUE, displayTitle = TRUE,
                           nGambles = 10){

  gInd <- ctrlValsList$dfdGamble

  if (isTRUE(withPracticeGamble)){

    list(
      shiny::fixedRow(

        # call the neGame() javascript function to prepare objects
        shiny::tag("script", paste0('newDfdGame(', gInd, ');')),

        # set up written info displayed above the boxes
        shiny::column(12,
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::column(12, align = "left",
                                                    shiny::h2(
                                                      ifelse(gInd == 1,
                                                             gambleTitlePractice,
                                                             paste(gambleTitle,
                                                                   gInd - 1,
                                                                   "of", nGambles))))),
                      shiny::fixedRow(shiny::br()),

                      # set up logic to update the boxes via javascript. Arguments passed to js come from
                      # newGame() js function and the sendCustomMessage called just before this function
                      shiny::fixedRow(
                        shiny::column(3, align="center",
                                      shiny::HTML(paste(
                                        '<div class = "box"><div class = "gambles" id="gamble1"
                                        onclick="onGambleClick(\'gamble1\', 1, t, respTime, clickEnabled,
                                        gameNr)"><table>', containerOb$outcomes[[gInd]][[1]],
                                        "</table></div></div>"))),
                        shiny::column(3, align="center",
                                      shiny::HTML(paste(
                                        '<div class = "box"><div class = "gambles" id="gamble2"
                                        onclick="onGambleClick(\'gamble2\', 2, t, respTime, clickEnabled,
                                        gameNr)"><table>', containerOb$outcomes[[gInd]][[2]],
                                        "</table></div></div>"))),
                        shiny::column(3, align="center",
                                      shiny::HTML(paste(
                                        '<div class = "box"><div class = "gambles" id="gamble3"
                                        onclick="onGambleClick(\'gamble3\', 3, t, respTime, clickEnabled,
                                        gameNr)"><table>', containerOb$outcomes[[gInd]][[3]],
                                        "</table></div></div>"))),
                        shiny::column(3, align="center",
                                      shiny::HTML(paste(
                                        '<div class = "box"><div class = "gambles" id="gamble4"
                                        onclick="onGambleClick(\'gamble4\', 4, t, respTime, clickEnabled,
                                        gameNr)"><table>', containerOb$outcomes[[gInd]][[4]],
                                        "</table></div></div>")))
                        ))))
  } else if (isTRUE(displayTitle)) {
    list(
      shiny::fixedRow(

        # call the neGame() javascript function to prepare objects
        shiny::tag("script", paste0('newDfdGame(', gInd, ');')),

        # set up written info displayed above the boxes
        shiny::column(12,
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::column(12, align = "left",
                                                    shiny::h2(
                                                      paste(gambleTitle,
                                                            gInd - 1,
                                                            "of", nGambles)))),
                      shiny::fixedRow(shiny::br()),

                      # set up logic to update the boxes via javascript. Arguments passed to js come from
                      # newGame() js function and the sendCustomMessage called just before this function
                      shiny::fixedRow(
                        shiny::column(3, align="center",
                                      shiny::HTML(paste(
                                        '<div class = "box"><div class = "gambles" id="gamble1"
                                        onclick="onGambleClick(\'gamble1\', 1, t, respTime, clickEnabled,
                                        gameNr)"><table>', containerOb$outcomes[[gInd]][[1]],
                                        "</table></div></div>"))),
                        shiny::column(3, align="center",
                                      shiny::HTML(paste(
                                        '<div class = "box"><div class = "gambles" id="gamble2"
                                        onclick="onGambleClick(\'gamble2\', 2, t, respTime, clickEnabled,
                                        gameNr)"><table>', containerOb$outcomes[[gInd]][[2]],
                                        "</table></div></div>"))),
                        shiny::column(3, align="center",
                                      shiny::HTML(paste(
                                        '<div class = "box"><div class = "gambles" id="gamble3"
                                        onclick="onGambleClick(\'gamble3\', 3, t, respTime, clickEnabled,
                                        gameNr)"><table>', containerOb$outcomes[[gInd]][[3]],
                                        "</table></div></div>"))),
                        shiny::column(3, align="center",
                                      shiny::HTML(paste(
                                        '<div class = "box"><div class = "gambles" id="gamble4"
                                        onclick="onGambleClick(\'gamble4\', 4, t, respTime, clickEnabled,
                                        gameNr)"><table>', containerOb$outcomes[[gInd]][[4]],
                                        "</table></div></div>")))
                                      ))))
  } else {
    list(
      shiny::fixedRow(

        # call the neGame() javascript function to prepare objects
        shiny::tag("script", paste0('newDfdGame(', gInd, ');')),

        # set up written info displayed above the boxes
        shiny::column(12,
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),

                      # set up logic to update the boxes via javascript. Arguments passed to js come from
                      # newGame() js function and the sendCustomMessage called just before this function
                      shiny::fixedRow(
                        shiny::column(3, align="center",
                                      shiny::HTML(paste(
                                        '<div class = "box"><div class = "gambles" id="gamble1"
                                        onclick="onGambleClick(\'gamble1\', 1, t, respTime, clickEnabled,
                                        gameNr)"><table>', containerOb$outcomes[[gInd]][[1]],
                                        "</table></div></div>"))),
                        shiny::column(3, align="center",
                                      shiny::HTML(paste(
                                        '<div class = "box"><div class = "gambles" id="gamble2"
                                        onclick="onGambleClick(\'gamble2\', 2, t, respTime, clickEnabled,
                                        gameNr)"><table>', containerOb$outcomes[[gInd]][[2]],
                                        "</table></div></div>"))),
                        shiny::column(3, align="center",
                                      shiny::HTML(paste(
                                        '<div class = "box"><div class = "gambles" id="gamble3"
                                        onclick="onGambleClick(\'gamble3\', 3, t, respTime, clickEnabled,
                                        gameNr)"><table>', containerOb$outcomes[[gInd]][[3]],
                                        "</table></div></div>"))),
                        shiny::column(3, align="center",
                                      shiny::HTML(paste(
                                        '<div class = "box"><div class = "gambles" id="gamble4"
                                        onclick="onGambleClick(\'gamble4\', 4, t, respTime, clickEnabled,
                                        gameNr)"><table>', containerOb$outcomes[[gInd]][[4]],
                                        "</table></div></div>")))
                                      ))))
  }

}

