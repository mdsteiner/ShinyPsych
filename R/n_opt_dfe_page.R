#' Create an HTML Page to Display and Play a Decisions From Experience Task
#'
#' Create an html page and send information to javascript if an option is
#' clicked. Needs prepared lists of outcomes created by
#' \code{\link{createDfeList}} or of the same structure and control variables
#' created with \code{\link{createCtrlList}}.
#'
#' These functions are called from within \code{\link{multiOptsDfePage}}
#' because this also handles the appropriate custom message with.
#' @param ctrlValsList list of reactive values. Controls the flow through the
#'  Experiment. Can be created with \code{\link{createCtrlList}}.
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
#' @param buttonLabel string. Is displayed in the continue button. Default is
#'  set to "Make Decision".
#'
#' @return An html page that can be displayed by a shiny appl
#' @export
twoOptDfePage <- function(ctrlValsList, gambleTitle = "Gamble",
                          gambleTitlePractice = "Practice Gamble",
                          withPracticeGamble = TRUE, displayTitle = TRUE,
                          nGambles = 10, buttonLabel = "Make Decision"){

  if (isTRUE(withPracticeGamble)){

    list(
      shiny::fixedRow(

        # call the neGame() javascript function to prepare objects
        shiny::tag("script", 'newDfeGame();'),

        # set up written info displayed above the boxes
        shiny::column(12,
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::column(12, align = "left",
                                                    shiny::h2(ifelse(ctrlValsList$dfeGamble == 1,
                                                                     gambleTitlePractice,
                                                                     paste(gambleTitle,
                                                                           ctrlValsList$dfeGamble - 1,
                                                                           "of", nGambles))))),
                      shiny::fixedRow(shiny::br()),

                      # set up logic to update the boxes via javascript. Arguments passed to js come from newGame() js function and
                      # the sendCustomMessage called just before this function
                      shiny::fixedRow(
                        shiny::column(1, align="center",
                                      shiny::HTML('<h1 id="emptySpace1" class="emptySpace">Place</h1>')),
                        shiny::column(5, align="center",
                                      shiny::HTML('<h1 id="deck1" class="decks" onclick="updateDfeTwoOpts(\'deck1\',
                                                  \'deck2\', enOne, enTwo, ind, 1, outcome, samples, gambleNr,
                                                  respTime, trial, t, clickEnabled, signalColors, makeDecision,
                                                  selected, finalOutcome)"> </h1>')),
                        shiny::column(5, align="center",
                                      shiny::HTML('<h1 id="deck2" class="decks" onclick="updateDfeTwoOpts(\'deck2\',
                                                  \'deck1\', enTwo, enOne, ind, 2, outcome, samples, gambleNr,
                                                  respTime, trial, t, clickEnabled, signalColors, makeDecision,
                                                  selected, finalOutcome)"> </h1>')),
                        shiny::column(1, align="center",
                                      shiny::HTML('<h1 id="emptySpace2" class="emptySpace">Place</h1>'))),

                      # set up the continue Button
                      shiny::fixedRow(
                        shiny::column(6,
                                      shinyjs::disabled(shiny::actionButton("decideDfe", label = buttonLabel,
                                                                            style =  "margin-top: 0.2em;
                                                                            margin-left: 8.5em;
                                                                            margin-bottom: 1.5em",
                                                                            onclick =
                                                                              "enableDecisionTwoOpt(makeDecision,
                                                                            \"deck1\", \"deck2\");")),
                                      offset =  4))))
        )
  } else if (isTRUE(displayTitle)) {
    list(
      shiny::fixedRow(

        # call the neGame() javascript function to prepare objects
        shiny::tag("script", 'newDfeGame();'),

        # set up written info displayed above the boxes
        shiny::column(12,
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::column(12, align = "left",
                                                    shiny::h2(paste(gambleTitle,
                                                                    ctrlValsList$dfeGamble - 1,
                                                                    "of ",
                                                                    nGambles)))),
                      shiny::fixedRow(shiny::br()),

                      # set up logic to update the boxes via javascript. Arguments passed to js come from newGame() js function and
                      # the sendCustomMessage called just before this function
                      shiny::fixedRow(
                        shiny::column(1, align="center",
                                      shiny::HTML('<h1 id="emptySpace1" class="emptySpace">Place</h1>')),
                        shiny::column(5, align="center",
                                      shiny::HTML('<h1 id="deck1" class="decks" onclick="updateDfeTwoOpts(\'deck1\',
                                                  \'deck2\', enOne, enTwo, ind, 1, outcome, samples, gambleNr,
                                                  respTime, trial, t, clickEnabled, signalColors, makeDecision,
                                                  selected, finalOutcome)"> </h1>')),
                        shiny::column(5, align="center",
                                      shiny::HTML('<h1 id="deck2" class="decks" onclick="updateDfeTwoOpts(\'deck2\',
                                                  \'deck1\', enTwo, enOne, ind, 2, outcome, samples, gambleNr,
                                                  respTime, trial, t, clickEnabled, signalColors, makeDecision,
                                                  selected, finalOutcome)"> </h1>')),
                        shiny::column(1, align="center",
                                      shiny::HTML('<h1 id="emptySpace2" class="emptySpace">Place</h1>'))),

                      # set up the continue Button
                      shiny::fixedRow(
                        shiny::column(6,
                                      shinyjs::disabled(shiny::actionButton("decideDfe",
                                                                            label = buttonLabel,
                                                                            style =  "margin-top: 0.2em;
                                                                            margin-left: 8.5em;
                                                                            margin-bottom: 1.5em",
                                                                            onclick =
                                                                              "enableDecisionTwoOpt(makeDecision,
                                                                            \"deck1\", \"deck2\");")),
                                      offset =  4))))
        )
  } else {
    list(
      shiny::fixedRow(

        # call the neGame() javascript function to prepare objects
        shiny::tag("script", 'newDfeGame();'),

        # set up written info displayed above the boxes
        shiny::column(12,
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),

                      # set up logic to update the boxes via javascript. Arguments passed to js come from newGame() js function and
                      # the sendCustomMessage called just before this function
                      shiny::fixedRow(
                        shiny::column(1, align="center",
                                      shiny::HTML('<h1 id="emptySpace1" class="emptySpace">Place</h1>')),
                        shiny::column(5, align="center",
                                      shiny::HTML('<h1 id="deck1" class="decks" onclick="updateDfeTwoOpts(\'deck1\',
                                                  \'deck2\', enOne, enTwo, ind, 1, outcome, samples, gambleNr,
                                                  respTime, trial, t, clickEnabled, signalColors, makeDecision,
                                                  selected, finalOutcome)"> </h1>')),
                        shiny::column(5, align="center",
                                      shiny::HTML('<h1 id="deck2" class="decks" onclick="updateDfeTwoOpts(\'deck2\',
                                                  \'deck1\', enTwo, enOne, ind, 2, outcome, samples, gambleNr,
                                                  respTime, trial, t, clickEnabled, signalColors, makeDecision,
                                                  selected, finalOutcome)"> </h1>')),
                        shiny::column(1, align="center",
                                      shiny::HTML('<h1 id="emptySpace2" class="emptySpace">Place</h1>'))),

                      # set up the continue Button
                      shiny::fixedRow(
                        shiny::column(6,
                                      shinyjs::disabled(shiny::actionButton("decideDfe",
                                                                            label = buttonLabel,
                                                                            style =  "margin-top: 0.2em;
                                                                            margin-left: 8.5em;
                                                                            margin-bottom: 1.5em",
                                                                            onclick =
                                                                              "enableDecisionTwoOpt(makeDecision,
                                                                            \"deck1\", \"deck2\");")),
                                      offset =  4))))
        )
  }

}


# threeOptDfePage =====================================

#' @describeIn twoOptDfePage A three option gamble version
threeOptDfePage <- function(ctrlValsList, gambleTitle = "Gamble",
                            gambleTitlePractice = "Practice Gamble",
                            withPracticeGamble = TRUE, displayTitle = TRUE,
                            nGambles = 10, buttonLabel = "Make Decision"){

  if (isTRUE(withPracticeGamble)){

    list(
      shiny::fixedRow(

        # call the neGame() javascript function to prepare objects
        shiny::tag("script", 'newDfeGame();'),

        # set up written info displayed above the boxes
        shiny::column(12,
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::column(12, align = "left",
                                                    shiny::h2(ifelse(ctrlValsList$dfeGamble == 1,
                                                                     gambleTitlePractice,
                                                                     paste(gambleTitle,
                                                                           ctrlValsList$dfeGamble - 1,
                                                                           "of", nGambles))))),
                      shiny::fixedRow(shiny::br()),

                      # set up logic to update the boxes via javascript. Arguments passed to js come from newGame() js function and
                      # the sendCustomMessage called just before this function
                      shiny::fixedRow(
                        shiny::column(4, align="center",
                                      shiny::HTML('<h1 id="deck1" class="decks" onclick="updateDfeThreeOpts(\'deck1\',
                                                  \'deck2\', \'deck3\', enOne, enTwo, enThree, ind, 1, outcome,
                                                  samples, gambleNr, respTime, trial, t, clickEnabled, signalColors,
                                                  makeDecision, selected, finalOutcome)"> </h1>')),
                        shiny::column(4, align="center",
                                      shiny::HTML('<h1 id="deck2" class="decks" onclick="updateDfeThreeOpts(\'deck2\',
                                                  \'deck1\', \'deck3\', enTwo, enOne, enThree, ind, 2, outcome, samples,
                                                  gambleNr, respTime, trial, t, clickEnabled, signalColors,
                                                  makeDecision, selected, finalOutcome)"> </h1>')),
                        shiny::column(4, align="center",
                                      shiny::HTML('<h1 id="deck3" class="decks" onclick="updateDfeThreeOpts(\'deck3\',
                                                  \'deck2\', \'deck1\', enThree, enTwo, enOne, ind, 3, outcome, samples,
                                                  gambleNr, respTime, trial, t, clickEnabled, signalColors,
                                                  makeDecision, selected, finalOutcome)"> </h1>'))),

                      # set up the continue Button
                      shiny::fixedRow(
                        shiny::column(6,
                                      shinyjs::disabled(shiny::actionButton("decideDfe", label = buttonLabel,
                                                                            style =  "margin-top: 1.2em;
                                                                            margin-left: 8.5em;
                                                                            margin-bottom: 1.5em",
                                                                            onclick =
                                                                              "enableDecisionThreeOpt(makeDecision,
                                                                            \"deck1\",
                                                                            \"deck2\",
                                                                            \"deck3\");")),
                                      offset =  4))))
                        )
  } else if (isTRUE(displayTitle)) {
    list(
      shiny::fixedRow(

        # call the neGame() javascript function to prepare objects
        shiny::tag("script", 'newDfeGame();'),

        # set up written info displayed above the boxes
        shiny::column(12,
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::column(12, align = "left",
                                                    shiny::h2(paste(gambleTitle,
                                                                    ctrlValsList$dfeGamble - 1,
                                                                    "of ",
                                                                    nGambles)))),
                      shiny::fixedRow(shiny::br()),

                      # set up logic to update the boxes via javascript. Arguments passed to js come from newGame() js function and
                      # the sendCustomMessage called just before this function
                      shiny::fixedRow(
                        shiny::column(4, align="center",
                                      shiny::HTML('<h1 id="deck1" class="decks" onclick="updateDfeThreeOpts(\'deck1\',
                                                  \'deck2\', \'deck3\', enOne, enTwo, enThree, ind, 1, outcome,
                                                  samples, gambleNr, respTime, trial, t, clickEnabled, signalColors,
                                                  makeDecision, selected, finalOutcome)"> </h1>')),
                        shiny::column(4, align="center",
                                      shiny::HTML('<h1 id="deck2" class="decks" onclick="updateDfeThreeOpts(\'deck2\',
                                                  \'deck1\', \'deck3\', enTwo, enOne, enThree, ind, 2, outcome, samples,
                                                  gambleNr, respTime, trial, t, clickEnabled, signalColors,
                                                  makeDecision, selected, finalOutcome)"> </h1>')),
                        shiny::column(4, align="center",
                                      shiny::HTML('<h1 id="deck3" class="decks" onclick="updateDfeThreeOpts(\'deck3\',
                                                  \'deck2\', \'deck1\', enThree, enTwo, enOne, ind, 3, outcome, samples,
                                                  gambleNr, respTime, trial, t, clickEnabled, signalColors,
                                                  makeDecision, selected, finalOutcome)"> </h1>'))),

                      # set up the continue Button
                      shiny::fixedRow(
                        shiny::column(6,
                                      shinyjs::disabled(shiny::actionButton("decideDfe",
                                                                            label = buttonLabel,
                                                                            style =  "margin-top: 1.2em;
                                                                            margin-left: 8.5em;
                                                                            margin-bottom: 1.5em",
                                                                            onclick =
                                                                              "enableDecisionThreeOpt(makeDecision,
                                                                            \"deck1\",
                                                                            \"deck2\",
                                                                            \"deck3\");")),
                                      offset =  4))))
                        )
  } else {
    list(
      shiny::fixedRow(

        # call the neGame() javascript function to prepare objects
        shiny::tag("script", 'newDfeGame();'),

        # set up written info displayed above the boxes
        shiny::column(12,
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),

                      # set up logic to update the boxes via javascript. Arguments passed to js come from newGame() js function and
                      # the sendCustomMessage called just before this function
                      shiny::fixedRow(
                        shiny::column(4, align="center",
                                      shiny::HTML('<h1 id="deck1" class="decks" onclick="updateDfeThreeOpts(\'deck1\',
                                                  \'deck2\', \'deck3\', enOne, enTwo, enThree, ind, 1, outcome,
                                                  samples, gambleNr, respTime, trial, t, clickEnabled, signalColors,
                                                  makeDecision, selected, finalOutcome)"> </h1>')),
                        shiny::column(4, align="center",
                                      shiny::HTML('<h1 id="deck2" class="decks" onclick="updateDfeThreeOpts(\'deck2\',
                                                  \'deck1\', \'deck3\', enTwo, enOne, enThree, ind, 2, outcome, samples,
                                                  gambleNr, respTime, trial, t, clickEnabled, signalColors,
                                                  makeDecision, selected, finalOutcome)"> </h1>')),
                        shiny::column(4, align="center",
                                      shiny::HTML('<h1 id="deck3" class="decks" onclick="updateDfeThreeOpts(\'deck3\',
                                                  \'deck2\', \'deck1\', enThree, enTwo, enOne, ind, 3, outcome, samples,
                                                  gambleNr, respTime, trial, t, clickEnabled, signalColors,
                                                  makeDecision, selected, finalOutcome)"> </h1>'))),

                      # set up the continue Button
                      shiny::fixedRow(
                        shiny::column(6,
                                      shinyjs::disabled(shiny::actionButton("decideDfe",
                                                                            label = buttonLabel,
                                                                            style =  "margin-top: 1.2em;
                                                                            margin-left: 8.5em;
                                                                            margin-bottom: 1.5em",
                                                                            onclick =
                                                                              "enableDecisionThreeOpt(makeDecision,
                                                                            \"deck1\",
                                                                            \"deck2\",
                                                                            \"deck3\");")),
                                      offset =  4))))
                        )
  }

}

# fourOptDfePage =====================================

#' @describeIn twoOptDfePage A four option gamble version
fourOptDfePage <- function(ctrlValsList, gambleTitle = "Gamble",
                           gambleTitlePractice = "Practice Gamble",
                           withPracticeGamble = TRUE, displayTitle = TRUE,
                           nGambles = 10, buttonLabel = "Make Decision"){

  if (isTRUE(withPracticeGamble)){

    list(
      shiny::fixedRow(

        # call the neGame() javascript function to prepare objects
        shiny::tag("script", 'newDfeGame();'),

        # set up written info displayed above the boxes
        shiny::column(12,
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::column(12, align = "left",
                                                    shiny::h2(ifelse(ctrlValsList$dfeGamble == 1,
                                                                     gambleTitlePractice,
                                                                     paste(gambleTitle,
                                                                           ctrlValsList$dfeGamble - 1,
                                                                           "of", nGambles))))),
                      shiny::fixedRow(shiny::br()),

                      # set up logic to update the boxes via javascript. Arguments passed to js come from newGame() js
                      # function and the sendCustomMessage called just before this function
                      shiny::fixedRow(
                        shiny::column(3, align="center",
                                      shiny::HTML('<h1 id="deck1" class="decks" onclick="updateDfeFourOpts(\'deck1\',
                                                  \'deck2\', \'deck3\', \'deck4\', enOne, enTwo, enThree, enFour, ind,
                                                  1, outcome, samples, gambleNr, respTime, trial, t, clickEnabled,
                                                  signalColors, makeDecision, selected, finalOutcome)"> </h1>')),
                        shiny::column(3, align="center",
                                      shiny::HTML('<h1 id="deck2" class="decks" onclick="updateDfeFourOpts(\'deck2\',
                                                  \'deck1\', \'deck3\', \'deck4\', enTwo, enOne, enThree, enFour, ind,
                                                  2, outcome, samples, gambleNr, respTime, trial, t, clickEnabled,
                                                  signalColors, makeDecision, selected, finalOutcome)"> </h1>')),
                        shiny::column(3, align="center",
                                      shiny::HTML('<h1 id="deck3" class="decks" onclick="updateDfeFourOpts(\'deck3\',
                                                  \'deck2\', \'deck1\', \'deck4\', enThree, enTwo, enOne, enFour, ind,
                                                  3, outcome, samples, gambleNr, respTime, trial, t, clickEnabled,
                                                  signalColors, makeDecision, selected, finalOutcome)"> </h1>')),
                        shiny::column(3, align="center",
                                      shiny::HTML('<h1 id="deck4" class="decks" onclick="updateDfeFourOpts(\'deck4\',
                                                  \'deck2\', \'deck1\', \'deck3\', enFour, enThree, enTwo, enOne, ind,
                                                  4, outcome, samples, gambleNr, respTime, trial, t, clickEnabled,
                                                  signalColors, makeDecision, selected, finalOutcome)"> </h1>'))),

                      # set up the continue Button
                      shiny::fixedRow(
                        shiny::column(6,
                                      shinyjs::disabled(shiny::actionButton("decideDfe", label = buttonLabel,
                                                                            style =  "margin-top: 1.2em;
                                                                            margin-left: 8.5em;
                                                                            margin-bottom: 1.5em",
                                                                            onclick =
                                                                              "enableDecisionFourOpt(makeDecision,
                                                                            \"deck1\",
                                                                            \"deck2\",
                                                                            \"deck3\",
                                                                            \"deck4\");")),
                                      offset =  4))))
                        )
  } else if (isTRUE(displayTitle)) {
    list(
      shiny::fixedRow(

        # call the neGame() javascript function to prepare objects
        shiny::tag("script", 'newDfeGame();'),

        # set up written info displayed above the boxes
        shiny::column(12,
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::column(12, align = "left",
                                                    shiny::h2(paste(gambleTitle,
                                                                    ctrlValsList$dfeGamble - 1,
                                                                    "of ",
                                                                    nGambles)))),
                      shiny::fixedRow(shiny::br()),

                      # set up logic to update the boxes via javascript. Arguments passed to js come from newGame() js
                      # function and the sendCustomMessage called just before this function
                      shiny::fixedRow(
                        shiny::column(3, align="center",
                                      shiny::HTML('<h1 id="deck1" class="decks" onclick="updateDfeFourOpts(\'deck1\',
                                                  \'deck2\', \'deck3\', \'deck4\', enOne, enTwo, enThree, enFour, ind,
                                                  1, outcome, samples, gambleNr, respTime, trial, t, clickEnabled,
                                                  signalColors, makeDecision, selected, finalOutcome)"> </h1>')),
                        shiny::column(3, align="center",
                                      shiny::HTML('<h1 id="deck2" class="decks" onclick="updateDfeFourOpts(\'deck2\',
                                                  \'deck1\', \'deck3\', \'deck4\', enTwo, enOne, enThree, enFour, ind,
                                                  2, outcome, samples, gambleNr, respTime, trial, t, clickEnabled,
                                                  signalColors, makeDecision, selected, finalOutcome)"> </h1>')),
                        shiny::column(3, align="center",
                                      shiny::HTML('<h1 id="deck3" class="decks" onclick="updateDfeFourOpts(\'deck3\',
                                                  \'deck2\', \'deck1\', \'deck4\', enThree, enTwo, enOne, enFour, ind,
                                                  3, outcome, samples, gambleNr, respTime, trial, t, clickEnabled,
                                                  signalColors, makeDecision, selected, finalOutcome)"> </h1>')),
                        shiny::column(3, align="center",
                                      shiny::HTML('<h1 id="deck4" class="decks" onclick="updateDfeFourOpts(\'deck4\',
                                                  \'deck2\', \'deck1\', \'deck3\', enFour, enThree, enTwo, enOne, ind,
                                                  4, outcome, samples, gambleNr, respTime, trial, t, clickEnabled,
                                                  signalColors, makeDecision, selected, finalOutcome)"> </h1>'))),

                      # set up the continue Button
                      shiny::fixedRow(
                        shiny::column(6,
                                      shinyjs::disabled(shiny::actionButton("decideDfe",
                                                                            label = buttonLabel,
                                                                            style =  "margin-top: 1.2em;
                                                                            margin-left: 8.5em;
                                                                            margin-bottom: 1.5em",
                                                                            onclick =
                                                                              "enableDecisionFourOpt(makeDecision,
                                                                            \"deck1\",
                                                                            \"deck2\",
                                                                            \"deck3\",
                                                                            \"deck4\");")),
                                      offset =  4))))
                        )
  } else {
    list(
      shiny::fixedRow(

        # call the neGame() javascript function to prepare objects
        shiny::tag("script", 'newDfeGame();'),

        # set up written info displayed above the boxes
        shiny::column(12,
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),

                      # set up logic to update the boxes via javascript. Arguments passed to js come from newGame() js
                      # function and the sendCustomMessage called just before this function
                      shiny::fixedRow(
                        shiny::column(3, align="center",
                                      shiny::HTML('<h1 id="deck1" class="decks" onclick="updateDfeFourOpts(\'deck1\',
                                                  \'deck2\', \'deck3\', \'deck4\', enOne, enTwo, enThree, enFour, ind,
                                                  1, outcome, samples, gambleNr, respTime, trial, t, clickEnabled,
                                                  signalColors, makeDecision, selected, finalOutcome)"> </h1>')),
                        shiny::column(3, align="center",
                                      shiny::HTML('<h1 id="deck2" class="decks" onclick="updateDfeFourOpts(\'deck2\',
                                                  \'deck1\', \'deck3\', \'deck4\', enTwo, enOne, enThree, enFour, ind,
                                                  2, outcome, samples, gambleNr, respTime, trial, t, clickEnabled,
                                                  signalColors, makeDecision, selected, finalOutcome)"> </h1>')),
                        shiny::column(3, align="center",
                                      shiny::HTML('<h1 id="deck3" class="decks" onclick="updateDfeFourOpts(\'deck3\',
                                                  \'deck2\', \'deck1\', \'deck4\', enThree, enTwo, enOne, enFour, ind,
                                                  3, outcome, samples, gambleNr, respTime, trial, t, clickEnabled,
                                                  signalColors, makeDecision, selected, finalOutcome)"> </h1>')),
                        shiny::column(3, align="center",
                                      shiny::HTML('<h1 id="deck4" class="decks" onclick="updateDfeFourOpts(\'deck4\',
                                                  \'deck2\', \'deck1\', \'deck3\', enFour, enThree, enTwo, enOne, ind,
                                                  4, outcome, samples, gambleNr, respTime, trial, t, clickEnabled,
                                                  signalColors, makeDecision, selected, finalOutcome)"> </h1>'))),

                      # set up the continue Button
                      shiny::fixedRow(
                        shiny::column(6,
                                      shinyjs::disabled(shiny::actionButton("decideDfe",
                                                                            label = buttonLabel,
                                                                            style =  "margin-top: 1.2em;
                                                                            margin-left: 8.5em;
                                                                            margin-bottom: 1.5em",
                                                                            onclick =
                                                                              "enableDecisionFourOpt(makeDecision,
                                                                            \"deck1\",
                                                                            \"deck2\",
                                                                            \"deck3\",
                                                                            \"deck4\");")),
                                      offset =  4))))
                        )
  }

}


# fiveOptDfePage =====================================

#' @describeIn twoOptDfePage A five option gamble version
fiveOptDfePage <- function(ctrlValsList, gambleTitle = "Gamble",
                           gambleTitlePractice = "Practice Gamble",
                           withPracticeGamble = TRUE, displayTitle = TRUE,
                           nGambles = 10, buttonLabel = "Make Decision"){

  if (isTRUE(withPracticeGamble)){

    list(
      shiny::fixedRow(

        # call the neGame() javascript function to prepare objects
        shiny::tag("script", 'newDfeGame();'),

        # set up written info displayed above the boxes
        shiny::column(12,
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::column(12, align = "left",
                                                    shiny::h2(ifelse(ctrlValsList$dfeGamble == 1,
                                                                     gambleTitlePractice,
                                                                     paste(gambleTitle,
                                                                           ctrlValsList$dfeGamble - 1,
                                                                           "of", nGambles))))),
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),
                      # set up logic to update the boxes via javascript. Arguments passed to js come from newGame() js
                      # function and the sendCustomMessage called just before this function
                      shiny::fixedRow(
                        shiny::column(1, align="center",
                                      shiny::HTML('<h1 id="emptySpace1" class="emptySpace">Place</h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck1" class="decks" onclick="updateDfeFiveOpts(\'deck1\',
                                                  \'deck2\', \'deck3\', \'deck4\', \'deck5\', enOne, enTwo, enThree,
                                                  enFour, enFive, ind, 1, outcome, samples, gambleNr, respTime, trial,
                                                  t, clickEnabled, signalColors, makeDecision, selected,
                                                  finalOutcome)"> </h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck2" class="decks" onclick="updateDfeFiveOpts(\'deck2\',
                                                  \'deck1\', \'deck3\', \'deck4\', \'deck5\', enTwo, enOne, enThree,
                                                  enFour, enFive, ind, 2, outcome, samples, gambleNr, respTime, trial,
                                                  t, clickEnabled, signalColors, makeDecision, selected,
                                                  finalOutcome)"> </h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck3" class="decks" onclick="updateDfeFiveOpts(\'deck3\',
                                                  \'deck2\', \'deck1\', \'deck4\', \'deck5\', enThree, enTwo, enOne,
                                                  enFour, enFive, ind, 3, outcome, samples, gambleNr, respTime, trial,
                                                  t, clickEnabled, signalColors, makeDecision, selected,
                                                  finalOutcome)"> </h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck4" class="decks" onclick="updateDfeFiveOpts(\'deck4\',
                                                  \'deck2\', \'deck1\', \'deck3\', \'deck5\', enFour, enThree, enTwo,
                                                  enOne, enFive, ind, 4, outcome, samples, gambleNr, respTime, trial,
                                                  t, clickEnabled, signalColors, makeDecision, selected,
                                                  finalOutcome)"> </h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck5" class="decks" onclick="updateDfeFiveOpts(\'deck5\',
                                                  \'deck2\', \'deck1\', \'deck3\', \'deck4\', enFive, enFour, enThree,
                                                  enTwo, enOne, ind, 5, outcome, samples, gambleNr, respTime, trial,
                                                  t, clickEnabled, signalColors, makeDecision, selected,
                                                  finalOutcome)"> </h1>')),
                        shiny::column(1, align="center",
                                      shiny::HTML('<h1 id="emptySpace2" class="emptySpace">Place</h1>'))),

                      # set up the continue Button
                      shiny::fixedRow(
                        shiny::column(6,
                                      shinyjs::disabled(shiny::actionButton("decideDfe", label = buttonLabel,
                                                                            style =  "margin-top: 0em;
                                                                            margin-left: 8.5em;
                                                                            margin-bottom: 1.5em",
                                                                            onclick =
                                                                              "enableDecisionFiveOpt(makeDecision,
                                                                            \"deck1\",
                                                                            \"deck2\",
                                                                            \"deck3\",
                                                                            \"deck4\",
                                                                            \"deck5\");")),
                                      offset =  4))))
                        )
  } else if (isTRUE(displayTitle)) {
    list(
      shiny::fixedRow(

        # call the neGame() javascript function to prepare objects
        shiny::tag("script", 'newDfeGame();'),

        # set up written info displayed above the boxes
        shiny::column(12,
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::column(12, align = "left",
                                                    shiny::h2(paste(gambleTitle,
                                                                    ctrlValsList$dfeGamble - 1,
                                                                    "of ",
                                                                    nGambles)))),
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),
                      # set up logic to update the boxes via javascript. Arguments passed to js come from newGame() js
                      # function and the sendCustomMessage called just before this function
                      shiny::fixedRow(
                        shiny::column(1, align="center",
                                      shiny::HTML('<h1 id="emptySpace1" class="emptySpace">Place</h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck1" class="decks" onclick="updateDfeFiveOpts(\'deck1\',
                                                  \'deck2\', \'deck3\', \'deck4\', \'deck5\', enOne, enTwo, enThree,
                                                  enFour, enFive, ind, 1, outcome, samples, gambleNr, respTime, trial,
                                                  t, clickEnabled, signalColors, makeDecision, selected,
                                                  finalOutcome)"> </h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck2" class="decks" onclick="updateDfeFiveOpts(\'deck2\',
                                                  \'deck1\', \'deck3\', \'deck4\', \'deck5\', enTwo, enOne, enThree,
                                                  enFour, enFive, ind, 2, outcome, samples, gambleNr, respTime, trial,
                                                  t, clickEnabled, signalColors, makeDecision, selected,
                                                  finalOutcome)"> </h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck3" class="decks" onclick="updateDfeFiveOpts(\'deck3\',
                                                  \'deck2\', \'deck1\', \'deck4\', \'deck5\', enThree, enTwo, enOne,
                                                  enFour, enFive, ind, 3, outcome, samples, gambleNr, respTime, trial,
                                                  t, clickEnabled, signalColors, makeDecision, selected,
                                                  finalOutcome)"> </h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck4" class="decks" onclick="updateDfeFiveOpts(\'deck4\',
                                                  \'deck2\', \'deck1\', \'deck3\', \'deck5\', enFour, enThree, enTwo,
                                                  enOne, enFive, ind, 4, outcome, samples, gambleNr, respTime, trial,
                                                  t, clickEnabled, signalColors, makeDecision, selected,
                                                  finalOutcome)"> </h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck5" class="decks" onclick="updateDfeFiveOpts(\'deck5\',
                                                  \'deck2\', \'deck1\', \'deck3\', \'deck4\', enFive, enFour, enThree,
                                                  enTwo, enOne, ind, 5, outcome, samples, gambleNr, respTime, trial,
                                                  t, clickEnabled, signalColors, makeDecision, selected,
                                                  finalOutcome)"> </h1>')),
                        shiny::column(1, align="center",
                                      shiny::HTML('<h1 id="emptySpace2" class="emptySpace">Place</h1>'))),

                      # set up the continue Button
                      shiny::fixedRow(
                        shiny::column(6,
                                      shinyjs::disabled(shiny::actionButton("decideDfe",
                                                                            label = buttonLabel,
                                                                            style =  "margin-top: 0em;
                                                                            margin-left: 8.5em;
                                                                            margin-bottom: 1.5em",
                                                                            onclick =
                                                                              "enableDecisionFiveOpt(makeDecision,
                                                                            \"deck1\",
                                                                            \"deck2\",
                                                                            \"deck3\",
                                                                            \"deck4\",
                                                                            \"deck5\");")),
                                      offset =  4))))
                        )
  } else {
    list(
      shiny::fixedRow(

        # call the neGame() javascript function to prepare objects
        shiny::tag("script", 'newDfeGame();'),

        # set up written info displayed above the boxes
        shiny::column(12,
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),
                      # set up logic to update the boxes via javascript. Arguments passed to js come from newGame() js
                      # function and the sendCustomMessage called just before this function
                      shiny::fixedRow(
                        shiny::column(1, align="center",
                                      shiny::HTML('<h1 id="emptySpace1" class="emptySpace">Place</h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck1" class="decks" onclick="updateDfeFiveOpts(\'deck1\',
                                                  \'deck2\', \'deck3\', \'deck4\', \'deck5\', enOne, enTwo, enThree,
                                                  enFour, enFive, ind, 1, outcome, samples, gambleNr, respTime, trial,
                                                  t, clickEnabled, signalColors, makeDecision, selected,
                                                  finalOutcome)"> </h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck2" class="decks" onclick="updateDfeFiveOpts(\'deck2\',
                                                  \'deck1\', \'deck3\', \'deck4\', \'deck5\', enTwo, enOne, enThree,
                                                  enFour, enFive, ind, 2, outcome, samples, gambleNr, respTime, trial,
                                                  t, clickEnabled, signalColors, makeDecision, selected,
                                                  finalOutcome)"> </h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck3" class="decks" onclick="updateDfeFiveOpts(\'deck3\',
                                                  \'deck2\', \'deck1\', \'deck4\', \'deck5\', enThree, enTwo, enOne,
                                                  enFour, enFive, ind, 3, outcome, samples, gambleNr, respTime, trial,
                                                  t, clickEnabled, signalColors, makeDecision, selected,
                                                  finalOutcome)"> </h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck4" class="decks" onclick="updateDfeFiveOpts(\'deck4\',
                                                  \'deck2\', \'deck1\', \'deck3\', \'deck5\', enFour, enThree, enTwo,
                                                  enOne, enFive, ind, 4, outcome, samples, gambleNr, respTime, trial,
                                                  t, clickEnabled, signalColors, makeDecision, selected,
                                                  finalOutcome)"> </h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck5" class="decks" onclick="updateDfeFiveOpts(\'deck5\',
                                                  \'deck2\', \'deck1\', \'deck3\', \'deck4\', enFive, enFour, enThree,
                                                  enTwo, enOne, ind, 5, outcome, samples, gambleNr, respTime, trial,
                                                  t, clickEnabled, signalColors, makeDecision, selected,
                                                  finalOutcome)"> </h1>')),
                        shiny::column(1, align="center",
                                      shiny::HTML('<h1 id="emptySpace2" class="emptySpace">Place</h1>'))),

                      # set up the continue Button
                      shiny::fixedRow(
                        shiny::column(6,
                                      shinyjs::disabled(shiny::actionButton("decideDfe",
                                                                            label = buttonLabel,
                                                                            style =  "margin-top: 0em;
                                                                            margin-left: 8.5em;
                                                                            margin-bottom: 1.5em",
                                                                            onclick =
                                                                              "enableDecisionFiveOpt(makeDecision,
                                                                            \"deck1\",
                                                                            \"deck2\",
                                                                            \"deck3\",
                                                                            \"deck4\",
                                                                            \"deck5\");")),
                                      offset =  4))))
                        )
  }

}


# sixOptDfePage ======================================

#' @describeIn twoOptDfePage A six option gamble version
sixOptDfePage <- function(ctrlValsList, gambleTitle = "Gamble",
                          gambleTitlePractice = "Practice Gamble",
                          withPracticeGamble = TRUE, displayTitle = TRUE,
                          nGambles = 10, buttonLabel = "Make Decision"){

  if (isTRUE(withPracticeGamble)){

    list(
      shiny::fixedRow(

        # call the neGame() javascript function to prepare objects
        shiny::tag("script", 'newDfeGame();'),

        # set up written info displayed above the boxes
        shiny::column(12,
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::column(12, align = "left",
                                                    shiny::h2(ifelse(ctrlValsList$dfeGamble == 1,
                                                                     gambleTitlePractice,
                                                                     paste(gambleTitle,
                                                                           ctrlValsList$dfeGamble - 1,
                                                                           "of", nGambles))))),
                      shiny::fixedRow(shiny::br()),

                      # set up logic to update the boxes via javascript. Arguments passed to js come from newGame() js
                      # function and the sendCustomMessage called just before this function
                      shiny::fixedRow(
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck1" class="decks" onclick="updateDfeSixOpts(\'deck1\',
                                                  \'deck2\', \'deck3\', \'deck4\', \'deck4\', \'deck6\', enOne, enTwo,
                                                  enThree, enFour, enFive, enSix, ind, 1, outcome, samples, gambleNr,
                                                  respTime, trial, t, clickEnabled, signalColors, makeDecision,
                                                  selected, finalOutcome)"> </h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck2" class="decks" onclick="updateDfeSixOpts(\'deck2\',
                                                  \'deck1\', \'deck3\', \'deck4\', \'deck4\', \'deck6\', enTwo, enOne,
                                                  enThree, enFour, enFive, enSix, ind, 2, outcome, samples, gambleNr,
                                                  respTime, trial, t, clickEnabled, signalColors, makeDecision,
                                                  selected, finalOutcome)"> </h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck3" class="decks" onclick="updateDfeSixOpts(\'deck3\',
                                                  \'deck2\', \'deck1\', \'deck4\', \'deck4\', \'deck6\', enThree, enTwo,
                                                  enOne, enFour, enFive, enSix, ind, 3, outcome, samples, gambleNr,
                                                  respTime, trial, t, clickEnabled, signalColors, makeDecision,
                                                  selected, finalOutcome)"> </h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck4" class="decks" onclick="updateDfeSixOpts(\'deck4\',
                                                  \'deck2\', \'deck1\', \'deck3\', \'deck5\', \'deck6\',enFour, enThree,
                                                  enTwo, enOne, enFive, enSix, ind, 4, outcome, samples, gambleNr,
                                                  respTime, trial, t, clickEnabled, signalColors, makeDecision,
                                                  selected, finalOutcome)"> </h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck5" class="decks" onclick="updateDfeSixOpts(\'deck5\',
                                                  \'deck2\', \'deck1\', \'deck3\', \'deck4\', \'deck6\', enFive,
                                                  enThree, enTwo, enOne, enFour, enSix, ind, 5, outcome, samples,
                                                  gambleNr, respTime, trial, t, clickEnabled, signalColors,
                                                  makeDecision, selected, finalOutcome)"> </h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck6" class="decks" onclick="updateDfeSixOpts(\'deck6\',
                                                  \'deck2\', \'deck1\', \'deck3\', \'deck5\', \'deck4\',enSix, enThree,
                                                  enTwo, enOne, enFive, enFour, ind, 6, outcome, samples, gambleNr,
                                                  respTime, trial, t, clickEnabled, signalColors, makeDecision,
                                                  selected, finalOutcome)"> </h1>'))),

                      # set up the continue Button
                      shiny::fixedRow(
                        shiny::column(6,
                                      shinyjs::disabled(shiny::actionButton("decideDfe", label = buttonLabel,
                                                                            style =  "margin-top: 1.2em;
                                                                            margin-left: 8.8em;
                                                                            margin-bottom: 1.5em",
                                                                            onclick =
                                                                              "enableDecisionSixOpt(makeDecision,
                                                                            \"deck1\",
                                                                            \"deck2\",
                                                                            \"deck3\",
                                                                            \"deck4\",
                                                                            \"deck5\",
                                                                            \"deck6\");")),
                                      offset =  4))))
                        )
  } else if (isTRUE(displayTitle)) {
    list(
      shiny::fixedRow(

        # call the neGame() javascript function to prepare objects
        shiny::tag("script", 'newDfeGame();'),

        # set up written info displayed above the boxes
        shiny::column(12,
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::column(12, align = "left",
                                                    shiny::h2(paste(gambleTitle,
                                                                    ctrlValsList$dfeGamble - 1,
                                                                    "of ",
                                                                    nGambles)))),
                      shiny::fixedRow(shiny::br()),

                      # set up logic to update the boxes via javascript. Arguments passed to js come from newGame() js
                      # function and the sendCustomMessage called just before this function
                      shiny::fixedRow(
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck1" class="decks" onclick="updateDfeSixOpts(\'deck1\',
                                                  \'deck2\', \'deck3\', \'deck4\', \'deck4\', \'deck6\', enOne, enTwo,
                                                  enThree, enFour, enFive, enSix, ind, 1, outcome, samples, gambleNr,
                                                  respTime, trial, t, clickEnabled, signalColors, makeDecision,
                                                  selected, finalOutcome)"> </h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck2" class="decks" onclick="updateDfeSixOpts(\'deck2\',
                                                  \'deck1\', \'deck3\', \'deck4\', \'deck4\', \'deck6\', enTwo, enOne,
                                                  enThree, enFour, enFive, enSix, ind, 2, outcome, samples, gambleNr,
                                                  respTime, trial, t, clickEnabled, signalColors, makeDecision,
                                                  selected, finalOutcome)"> </h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck3" class="decks" onclick="updateDfeSixOpts(\'deck3\',
                                                  \'deck2\', \'deck1\', \'deck4\', \'deck4\', \'deck6\', enThree, enTwo,
                                                  enOne, enFour, enFive, enSix, ind, 3, outcome, samples, gambleNr,
                                                  respTime, trial, t, clickEnabled, signalColors, makeDecision,
                                                  selected, finalOutcome)"> </h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck4" class="decks" onclick="updateDfeSixOpts(\'deck4\',
                                                  \'deck2\', \'deck1\', \'deck3\', \'deck5\', \'deck6\',enFour, enThree,
                                                  enTwo, enOne, enFive, enSix, ind, 4, outcome, samples, gambleNr,
                                                  respTime, trial, t, clickEnabled, signalColors, makeDecision,
                                                  selected, finalOutcome)"> </h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck5" class="decks" onclick="updateDfeSixOpts(\'deck5\',
                                                  \'deck2\', \'deck1\', \'deck3\', \'deck4\', \'deck6\', enFive,
                                                  enThree, enTwo, enOne, enFour, enSix, ind, 5, outcome, samples,
                                                  gambleNr, respTime, trial, t, clickEnabled, signalColors,
                                                  makeDecision, selected, finalOutcome)"> </h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck6" class="decks" onclick="updateDfeSixOpts(\'deck6\',
                                                  \'deck2\', \'deck1\', \'deck3\', \'deck5\', \'deck4\',enSix, enThree,
                                                  enTwo, enOne, enFive, enFour, ind, 6, outcome, samples, gambleNr,
                                                  respTime, trial, t, clickEnabled, signalColors, makeDecision,
                                                  selected, finalOutcome)"> </h1>'))),

                      # set up the continue Button
                      shiny::fixedRow(
                        shiny::column(6,
                                      shinyjs::disabled(shiny::actionButton("decideDfe",
                                                                            label = buttonLabel,
                                                                            style =  "margin-top: 1.2em;
                                                                            margin-left: 8.8em;
                                                                            margin-bottom: 1.5em",
                                                                            onclick =
                                                                              "enableDecisionSixOpt(makeDecision,
                                                                            \"deck1\",
                                                                            \"deck2\",
                                                                            \"deck3\",
                                                                            \"deck4\",
                                                                            \"deck5\",
                                                                            \"deck6\");")),
                                      offset =  4))))
                        )
  } else {
    list(
      shiny::fixedRow(

        # call the neGame() javascript function to prepare objects
        shiny::tag("script", 'newDfeGame();'),

        # set up written info displayed above the boxes
        shiny::column(12,
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::br()),

                      # set up logic to update the boxes via javascript. Arguments passed to js come from newGame() js
                      # function and the sendCustomMessage called just before this function
                      shiny::fixedRow(
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck1" class="decks" onclick="updateDfeSixOpts(\'deck1\',
                                                  \'deck2\', \'deck3\', \'deck4\', \'deck4\', \'deck6\', enOne, enTwo,
                                                  enThree, enFour, enFive, enSix, ind, 1, outcome, samples, gambleNr,
                                                  respTime, trial, t, clickEnabled, signalColors, makeDecision,
                                                  selected, finalOutcome)"> </h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck2" class="decks" onclick="updateDfeSixOpts(\'deck2\',
                                                  \'deck1\', \'deck3\', \'deck4\', \'deck4\', \'deck6\', enTwo, enOne,
                                                  enThree, enFour, enFive, enSix, ind, 2, outcome, samples, gambleNr,
                                                  respTime, trial, t, clickEnabled, signalColors, makeDecision,
                                                  selected, finalOutcome)"> </h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck3" class="decks" onclick="updateDfeSixOpts(\'deck3\',
                                                  \'deck2\', \'deck1\', \'deck4\', \'deck4\', \'deck6\', enThree, enTwo,
                                                  enOne, enFour, enFive, enSix, ind, 3, outcome, samples, gambleNr,
                                                  respTime, trial, t, clickEnabled, signalColors, makeDecision,
                                                  selected, finalOutcome)"> </h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck4" class="decks" onclick="updateDfeSixOpts(\'deck4\',
                                                  \'deck2\', \'deck1\', \'deck3\', \'deck5\', \'deck6\',enFour, enThree,
                                                  enTwo, enOne, enFive, enSix, ind, 4, outcome, samples, gambleNr,
                                                  respTime, trial, t, clickEnabled, signalColors, makeDecision,
                                                  selected, finalOutcome)"> </h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck5" class="decks" onclick="updateDfeSixOpts(\'deck5\',
                                                  \'deck2\', \'deck1\', \'deck3\', \'deck4\', \'deck6\', enFive,
                                                  enThree, enTwo, enOne, enFour, enSix, ind, 5, outcome, samples,
                                                  gambleNr, respTime, trial, t, clickEnabled, signalColors,
                                                  makeDecision, selected, finalOutcome)"> </h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck6" class="decks" onclick="updateDfeSixOpts(\'deck6\',
                                                  \'deck2\', \'deck1\', \'deck3\', \'deck5\', \'deck4\',enSix, enThree,
                                                  enTwo, enOne, enFive, enFour, ind, 6, outcome, samples, gambleNr,
                                                  respTime, trial, t, clickEnabled, signalColors, makeDecision,
                                                  selected, finalOutcome)"> </h1>'))),

                      # set up the continue Button
                      shiny::fixedRow(
                        shiny::column(6,
                                      shinyjs::disabled(shiny::actionButton("decideDfe",
                                                                            label = buttonLabel,
                                                                            style =  "margin-top: 1.2em;
                                                                            margin-left: 8.8em;
                                                                            margin-bottom: 1.5em",
                                                                            onclick =
                                                                              "enableDecisionSixOpt(makeDecision,
                                                                            \"deck1\",
                                                                            \"deck2\",
                                                                            \"deck3\",
                                                                            \"deck4\",
                                                                            \"deck5\",
                                                                            \"deck6\");")),
                                      offset =  4))))
                        )
  }

}

