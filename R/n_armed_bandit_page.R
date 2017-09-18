#' Create an HTML Page to Display and Play a 2 to 6 Armed Bandit
#'
#' Create an html page and send information to javascript if an option is
#' clicked. Needs prepared lists of outcome values created by
#' \code{\link{createBanditList}} or of the same structure and control variables
#' created with \code{\link{createCtrlList}}.
#'
#' These functions are called from within \code{\link{multiArmedBanditPage}}
#' because this also handles the appropriate custom message.
#' @param ctrlValsList list of reactive values. Controls the flow through the
#'  Experiment. Can be created with \code{\link{createCtrlList}}.
#' @param gameTitle string. Is displayed as title of the trial, e.g. if it is
#'  set to "Game" the displayed title in game 2 of 10 will be "Game 2 of 10".
#' @param nTrials integer. The number of games played WITHOUT the practice game.
#' @param clickCounter string. Is displayed next to the number of clicks still
#'  left in one game. Default is set to "Clicks Remaining".
#' @param gameTitlePractice string. Is displayed as title of the practice game.
#' @param withPracticeGame logical. If TRUE (default) a practice
#'  game must be provided and will be displayed.
#' @param pointTitle string. Is displayed as label next to the number of points
#'  earned. Default is set to "Points Earned".
#' @param nGames integer. The number of games played WITHOUT the practice game.
#' @param buttonLabel string. Is displayed in the continue button. Default is
#'  set to "Click to Continue to next Game".
#'
#' @return An html page that can be displayed by the shiny app.
#' @export
twoArmedBanditPage <- function(ctrlValsList, gameTitle = "Game ",
                               nTrials = NULL, clickCounter = "Clicks Remaining",
                               gameTitlePractice = "Practice Game",
                               withPracticeGame = TRUE,
                               pointTitle = "Points Earned", nGames = 10,
                               buttonLabel = "Click to Continue to next Game"){

  if (isTRUE(withPracticeGame)){
    list(
      shiny::fixedRow(

        # call the neGame() javascript function to prepare objects
        shiny::tag("script", 'newGame();'),

        # set up written info displayed above the boxes
        shiny::column(12,
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::column(12, align = "left",
                                                    shiny::h2(
                                                      ifelse(ctrlValsList$banditGame == 1,
                                                             gameTitlePractice,
                                                             paste(gameTitle,
                                                                   ctrlValsList$banditGame - 1,
                                                                   "of ", nGames))))),
                      shiny::fixedRow(
                        shiny::column(6, align="right",
                                      shiny::p(id = "clicksRemaining",
                                               nTrials)),
                        shiny::column(6, align="left",
                                      shiny::h3(class = "upperParams",
                                                clickCounter))
                      ),

                      shiny::fixedRow(
                        shiny::column(6, align="right",
                                      shiny::p(id = "pointCounter", "0")),
                        shiny::column(6, align="left",
                                      shiny::h3(class = "upperParams",
                                                pointTitle))
                      ),

                      shiny::fixedRow(shiny::br()),

                      # set up logic to update the boxes via javascript.
                      # Arguments passed to js come from newGame() js function and
                      # the sendCustomMessage called just before this function
                      shiny::fixedRow(
                        shiny::column(1, align="center",
                                      shiny::HTML('<h1 id="emptySpace1" class="emptySpace">Place</h1>')),
                        shiny::column(5, align="center",
                                      shiny::HTML('<h1 id="deck1" class="decks" onclick="updateBanditTwoArms(\'deck1\', \'deck2\',
                                                  \'pointCounter\', \'clicksRemaining\', enOne, enTwo, ind, 1, outcome, outcomeCum,
                                                  selection, nTrials, gameNr, respTime, trial, t, clickEnabled, nDigits)"> </h1>')),
                        shiny::column(5, align="center",
                                      shiny::HTML('<h1 id="deck2" class="decks" onclick="updateBanditTwoArms(\'deck2\', \'deck1\',
                                                  \'pointCounter\', \'clicksRemaining\', enTwo, enOne, ind, 2, outcome, outcomeCum,
                                                  selection, nTrials, gameNr, respTime, trial, t, clickEnabled, nDigits)"> </h1>')),
                        shiny::column(1, align="center",
                                      shiny::HTML('<h1 id="emptySpace2" class="emptySpace">Place</h1>'))),

                      # set up the continue Button
                      shiny::fixedRow(
                        shiny::column(6,
                                      shinyjs::hidden(
                                        shiny::actionButton("continueBandit",
                                                            label = buttonLabel,
                                                            style =  "margin-top: 2em; margin-left: 5.3em; margin-bottom: 3em" )),
                                      offset =  4))))
        )

  } else {

    list(
      shiny::fixedRow(

        # call the neGame() javascript function to prepare objects
        shiny::tag("script", 'newGame();'),

        # set up written info displayed above the boxes
        shiny::column(12, shiny::fixedRow(
          shiny::br()),
          shiny::fixedRow(shiny::column(12, align = "left",shiny::h2(
            paste(gameTitle, ctrlValsList$banditGame, "of ", nGames)))),
          shiny::fixedRow(
            shiny::column(6, align="right", shiny::p(id = "clicksRemaining",
                                                     paste(nTrials))),
            shiny::column(6, align="left", shiny::h3(class = "upperParams",
                                                     clickCounter))),
          shiny::fixedRow(
            shiny::column(6, align="right", shiny::p(id = "pointCounter",
                                                     "0")),
            shiny::column(6, align="left", shiny::h3(class = "upperParams",
                                                     pointTitle))
          ),
          shiny::fixedRow(shiny::br()),
          # set up logic to update the boxes via javascript. Arguments passed to
          # js come from newGame() js function and
          # the sendCustomMessage called just before this function
          shiny::fixedRow(
            shiny::column(1, align="center",
                          shiny::HTML('<h1 id="emptySpace1" class="emptySpace">Place</h1>')),
            shiny::column(5, align="center",
                          shiny::HTML('<h1 id="deck1" class="decks" onclick="updateBanditTwoArms(\'deck1\', \'deck2\',
                                      \'pointCounter\', \'clicksRemaining\', enOne, enTwo, ind, 1, outcome, outcomeCum,
                                      selection, nTrials, gameNr, respTime, trial, t, clickEnabled, nDigits)"> </h1>')),
            shiny::column(5, align="center",
                          shiny::HTML('<h1 id="deck2" class="decks" onclick="updateBanditTwoArms(\'deck2\', \'deck1\',
                                      \'pointCounter\', \'clicksRemaining\', enTwo, enOne, ind, 2, outcome, outcomeCum,
                                      selection, nTrials, gameNr, respTime, trial, t, clickEnabled, nDigits)"> </h1>')),
            shiny::column(1, align="center",
                          shiny::HTML('<h1 id="emptySpace2" class="emptySpace">Place</h1>'))),
          # set up the continue Button
          shiny::fixedRow(
            shiny::column(6, shinyjs::hidden(
              shiny::actionButton("continueBandit",
                                  label = buttonLabel,
                                  style =  "margin-top: 2em; margin-left: 5.3em; margin-bottom: 3em" )),
              offset =  4))))
        )

  }

}


# threeArmedBanditPage =====================================

#' @describeIn twoArmedBanditPage A three armed bandit version
threeArmedBanditPage <- function(ctrlValsList, gameTitle = "Game ",
                                 nTrials = NULL, clickCounter = "Clicks Remaining",
                                 gameTitlePractice = "Practice Game",
                                 withPracticeGame = TRUE,
                                 pointTitle = "Points Earned", nGames = 10,
                                 buttonLabel = "Click to Continue to next Game"){

  if (isTRUE(withPracticeGame)){
    list(
      shiny::fixedRow(

        # call the neGame() javascript function to prepare objects
        shiny::tag("script", 'newGame();'),

        # set up written info displayed above the boxes
        shiny::column(12,
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::column(12, align = "left",
                                                    shiny::h2(
                                                      ifelse(ctrlValsList$banditGame == 1,
                                                             gameTitlePractice,
                                                             paste(gameTitle,
                                                                   ctrlValsList$banditGame - 1,
                                                                   "of ", nGames))))),
                      shiny::fixedRow(
                        shiny::column(6, align="right",
                                      shiny::p(id = "clicksRemaining",
                                               nTrials)),
                        shiny::column(6, align="left",
                                      shiny::h3(class = "upperParams",
                                                clickCounter))
                      ),

                      shiny::fixedRow(
                        shiny::column(6, align="right", shiny::p(id = "pointCounter", "0")),
                        shiny::column(6, align="left", shiny::h3(class = "upperParams", pointTitle))
                      ),

                      shiny::fixedRow(shiny::br()),

                      # set up logic to update the boxes via javascript.
                      # Arguments passed to js come from newGame() js function and
                      # the sendCustomMessage called just before this function
                      shiny::fixedRow(
                        shiny::column(4, align="center",
                                      shiny::HTML('<h1 id="deck1" class="decks" onclick="updateBanditThreeArms(\'deck1\',
                                                  \'deck2\', \'deck3\', \'pointCounter\', \'clicksRemaining\', enOne,
                                                  enTwo, enThree, ind, 1, outcome, outcomeCum, selection, nTrials, gameNr,
                                                  respTime, trial, t, clickEnabled, nDigits)"> </h1>')),
                        shiny::column(4, align="center",
                                      shiny::HTML('<h1 id="deck2" class="decks" onclick="updateBanditThreeArms(\'deck2\',
                                                  \'deck1\', \'deck3\', \'pointCounter\', \'clicksRemaining\', enTwo, enOne,
                                                  enThree, ind, 2, outcome, outcomeCum, selection, nTrials, gameNr, respTime,
                                                  trial, t, clickEnabled, nDigits)"> </h1>')),
                        shiny::column(4, align="center",
                                      shiny::HTML('<h1 id="deck3" class="decks" onclick="updateBanditThreeArms(\'deck3\',
                                                  \'deck2\', \'deck1\', \'pointCounter\', \'clicksRemaining\', enThree,
                                                  enOne, enTwo, ind, 3, outcome, outcomeCum, selection, nTrials, gameNr,
                                                  respTime, trial, t, clickEnabled, nDigits)"> </h1>'))
                        ),

                      # set up the continue Button
                      shiny::fixedRow(
                        shiny::column(6,
                                      shinyjs::hidden(
                                        shiny::actionButton("continueBandit",
                                                            label = buttonLabel,
                                                            style = "margin-top: 2em;
                                                            margin-left: 5.3em;
                                                            margin-bottom: 3em" )),
                                      offset =  4))))
                        )

  } else {

    list(
      shiny::fixedRow(

        # call the neGame() javascript function to prepare objects
        shiny::tag("script", 'newGame();'),

        # set up written info displayed above the boxes
        shiny::column(12, shiny::fixedRow(
          shiny::br()),
          shiny::fixedRow(shiny::column(12, align = "left",shiny::h2(
            paste(gameTitle, ctrlValsList$banditGame, "of ", nGames)))),
          shiny::fixedRow(
            shiny::column(6, align="right", shiny::p(id = "clicksRemaining",
                                                     paste(nTrials))),
            shiny::column(6, align="left", shiny::h3(class = "upperParams",
                                                     clickCounter))),
          shiny::fixedRow(
            shiny::column(6, align="right", shiny::p(id = "pointCounter",
                                                     "0")),
            shiny::column(6, align="left", shiny::h3(class = "upperParams",
                                                     pointTitle))
          ),
          shiny::fixedRow(shiny::br()),
          # set up logic to update the boxes via javascript. Arguments passed to
          # js come from newGame() js function and
          # the sendCustomMessage called just before this function
          shiny::fixedRow(
            shiny::column(4, align="center",
                          shiny::HTML('<h1 id="deck1" class="decks" onclick="updateBanditThreeArms(\'deck1\', \'deck2\',
                                      \'deck3\', \'pointCounter\', \'clicksRemaining\', enOne, enTwo, enThree, ind, 1,
                                      outcome, outcomeCum, selection, nTrials, gameNr, respTime, trial, t,
                                      clickEnabled, nDigits)"> </h1>')),
            shiny::column(4, align="center",
                          shiny::HTML('<h1 id="deck2" class="decks" onclick="updateBanditThreeArms(\'deck2\', \'deck1\',
                                      \'deck3\', \'pointCounter\', \'clicksRemaining\', enTwo, enOne, enThree, ind, 2,
                                      outcome, outcomeCum, selection, nTrials, gameNr, respTime, trial, t,
                                      clickEnabled, nDigits)"> </h1>')),
            shiny::column(4, align="center",
                          shiny::HTML('<h1 id="deck3" class="decks" onclick="updateBanditThreeArms(\'deck3\', \'deck2\',
                                      \'deck1\', \'pointCounter\', \'clicksRemaining\', enThree, enOne, enTwo, ind, 3,
                                      outcome, outcomeCum, selection, nTrials, gameNr, respTime, trial, t,
                                      clickEnabled, nDigits)"> </h1>'))
            ),
          # set up the continue Button
          shiny::fixedRow(
            shiny::column(6, shinyjs::hidden(
              shiny::actionButton("continueBandit",
                                  label = buttonLabel,
                                  style =  "margin-top: 2em; margin-left: 5.3em; margin-bottom: 3em" )),
              offset =  4))))
    )

  }

}


# fourArmedBanditPage =====================================

#' @describeIn twoArmedBanditPage A four armed bandit version
fourArmedBanditPage <- function(ctrlValsList, gameTitle = "Game ",
                                nTrials = NULL, clickCounter = "Clicks Remaining",
                                gameTitlePractice = "Practice Game",
                                withPracticeGame = TRUE,
                                pointTitle = "Points Earned", nGames = 10,
                                buttonLabel = "Click to Continue to next Game"){

  if (isTRUE(withPracticeGame)){
    list(
      shiny::fixedRow(

        # call the neGame() javascript function to prepare objects
        shiny::tag("script", 'newGame();'),

        # set up written info displayed above the boxes
        shiny::column(12,
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::column(12, align = "left",
                                                    shiny::h2(
                                                      ifelse(ctrlValsList$banditGame == 1,
                                                             gameTitlePractice,
                                                             paste(gameTitle,
                                                                   ctrlValsList$banditGame - 1,
                                                                   "of ", nGames))))),
                      shiny::fixedRow(
                        shiny::column(6, align="right",
                                      shiny::p(id = "clicksRemaining",
                                               nTrials)),
                        shiny::column(6, align="left",
                                      shiny::h3(class = "upperParams",
                                                clickCounter))
                      ),

                      shiny::fixedRow(
                        shiny::column(6, align="right",
                                      shiny::p(id = "pointCounter", "0")),
                        shiny::column(6, align="left",
                                      shiny::h3(class = "upperParams",
                                                pointTitle))
                      ),

                      shiny::fixedRow(shiny::br()),

                      # set up logic to update the boxes via javascript. Arguments passed to js come from newGame() js function and
                      # the sendCustomMessage called just before this function
                      shiny::fixedRow(
                        shiny::column(3, align="center",
                                      shiny::HTML('<h1 id="deck1" class="decks" onclick="updateBanditFourArms(\'deck1\',
                                                  \'deck2\', \'deck3\', \'deck4\', \'pointCounter\', \'clicksRemaining\',
                                                  enOne, enTwo, enThree, enFour, ind, 1, outcome, outcomeCum, selection,
                                                  nTrials, gameNr, respTime, trial, t, clickEnabled, nDigits)"> </h1>')),
                        shiny::column(3, align="center",
                                      shiny::HTML('<h1 id="deck2" class="decks" onclick="updateBanditFourArms(\'deck2\',
                                                  \'deck1\', \'deck3\', \'deck4\', \'pointCounter\', \'clicksRemaining\',
                                                  enTwo, enOne, enThree, enFour, ind, 2, outcome, outcomeCum, selection,
                                                  nTrials, gameNr, respTime, trial, t, clickEnabled, nDigits)"> </h1>')),
                        shiny::column(3, align="center",
                                      shiny::HTML('<h1 id="deck3" class="decks" onclick="updateBanditFourArms(\'deck3\',
                                                  \'deck2\', \'deck1\', \'deck4\', \'pointCounter\', \'clicksRemaining\',
                                                  enThree, enOne, enTwo, enFour, ind, 3, outcome, outcomeCum, selection,
                                                  nTrials, gameNr, respTime, trial, t, clickEnabled, nDigits)"> </h1>')),
                        shiny::column(3, align="center",
                                      shiny::HTML('<h1 id="deck4" class="decks" onclick="updateBanditFourArms(\'deck4\',
                                                  \'deck3\', \'deck2\', \'deck1\', \'pointCounter\', \'clicksRemaining\',
                                                  enFour, enThree, enOne, enTwo, ind, 4, outcome, outcomeCum, selection,
                                                  nTrials, gameNr, respTime, trial, t, clickEnabled, nDigits)"> </h1>'))
                        ),

                      # set up the continue Button
                      shiny::fixedRow(
                        shiny::column(6,
                                      shinyjs::hidden(
                                        shiny::actionButton("continueBandit",
                                                            label = buttonLabel,
                                                            style = "margin-top: 2em;
                                                            margin-left: 5.3em;
                                                            margin-bottom: 3em" )),
                                      offset =  4))))
                        )

  } else {

    list(
      shiny::fixedRow(

        # call the neGame() javascript function to prepare objects
        shiny::tag("script", 'newGame();'),

        # set up written info displayed above the boxes
        shiny::column(12, shiny::fixedRow(
          shiny::br()),
          shiny::fixedRow(shiny::column(12, align = "left",shiny::h2(
            paste(gameTitle, ctrlValsList$banditGame, "of ", nGames)))),
          shiny::fixedRow(
            shiny::column(6, align="right", shiny::p(id = "clicksRemaining",
                                                     paste(nTrials))),
            shiny::column(6, align="left", shiny::h3(class = "upperParams",
                                                     clickCounter))),
          shiny::fixedRow(
            shiny::column(6, align="right", shiny::p(id = "pointCounter",
                                                     "0")),
            shiny::column(6, align="left", shiny::h3(class = "upperParams",
                                                     pointTitle))
          ),
          shiny::fixedRow(shiny::br()),
          # set up logic to update the boxes via javascript. Arguments passed to
          # js come from newGame() js function and
          # the sendCustomMessage called just before this function
          shiny::fixedRow(
            shiny::column(3, align="center",
                          shiny::HTML('<h1 id="deck1" class="decks" onclick="updateBanditFourArms(\'deck1\',
                                      \'deck2\', \'deck3\', \'deck4\', \'pointCounter\', \'clicksRemaining\',
                                      enOne, enTwo, enThree, enFour, ind, 1, outcome, outcomeCum, selection,
                                      nTrials, gameNr, respTime, trial, t, clickEnabled, nDigits)"> </h1>')),
            shiny::column(3, align="center",
                          shiny::HTML('<h1 id="deck2" class="decks" onclick="updateBanditFourArms(\'deck2\',
                                      \'deck1\', \'deck3\', \'deck4\', \'pointCounter\', \'clicksRemaining\',
                                      enTwo, enOne, enThree, enFour, ind, 2, outcome, outcomeCum, selection,
                                      nTrials, gameNr, respTime, trial, t, clickEnabled, nDigits)"> </h1>')),
            shiny::column(3, align="center",
                          shiny::HTML('<h1 id="deck3" class="decks" onclick="updateBanditFourArms(\'deck3\',
                                      \'deck2\', \'deck1\', \'deck4\', \'pointCounter\', \'clicksRemaining\',
                                      enThree, enOne, enTwo, enFour, ind, 3, outcome, outcomeCum, selection,
                                      nTrials, gameNr, respTime, trial, t, clickEnabled, nDigits)"> </h1>')),
            shiny::column(3, align="center",
                          shiny::HTML('<h1 id="deck4" class="decks" onclick="updateBanditFourArms(\'deck4\',
                                      \'deck3\', \'deck2\', \'deck1\', \'pointCounter\', \'clicksRemaining\',
                                      enFour, enThree, enOne, enTwo, ind, 4, outcome, outcomeCum, selection,
                                      nTrials, gameNr, respTime, trial, t, clickEnabled, nDigits)"> </h1>'))
            ),
          # set up the continue Button
          shiny::fixedRow(
            shiny::column(6, shinyjs::hidden(
              shiny::actionButton("continueBandit",
                                  label = buttonLabel,
                                  style =  "margin-top: 2em; margin-left: 5.3em; margin-bottom: 3em" )),
              offset =  4))))
      )

  }

}


# fiveArmedBanditPage =====================================


#' @describeIn twoArmedBanditPage A five armed bandit version
fiveArmedBanditPage <- function(ctrlValsList, gameTitle = "Game ",
                                nTrials = NULL, clickCounter = "Clicks Remaining",
                                gameTitlePractice = "Practice Game",
                                withPracticeGame = TRUE,
                                pointTitle = "Points Earned", nGames = 10,
                                buttonLabel = "Click to Continue to next Game"){

  if (isTRUE(withPracticeGame)){
    list(
      shiny::fixedRow(

        # call the neGame() javascript function to prepare objects
        shiny::tag("script", 'newGame();'),

        # set up written info displayed above the boxes
        shiny::column(12,
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::column(12, align = "left",
                                                    shiny::h2(
                                                      ifelse(ctrlValsList$banditGame == 1,
                                                             gameTitlePractice,
                                                             paste(gameTitle,
                                                                   ctrlValsList$banditGame - 1,
                                                                   "of ", nGames))))),
                      shiny::fixedRow(
                        shiny::column(6, align="right",
                                      shiny::p(id = "clicksRemaining",
                                               nTrials)),
                        shiny::column(6, align="left",
                                      shiny::h3(class = "upperParams",
                                                clickCounter))
                      ),

                      shiny::fixedRow(
                        shiny::column(6, align="right",
                                      shiny::p(id = "pointCounter", "0")),
                        shiny::column(6, align="left",
                                      shiny::h3(class = "upperParams",
                                                pointTitle))
                      ),

                      shiny::fixedRow(shiny::br()),

                      # set up logic to update the boxes via javascript.
                      # Arguments passed to js come from newGame() js function and
                      # the sendCustomMessage called just before this function
                      shiny::fixedRow(
                        shiny::column(1, align="center",
                                      shiny::HTML('<h1 id="emptySpace1" class="emptySpace">Place</h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck1" class="decks" onclick="updateBanditFiveArms(\'deck1\',
                                                  \'deck2\', \'deck3\', \'deck4\', \'deck5\', \'pointCounter\',
                                                  \'clicksRemaining\', enOne, enTwo, enThree, enFour, enFive, ind, 1,
                                                  outcome, outcomeCum, selection, nTrials, gameNr, respTime, trial, t,
                                                  clickEnabled, nDigits)"> </h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck2" class="decks" onclick="updateBanditFiveArms(\'deck2\',
                                                  \'deck1\', \'deck3\', \'deck4\', \'deck5\', \'pointCounter\',
                                                  \'clicksRemaining\', enTwo, enOne, enThree, enFour, enFive, ind, 2,
                                                  outcome, outcomeCum, selection, nTrials, gameNr, respTime, trial, t,
                                                  clickEnabled, nDigits)"> </h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck3" class="decks" onclick="updateBanditFiveArms(\'deck3\',
                                                  \'deck2\', \'deck1\', \'deck4\', \'deck5\', \'pointCounter\',
                                                  \'clicksRemaining\', enThree, enOne, enTwo, enFour, enFive, ind, 3,
                                                  outcome, outcomeCum, selection, nTrials, gameNr, respTime, trial, t,
                                                  clickEnabled, nDigits)"> </h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck4" class="decks" onclick="updateBanditFiveArms(\'deck4\',
                                                  \'deck3\', \'deck2\', \'deck1\', \'deck5\', \'pointCounter\',
                                                  \'clicksRemaining\', enFour, enThree, enOne, enTwo, enFive, ind, 4,
                                                  outcome, outcomeCum, selection, nTrials, gameNr, respTime, trial, t,
                                                  clickEnabled, nDigits)"> </h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck5" class="decks" onclick="updateBanditFiveArms(\'deck5\',
                                                  \'deck3\', \'deck2\', \'deck1\', \'deck4\', \'pointCounter\',
                                                  \'clicksRemaining\', enFive, enThree, enOne, enTwo, enFour, ind, 5,
                                                  outcome, outcomeCum, selection, nTrials, gameNr, respTime, trial, t,
                                                  clickEnabled, nDigits)"> </h1>')),
                        shiny::column(1, align="center",
                                      shiny::HTML('<h1 id="emptySpace2" class="emptySpace">Place</h1>'))
                        ),

                      # set up the continue Button
                      shiny::fixedRow(
                        shiny::column(6,
                                      shinyjs::hidden(
                                        shiny::actionButton("continueBandit",
                                                            label = buttonLabel,
                                                            style = "margin-top: 2em;
                                                            margin-left: 5.3em;
                                                            margin-bottom: 3em" )),
                                      offset = 4))))
                        )

  } else {

    list(
      shiny::fixedRow(

        # call the neGame() javascript function to prepare objects
        shiny::tag("script", 'newGame();'),

        # set up written info displayed above the boxes
        shiny::column(12, shiny::fixedRow(
          shiny::br()),
          shiny::fixedRow(shiny::column(12, align = "left",shiny::h2(
            paste(gameTitle, ctrlValsList$banditGame, "of ", nGames)))),
          shiny::fixedRow(
            shiny::column(6, align="right", shiny::p(id = "clicksRemaining",
                                                     paste(nTrials))),
            shiny::column(6, align="left", shiny::h3(class = "upperParams",
                                                     clickCounter))),
          shiny::fixedRow(
            shiny::column(6, align="right", shiny::p(id = "pointCounter",
                                                     "0")),
            shiny::column(6, align="left", shiny::h3(class = "upperParams",
                                                     pointTitle))
          ),
          shiny::fixedRow(shiny::br()),
          # set up logic to update the boxes via javascript. Arguments passed to
          # js come from newGame() js function and
          # the sendCustomMessage called just before this function
          shiny::fixedRow(
            shiny::column(1, align="center",
                          shiny::HTML('<h1 id="emptySpace1" class="emptySpace">Place</h1>')),
            shiny::column(2, align="center",
                          shiny::HTML('<h1 id="deck1" class="decks" onclick="updateBanditFiveArms(\'deck1\',
                                      \'deck2\', \'deck3\', \'deck4\', \'deck5\', \'pointCounter\',
                                      \'clicksRemaining\', enOne, enTwo, enThree, enFour, enFive, ind, 1,
                                      outcome, outcomeCum, selection, nTrials, gameNr, respTime, trial, t,
                                      clickEnabled, nDigits)"> </h1>')),
            shiny::column(2, align="center",
                          shiny::HTML('<h1 id="deck2" class="decks" onclick="updateBanditFiveArms(\'deck2\',
                                      \'deck1\', \'deck3\', \'deck4\', \'deck5\', \'pointCounter\',
                                      \'clicksRemaining\', enTwo, enOne, enThree, enFour, enFive, ind, 2,
                                      outcome, outcomeCum, selection, nTrials, gameNr, respTime, trial, t,
                                      clickEnabled, nDigits)"> </h1>')),
            shiny::column(2, align="center",
                          shiny::HTML('<h1 id="deck3" class="decks" onclick="updateBanditFiveArms(\'deck3\',
                                      \'deck2\', \'deck1\', \'deck4\', \'deck5\', \'pointCounter\',
                                      \'clicksRemaining\', enThree, enOne, enTwo, enFour, enFive, ind, 3,
                                      outcome, outcomeCum, selection, nTrials, gameNr, respTime, trial, t,
                                      clickEnabled, nDigits)"> </h1>')),
            shiny::column(2, align="center",
                          shiny::HTML('<h1 id="deck4" class="decks" onclick="updateBanditFiveArms(\'deck4\',
                                      \'deck3\', \'deck2\', \'deck1\', \'deck5\', \'pointCounter\',
                                      \'clicksRemaining\', enFour, enThree, enOne, enTwo, enFive, ind, 4,
                                      outcome, outcomeCum, selection, nTrials, gameNr, respTime, trial, t,
                                      clickEnabled, nDigits)"> </h1>')),
            shiny::column(2, align="center",
                          shiny::HTML('<h1 id="deck5" class="decks" onclick="updateBanditFiveArms(\'deck5\',
                                      \'deck3\', \'deck2\', \'deck1\', \'deck4\', \'pointCounter\',
                                      \'clicksRemaining\', enFive, enThree, enOne, enTwo, enFour, ind, 5,
                                      outcome, outcomeCum, selection, nTrials, gameNr, respTime, trial, t,
                                      clickEnabled, nDigits)"> </h1>')),
            shiny::column(1, align="center",
                          shiny::HTML('<h1 id="emptySpace2" class="emptySpace">Place</h1>'))
            ),
          # set up the continue Button
          shiny::fixedRow(
            shiny::column(6, shinyjs::hidden(
              shiny::actionButton("continueBandit",
                                  label = buttonLabel,
                                  style =  "margin-top: 2em; margin-left: 5.3em; margin-bottom: 3em" )),
              offset =  4))))
        )

  }

}


# sixArmedBanditPage =====================================

#' @describeIn twoArmedBanditPage A six armed bandit version
sixArmedBanditPage <- function(ctrlValsList, gameTitle = "Game ",
                               nTrials = NULL, clickCounter = "Clicks Remaining",
                               gameTitlePractice = "Practice Game",
                               withPracticeGame = TRUE,
                               pointTitle = "Points Earned", nGames = 10,
                               buttonLabel = "Click to Continue to next Game"){

  if (isTRUE(withPracticeGame)){
    list(
      shiny::fixedRow(

        # call the neGame() javascript function to prepare objects
        shiny::tag("script", 'newGame();'),

        # set up written info displayed above the boxes
        shiny::column(12,
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::column(12, align = "left",
                                                    shiny::h2(
                                                      ifelse(ctrlValsList$banditGame == 1,
                                                             gameTitlePractice,
                                                             paste(gameTitle,
                                                                   ctrlValsList$banditGame - 1,
                                                                   "of ", nGames))))),
                      shiny::fixedRow(
                        shiny::column(6, align="right",
                                      shiny::p(id = "clicksRemaining",
                                               nTrials)),
                        shiny::column(6, align="left",
                                      shiny::h3(class = "upperParams",
                                                clickCounter))
                      ),

                      shiny::fixedRow(
                        shiny::column(6, align="right",
                                      shiny::p(id = "pointCounter", "0")),
                        shiny::column(6, align="left",
                                      shiny::h3(class = "upperParams",
                                                pointTitle))
                      ),

                      shiny::fixedRow(shiny::br()),

                      # set up logic to update the boxes via javascript. Arguments passed to js come from newGame() js function and
                      # the sendCustomMessage called just before this function
                      shiny::fixedRow(
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck1" class="decks" onclick="updateBanditSixArms(\'deck1\',
                                                  \'deck2\', \'deck3\', \'deck4\', \'deck5\', \'deck6\', \'pointCounter\',
                                                  \'clicksRemaining\', enOne, enTwo, enThree, enFour, enFive, enSix, ind, 1,
                                                  outcome, outcomeCum, selection, nTrials, gameNr, respTime, trial, t,
                                                  clickEnabled, nDigits)"> </h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck2" class="decks" onclick="updateBanditSixArms(\'deck2\',
                                                  \'deck1\', \'deck3\', \'deck4\', \'deck5\', \'deck6\', \'pointCounter\',
                                                  \'clicksRemaining\', enTwo, enOne, enThree, enFour, enFive, enSix, ind, 2,
                                                  outcome, outcomeCum, selection, nTrials, gameNr, respTime, trial, t,
                                                  clickEnabled, nDigits)"> </h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck3" class="decks" onclick="updateBanditSixArms(\'deck3\',
                                                  \'deck2\', \'deck1\', \'deck4\', \'deck5\', \'deck6\', \'pointCounter\',
                                                  \'clicksRemaining\', enThree, enOne, enTwo, enFour, enFive, enSix, ind, 3,
                                                  outcome, outcomeCum, selection, nTrials, gameNr, respTime, trial, t,
                                                  clickEnabled, nDigits)"> </h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck4" class="decks" onclick="updateBanditSixArms(\'deck4\',
                                                  \'deck3\', \'deck2\', \'deck1\', \'deck5\', \'deck6\', \'pointCounter\',
                                                  \'clicksRemaining\', enFour, enThree, enOne, enTwo, enFive, enSix, ind, 4,
                                                  outcome, outcomeCum, selection, nTrials, gameNr, respTime, trial, t,
                                                  clickEnabled, nDigits)"> </h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck5" class="decks" onclick="updateBanditSixArms(\'deck5\',
                                                  \'deck2\', \'deck1\', \'deck4\', \'deck3\', \'deck6\', \'pointCounter\',
                                                  \'clicksRemaining\', enFive, enOne, enTwo, enFour, enThree, enSix, ind, 5,
                                                  outcome, outcomeCum, selection, nTrials, gameNr, respTime, trial, t,
                                                  clickEnabled, nDigits)"> </h1>')),
                        shiny::column(2, align="center",
                                      shiny::HTML('<h1 id="deck6" class="decks" onclick="updateBanditSixArms(\'deck6\',
                                                  \'deck3\', \'deck2\', \'deck1\', \'deck5\', \'deck4\', \'pointCounter\',
                                                  \'clicksRemaining\', enSix, enThree, enOne, enTwo, enFive, enFour, ind, 6,
                                                  outcome, outcomeCum, selection, nTrials, gameNr, respTime, trial, t,
                                                  clickEnabled, nDigits)"> </h1>'))
                        ),

                      # set up the continue Button
                      shiny::fixedRow(
                        shiny::column(6,
                                      shinyjs::hidden(
                                        shiny::actionButton("continueBandit",
                                                            label = buttonLabel,
                                                            style = "margin-top: 2em;
                                                            margin-left: 5.3em;
                                                            margin-bottom: 3em" )),
                                      offset =  4))))
                        )

  } else {

    list(
      shiny::fixedRow(

        # call the neGame() javascript function to prepare objects
        shiny::tag("script", 'newGame();'),

        # set up written info displayed above the boxes
        shiny::column(12, shiny::fixedRow(
          shiny::br()),
          shiny::fixedRow(shiny::column(12, align = "left",shiny::h2(
            paste(gameTitle, ctrlValsList$banditGame, "of ", nGames)))),
          shiny::fixedRow(
            shiny::column(6, align="right", shiny::p(id = "clicksRemaining",
                                                     paste(nTrials))),
            shiny::column(6, align="left", shiny::h3(class = "upperParams",
                                                     clickCounter))),
          shiny::fixedRow(
            shiny::column(6, align="right", shiny::p(id = "pointCounter",
                                                     "0")),
            shiny::column(6, align="left", shiny::h3(class = "upperParams",
                                                     pointTitle))
          ),
          shiny::fixedRow(shiny::br()),
          # set up logic to update the boxes via javascript. Arguments passed to
          # js come from newGame() js function and
          # the sendCustomMessage called just before this function
          shiny::fixedRow(
            shiny::column(2, align="center",
                          shiny::HTML('<h1 id="deck1" class="decks" onclick="updateBanditSixArms(\'deck1\',
                                      \'deck2\', \'deck3\', \'deck4\', \'deck5\', \'deck6\', \'pointCounter\',
                                      \'clicksRemaining\', enOne, enTwo, enThree, enFour, enFive, enSix, ind, 1,
                                      outcome, outcomeCum, selection, nTrials, gameNr, respTime, trial, t,
                                      clickEnabled, nDigits)"> </h1>')),
            shiny::column(2, align="center",
                          shiny::HTML('<h1 id="deck2" class="decks" onclick="updateBanditSixArms(\'deck2\',
                                      \'deck1\', \'deck3\', \'deck4\', \'deck5\', \'deck6\', \'pointCounter\',
                                      \'clicksRemaining\', enTwo, enOne, enThree, enFour, enFive, enSix, ind, 2,
                                      outcome, outcomeCum, selection, nTrials, gameNr, respTime, trial, t,
                                      clickEnabled, nDigits)"> </h1>')),
            shiny::column(2, align="center",
                          shiny::HTML('<h1 id="deck3" class="decks" onclick="updateBanditSixArms(\'deck3\',
                                      \'deck2\', \'deck1\', \'deck4\', \'deck5\', \'deck6\', \'pointCounter\',
                                      \'clicksRemaining\', enThree, enOne, enTwo, enFour, enFive, enSix, ind, 3,
                                      outcome, outcomeCum, selection, nTrials, gameNr, respTime, trial, t,
                                      clickEnabled, nDigits)"> </h1>')),
            shiny::column(2, align="center",
                          shiny::HTML('<h1 id="deck4" class="decks" onclick="updateBanditSixArms(\'deck4\',
                                      \'deck3\', \'deck2\', \'deck1\', \'deck5\', \'deck6\', \'pointCounter\',
                                      \'clicksRemaining\', enFour, enThree, enOne, enTwo, enFive, enSix, ind, 4,
                                      outcome, outcomeCum, selection, nTrials, gameNr, respTime, trial, t,
                                      clickEnabled, nDigits)"> </h1>')),
            shiny::column(2, align="center",
                          shiny::HTML('<h1 id="deck5" class="decks" onclick="updateBanditSixArms(\'deck5\',
                                      \'deck2\', \'deck1\', \'deck4\', \'deck3\', \'deck6\', \'pointCounter\',
                                      \'clicksRemaining\', enFive, enOne, enTwo, enFour, enThree, enSix, ind, 5,
                                      outcome, outcomeCum, selection, nTrials, gameNr, respTime, trial, t,
                                      clickEnabled, nDigits)"> </h1>')),
            shiny::column(2, align="center",
                          shiny::HTML('<h1 id="deck6" class="decks" onclick="updateBanditSixArms(\'deck6\',
                                      \'deck3\', \'deck2\', \'deck1\', \'deck5\', \'deck4\', \'pointCounter\',
                                      \'clicksRemaining\', enSix, enThree, enOne, enTwo, enFive, enFour, ind, 6,
                                      outcome, outcomeCum, selection, nTrials, gameNr, respTime, trial, t,
                                      clickEnabled, nDigits)"> </h1>'))
            ),
          # set up the continue Button
          shiny::fixedRow(
            shiny::column(6, shinyjs::hidden(
              shiny::actionButton("continueBandit",
                                  label = buttonLabel,
                                  style =  "margin-top: 2em; margin-left: 5.3em; margin-bottom: 3em" )),
              offset =  4))))
          )

  }

}

