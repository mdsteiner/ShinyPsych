
sixOptDfePage <- function(ctrlValsList = ctrlVals, gambleTitle = "Gamble",
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

