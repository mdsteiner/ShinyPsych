
fourOptDfePage <- function(ctrlValsList = ctrlVals, gambleTitle = "Gamble",
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

