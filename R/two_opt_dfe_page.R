
twoOptDfePage <- function(ctrlValsList = ctrlVals, gambleTitle = "Gamble",
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

