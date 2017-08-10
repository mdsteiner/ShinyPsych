
twoArmedBanditPage <- function(ctrlValsList = ctrlVals, gameTitle = "Game ",
                               nTrials = NULL, clickCounter = "Clicks Remaining",
                               gameTitlePractice = "Practice Game",
                               withPracticeGame = TRUE,
                               startPoint = "0",
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
                                                    shiny::h2(ifelse(ctrlValsList$banditGame == 1,
                                                                     gameTitlePractice,
                                                                     paste(gameTitle, ctrlValsList$banditGame - 1, "of ", nGames))))),
                      shiny::fixedRow(
                        shiny::column(6, align="right", shiny::p(id = "clicksRemaining", nTrials)),
                        shiny::column(6, align="left", shiny::h3(class = "upperParams", clickCounter))
               ),
               
               shiny::fixedRow(
                 shiny::column(6, align="right", shiny::p(id = "pointCounter", startPoint)),
                 shiny::column(6, align="left", shiny::h3(class = "upperParams", pointTitle))
               ),
               
               shiny::fixedRow(shiny::br()),
               
               # set up logic to update the boxes via javascript. Arguments passed to js come from newGame() js function and 
               # the sendCustomMessage called just before this function
               shiny::fixedRow(
                 shiny::column(1, align="center",
                               shiny::HTML('<h1 id="emptySpace1" class="emptySpace">Place</h1>')),
                 shiny::column(5, align="center",
                               shiny::HTML('<h1 id="deck1" class="decks" onclick="updateBanditTwoArms(\'deck1\', \'deck2\',
                                           \'pointCounter\', \'clicksRemaining\', eno, ent, ind, 1, outcome, outcomeCum,
                                           selection, nTrials, gameNr, respTime, trial, t, clickEnabled, nDigits)"> </h1>')),
                 shiny::column(5, align="center",
                               shiny::HTML('<h1 id="deck2" class="decks" onclick="updateBanditTwoArms(\'deck2\', \'deck1\',
                                            \'pointCounter\', \'clicksRemaining\', ent, eno, ind, 2, outcome, outcomeCum,
                                            selection, nTrials, gameNr, respTime, trial, t, clickEnabled, nDigits)"> </h1>')),
                 shiny::column(1, align="center",
                               shiny::HTML('<h1 id="emptySpace2" class="emptySpace">Place</h1>'))),
               
               # set up the continue Button
               shiny::fixedRow(
                 shiny::column(6,
                               shinyjs::hidden(shiny::actionButton("continueBandit", label = buttonLabel,
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
                                                     startPoint)),
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
                                           \'pointCounter\', \'clicksRemaining\', eno, ent, ind, 1, outcome, outcomeCum,
                                           selection, nTrials, gameNr, respTime, trial, t, clickEnabled, nDigits)"> </h1>')),
            shiny::column(5, align="center",
                          shiny::HTML('<h1 id="deck2" class="decks" onclick="updateBanditTwoArms(\'deck2\', \'deck1\',
                                            \'pointCounter\', \'clicksRemaining\', ent, eno, ind, 2, outcome, outcomeCum,
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

