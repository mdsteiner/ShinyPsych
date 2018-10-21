#' Create an HTML Page to Display and Play the BART
#'
#' Create an HTML page by using the shiny package and send balloon information
#' to javascript. To get the ctrlVals list call \code{\link{createCtrlList}} and
#' to for the balloonList call \code{\link{createBartList}}. The function only
#' works from within a shiny app (i.e. html code will also be produced when
#' this function is called from outside a shiny app but nothing will be
#' displayed).
#' @param ctrlVals list of reactive values. Controls the flow through the
#'  Experiment. Can be created with \code{\link{createCtrlList}}.
#' @param session The session object from a shiny app.
#' @param balloonList list. Contains information about the balloons, such as
#'  the pop values. Can be created with \code{\link{createBartList}}.
#' @param RDrawBoundary logical. If TRUE (default) an outer boundary is drawn
#'  to indicate the maximum possible pump value (not the actual pop
#'  value of that respective balloon!).
#'
#' @return An html page displayed in the shiny app.
#' @export
createBartPage <- function(ctrlVals, session, balloonList, RDrawBoundary = TRUE){

  # maximum value the balloon can take (used to draw the outer boundary)
  pop.max <- balloonList$max.pop[balloonList$balloonIds[ctrlVals$balloon]]

  # send data from server to browser
  session$sendCustomMessage(type='maxPopHandler', c(pop.max, RDrawBoundary))

  ctrlVals$proceed <- 1

  # Main display while the game is in progress
  if (ctrlVals$pop == 0 & ctrlVals$saveballoon == 0) {

    # Send the maximum pumps for this balloon to the JavaScript
    session$sendCustomMessage(type='maxPumpHandler',
                              balloonList$PopVals[ctrlVals$balloon])

    return(
      list(
        # Main Display: Contains both a pump and a save button
        shiny::fixedRow(
          shiny::column(12,
                        shiny::fixedRow(
                          shiny::column(3, align="center", shiny::h3(shiny::tag("i", "Balloon"))),
                          shiny::column(6, align="center", shiny::h3(shiny::tag("i", "Pumps"))),
                          shiny::column(3, align="center", shiny::h3(shiny::tag("i", "Points")))
                 ),
                 shiny::fixedRow(
                   shiny::column(3, align="center",
                                 shiny::p(style = "font-size:36px",
                                          paste0(ctrlVals$balloon, " of ",
                                                 balloonList$nBalloons))),
                   shiny::column(6, align="center",
                                 shiny::p(style = "font-size:36px",
                                          id = "pumpCounter", "0")), # This is updated via JavaScript
                   shiny::column(3, align="center",
                                 shiny::p(style = "font-size:36px",
                                          paste(ctrlVals$points.cum)))
                 ),
                 shiny::fixedRow(
                   shiny::column(12, align="center",
                                 shiny::tag("canvas",
                                            list(id ="balloonCanvas",
                                                 width = "500",
                                                 height = "300")))),

                 # The Balloon is also later updated via JavaScript
                 shiny::tag("script",
                             paste0('var jsCanvas = document.getElementById("balloonCanvas");
                             var ctx = jsCanvas.getContext("2d");
                             var t = new Date().getTime();
                             jsRedrawBalloon("',
                             balloonList$balloonColor[ctrlVals$balloon], '");')),

                 shiny::fixedRow(
                   shiny::column(12, align="center",
                                 shiny::actionButton("pump",
                                                     label = "Pump The Balloon",
                                                     onclick = paste0("jsPump(\"",
                                                                      balloonList$balloonColor[ctrlVals$balloon],
                                                                      "\")"))),
                   shiny::column(12, shiny::br()),
                   shiny::column(12, align="center",
                                 shiny::actionButton("saveballoon",
                                                     label = "Save!",
                                                     onclick = "jsSaveBalloon()"))
                 )))))
  }

  # Main display when the balloon popped
  else if (ctrlVals$pop == 1) {
    return(
      list(
        # Main Display: Contains both a pump and a save button
        shiny::fixedRow(
          shiny::column(12,
                        shiny::fixedRow(
                          shiny::column(3, align="center",
                                        shiny::h3(shiny::tag("i", "Balloon"))),
                          shiny::column(6, align="center",
                                        shiny::h3(shiny::tag("i", "Pumps"))),
                          shiny::column(3, align="center",
                                        shiny::h3(shiny::tag("i", "Points")))
                 ),
                 shiny::fixedRow(
                   shiny::column(3, align="center",
                                 shiny::p(style = "font-size:36px",
                                          paste0(ctrlVals$balloon, " of ",
                                                 balloonList$nBalloons))),
                   shiny::column(6, align="center",
                                 shiny::p(style = "font-size:36px",
                                          id = "pumpCounter", ctrlVals$pumps)),
                   shiny::column(3, align="center",
                                 shiny::p(style = "font-size:36px",
                                          paste(ctrlVals$points.cum)))
                 ),
                 shiny::fixedRow(
                   shiny::column(12, align="center",
                                 shiny::tag("canvas",
                                            list(id ="balloonCanvas",
                                                 width = "500",
                                                 height = "300")))),

                 shiny::tag("script", 'var c = document.getElementById("balloonCanvas");
                             var ctx = c.getContext("2d");
                             jsRedrawBalloon("red");
                             jsDelayButton();
                             '),

                 shiny::fixedRow(
                   shiny::column(12, align="center",
                                 shiny::actionButton("nextballoon",
                                                     label = "Go to the Next Balloon!",
                                                     onclick = "jsNewBalloon()"))
                 )))))
  }

  # Main display when the balloon is saved
  else if (ctrlVals$saveballoon == 1) {
    return(
      list(
        shiny::fixedRow(
          shiny::column(12,
                        shiny::fixedRow(
                          shiny::column(3, align="center",
                                        shiny::h3(shiny::tag("i", "Balloon"))),
                          shiny::column(6, align="center",
                                        shiny::h3(shiny::tag("i", "Pumps"))),
                          shiny::column(3, align="center",
                                        shiny::h3(shiny::tag("i", "Points")))
                 ),
                 shiny::fixedRow(
                   shiny::column(3, align="center",
                                 shiny::p(style = "font-size:36px",
                                          paste0(ctrlVals$balloon, " of ",
                                                 balloonList$nBalloons))),
                   shiny::column(6, align="center",
                                 shiny::p(style = "font-size:36px",
                                          id = "pumpCounter", ctrlVals$pumps)),
                   shiny::column(3, align="center",
                                 shiny::p(style = "font-size:36px",
                                          paste(ctrlVals$points.cum)))
                 ),
                 shiny::fixedRow(
                   shiny::column(12, align="center",
                                 shiny::tag("canvas", list(id ="balloonCanvas",
                                                           width = "500",
                                                           height = "300")))),

                 shiny::tag("script", 'var c = document.getElementById("balloonCanvas");
                                       var ctx = c.getContext("2d");
                                       jsRedrawBalloon("green");
                                       '),

                 shiny::fixedRow(
                   shiny::column(12, align="center",
                                 shiny::actionButton("nextballoon",
                                                     label = "Go to the Next Balloon!",
                                                     onclick = "jsNewBalloon()"))
                 )))))
  }
}
