#' Create HTML Page for Users not Allowed to Participate
#'
#' If you call \code{\link{checkId}} to make sure a user is acutally allowed to
#' participate but he is already in your database and therefore not allowed
#' to participate a second time you can then send him to this page. It does not
#' include a continue button, i.e. you can't navigate any further through the
#' experiment.
#' @param inputList The input object from a shiny app.
#' @param globId string. The global id given to the page on which the participant
#'  id was entered.
#' @param idLabel string. The id label you want the participant to see,
#'  e.g. if this is set to "WorkerId" the participant will (among other things)
#'  see "You entered the WorkerID ...".
#' @param idVar string. The id label specified for the \code{\link[shiny]{textInput}}
#'  in the file given to \code{\link{createPageList}}.
#'
#' @return An html page that can be displayed by shiny.
#' @export
createNotAllowedPage <- function(inputList, globId, idLabel = "WorkerID",
                                 idVar = "workerid"){
    list(
      shiny::br(), shiny::br(), shiny::br(),
      shiny::h2(paste0("You entered the ", idLabel, " ",
                       inputList[[paste0(globId, "_", idVar)]],".")),
      shiny::p("Sorry but you are not eligible for this study because you completed a similar study in the past.",
               id = "notEligible"),
      shiny::p("You may now close this window.")
    )
}
