#' Create HTML page for users who entered the wrong password
#'
#' If you call \code{\link{checkPassword}} to make sure a user is acutally allowed to
#' participate but they enter a wrong password and are therefore not allowed
#' to participate, you can then send them to this page. It includes a back button
#' to try entering the password again in case of a typo.
#' @return A HTML page that can be displayed by shiny.
#' @export
createWrongPasswordPage <- function(){
    list(
      shiny::br(), shiny::br(), shiny::br(),
      shiny::h2(paste0("You entered a wrong password.")),
      shiny::p("If you do not know the password, you are probably not eligible to participate in this study.
               If you landed on this page due to a typo, click the button below to go back."),
      shiny::actionButton(inputId = "wrongPWBack", label = "Go Back!")
      )
}
