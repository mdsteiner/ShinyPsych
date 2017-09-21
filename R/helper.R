
# helper for saveData function
.convertNull <- function(x){
  if (is.null(x)){
    val <- NA
  } else if (length(x) == 0){
    val <- NA
  } else {
    val <- x
  }
  val
}

# helper for create_page_list
.revArgsGsub <- function(x, pattern, replacement){
  # reverse input arguments for use in apply functions
  gsub(pattern = pattern, replacement = replacement, x = x)
}

# helper for create_page
.callTag <- function(index, pageList){
  # identify tag and call it with the appropriate arguments
  # define lists of possible inputs
  tagList <- c("h1", "h2", "h3", "h4", "h5", "h6", "p")
  multiList <- c("checkboxGroupInput", "radioButtons")
  textList <- c("passwordInput", "textInput")
  nusliList <- c("sliderInput", "numericInput")

  # check which function is matched to ensure correct use of arguments
  if (any(tagList == pageList$type[index])){
    if (substr(pageList$id[index], nchar(pageList$id[index]) - 1,
               nchar(pageList$id[index])) == "NA"){
      tempid <- NULL
    } else {
      tempid <- pageList$id[index]
    }

    # prints text, such as headers and paragraphs
    getExportedValue("shiny", pageList$type[index])(pageList$text[index],
                                                    width = pageList$width[index],
                                                    id = tempid)

  } else if (any(multiList == pageList$type[index])){
    # creates input objects such as radio buttons and multi check boxes.
    getExportedValue("shiny", pageList$type[index])(inputId = pageList$id[index],
                                                    choices = pageList$choices[[index]],
                                                    label = pageList$text[index],
                                                    selected = character(0),
                                                    width = pageList$width[index],
                                                    inline = pageList$inline[index])

  } else if (any(textList == pageList$type[index])){
    # creates input objects such as password or text input.
    getExportedValue("shiny", pageList$type[index])(pageList$id[index],
                                                    label = pageList$text[index],
                                                    placeholder = pageList$placeholder[index],
                                                    width = pageList$width[index])

  } else if (any(nusliList == pageList$type[index])){
    # creates input objects such as numeric input or slider
    getExportedValue("shiny", pageList$type[index])(pageList$id[index],
                                                    label = pageList$text[index],
                                                    min = pageList$min[index],
                                                    max = pageList$max[index],
                                                    value = pageList$choices[index],
                                                    width = pageList$width[index])

  } else if (pageList$type[index] == "img"){
    # post an image from a given source
    if (isTRUE(pageList$defaultList)){
      shiny::addResourcePath('pictures', system.file('extdata', package='ShinyPsych'))
    }
    if (substr(pageList$id[index], nchar(pageList$id[index]) - 1,
               nchar(pageList$id[index])) == "NA"){
      tempid <- NULL
    } else {
      tempid <- pageList$id[index]
    }
    getExportedValue("shiny", pageList$type[index])(src = pageList$text[index],
                                                    width = pageList$width[index],
                                                    height = pageList$height[index],
                                                    id = tempid)

  }else if (pageList$type[index] == "HTML"){

    # this is apropriate if the text is actually written html code
    shiny::HTML(pageList$text[index])

  } else if (pageList$type[index] == "checkboxInput"){
    # creates a checkbox that yields FALSE if unchecked and TRUE if checked
    getExportedValue("shiny", pageList$type[index])(pageList$id[index],
                                                    label = pageList$text[index],
                                                    width = pageList$width[index])

  } else if (pageList$type[index] == "selectInput"){
    # creates a dropdown list from which an input can be selected
    getExportedValue("shiny", pageList$type[index])(inputId = pageList$id[index],
                                                    choices = pageList$choices[[index]],
                                                    label = pageList$text[index],
                                                    selected = character(0),
                                                    width = pageList$width[index],
                                                    multiple = pageList$inline[index])

  } else if (pageList$type[index] == "textAreaInput"){
    getExportedValue("shiny", pageList$type[index])(pageList$id[index],
                                                    label = pageList$text[index],
                                                    placeholder = pageList$placeholder[index],
                                                    width = pageList$width[index],
                                                    height = pageList$height[index])

  } else if (pageList$type[index] == "dateInput"){
    val_temp <- if(is.na(pageList$placeholder[index])) NULL else pageList$placeholder[index]
    min_temp <- if(is.na(pageList$min[index])) NULL else pageList$min[index]
    max_temp <- if(is.na(pageList$max[index])) NULL else pageList$max[index]
    time_int <- c("month", "year", "decade")
    startview_temp <- if(is.na(pageList$choices[index])){
      "month"
    } else {
      time_int[pageList$choices[[index]]]
    }
    getExportedValue("shiny", pageList$type[index])(pageList$id[index],
                                                    label = pageList$text[index],
                                                    value = val_temp,
                                                    width = pageList$width[index],
                                                    min = min_temp,
                                                    max = max_temp,
                                                    startview = startview_temp)

  } else {
    # give exact value that raised the error
    stop("Couldn't identify function. See documentation for valid inputs. Note that spelling must match shiny functions!")
  }
}

# helper for onInputEnable
.checkInputFun <- function(Index, inList, checkType, charNum, checkInput){

  if (checkType[Index] == "isTRUE"){

    checkTemp <- !is.null(inList[[checkInput[Index]]]) &&
      isTRUE(inList[[checkInput[Index]]])

  } else if (checkType[Index] == "is.null"){

    checkTemp <- !is.null(inList[[checkInput[Index]]])

  } else if (checkType[Index] == "nchar"){

    checkTemp <- !is.null(inList[[checkInput[Index]]]) &&
      nchar(inList[[checkInput[Index]]]) >= charNum

  } else {

    stop(paste(checkType[Index],
               "is no valid checkType. Use one of \"isTRUE\", \"is.null\", \"nchar\""))
  }

  checkTemp

}

# helpers for createBanditList
# sample values from normal distributions
.getNormVals2 <- function(colIndex, rowIndex, parList, roundDigits){
  round(rnorm(parList$nTrials[rowIndex], mean = parList$mean[rowIndex, colIndex],
              sd = parList$sd[rowIndex, colIndex]), roundDigits)
}

.getNormVals1 <- function(gameIndex, distributionList, Arms, nDigits){
  cbind(sapply(seq_len(Arms), .getNormVals2, parList = distributionList,
               rowIndex = gameIndex, roundDigits = nDigits))
}

# sample values form exponential distributions
.getExpVals2 <- function(colIndex, rowIndex, parList, roundDigits){
  round(rexp(parList$nTrials[rowIndex], rate = parList$rate[rowIndex, colIndex]),
        roundDigits)
}

.getExpVals1 <- function(gameIndex, distributionList, Arms, nDigits){
  cbind(sapply(seq_len(Arms), .getExpVals2, parList = distributionList,
               rowIndex = gameIndex, roundDigits = nDigits))
}

# sample values from uniform distributions
.getUnifVals2 <- function(colIndex, rowIndex, parList, roundDigits){
  round(runif(parList$nTrials[rowIndex], min = parList$min[rowIndex, colIndex],
              max = parList$max[rowIndex, colIndex]), roundDigits)
}

.getUnifVals1 <- function(gameIndex, distributionList, Arms, nDigits){
  cbind(sapply(seq_len(Arms), .getUnifVals2, parList = distributionList,
               rowIndex = gameIndex, roundDigits = nDigits))
}

# sample values from beta distributions
.getBetaVals2 <- function(colIndex, rowIndex, parList, roundDigits){
  round(rbeta(parList$nTrials[rowIndex],
              shape1 = parList$shape1[rowIndex, colIndex],
              shape2 = parList$shape2[rowIndex, colIndex],
              ncp = parList$ncp[rowIndex, colIndex]), roundDigits)
}

.getBetaVals1 <- function(gameIndex, distributionList, Arms, nDigits){
  cbind(sapply(seq_len(Arms), .getBetaVals2, parList = distributionList,
               rowIndex = gameIndex, roundDigits = nDigits))
}

# sample values from ex-gaussian distributions
.getExgaussVals2 <- function(colIndex, rowIndex, parList, roundDigits){
  round(retimes::rexgauss(parList$nTrials[rowIndex],
                          mu = parList$mu[rowIndex, colIndex],
                          sigma = parList$sigma[rowIndex, colIndex],
                          tau = parList$tau[rowIndex, colIndex],
                          positive = parList$positive[rowIndex, colIndex]),
        roundDigits)
}

.getExgaussVals1 <- function(gameIndex, distributionList, Arms, nDigits){
  cbind(sapply(seq_len(Arms), .getExgaussVals2, parList = distributionList,
               rowIndex = gameIndex, roundDigits = nDigits))
}

# sample values from different distributions
.getDiffDistVals2 <- function(colIndex, rowIndex, parList, roundDigits){
  if (parList$distributionType[rowIndex, colIndex] == "normal"){

    round(rnorm(parList$nTrials[rowIndex], mean = parList$mean[rowIndex, colIndex],
                sd = parList$sd[rowIndex, colIndex]), roundDigits)

  } else if (parList$distributionType[rowIndex, colIndex] == "exp"){

    round(rexp(parList$nTrials[rowIndex], rate = parList$rate[rowIndex, colIndex]),
          roundDigits)

  } else if (parList$distributionType[rowIndex, colIndex] == "unif"){

    round(runif(parList$nTrials[rowIndex], min = parList$min[rowIndex, colIndex],
                max = parList$max[rowIndex, colIndex]), roundDigits)

  } else if (parList$distributionType[rowIndex, colIndex] == "beta") {

    round(rbeta(parList$nTrials[rowIndex],
                shape1 = parList$shape1[rowIndex, colIndex],
                shape2 = parList$shape2[rowIndex, colIndex],
                ncp = parList$ncp[rowIndex, colIndex]), roundDigits)

  } else if (parList$distributionType[rowIndex, colIndex] == "exgauss"){

    round(retimes::rexgauss(parList$nTrials[rowIndex],
                            mu = parList$mu[rowIndex, colIndex],
                            sigma = parList$sigma[rowIndex, colIndex],
                            tau = parList$tau[rowIndex, colIndex],
                            positive = parList$positive[rowIndex, colIndex]),
          roundDigits)

  } else {
    stop(paste(parList$distributionType[rowIndex, colIndex], "is no valid distribution type for this function. Must be one of \"normal\", \"exp\", \"unif\", \"beta\" or \"exgauss\""))
  }

}

# sample values from different distributions for each option
.getDiffDistVals1 <- function(gameIndex, distributionList, Arms, nDigits){
  cbind(sapply(seq_len(Arms), .getDiffDistVals2, parList = distributionList,
               rowIndex = gameIndex, roundDigits = nDigits))
}

# sample values from same distribution
.getSameDistVals <- function(dList, armNumber, rDigits){
  if (dList$distributionType == "normal"){

    lapply(seq_along(dList$nTrials), .getNormVals1,
           distributionList = dList, Arms = armNumber,
           nDigits = rDigits)

  } else if (dList$distributionType == "exp"){

    lapply(seq_along(dList$nTrials), .getExpVals1,
           distributionList = dList, Arms = armNumber,
           nDigits = rDigits)

  } else if (dList$distributionType == "unif"){

    lapply(seq_along(dList$nTrials), .getUnifVals1,
           distributionList = dList,Arms = armNumber,
           nDigits = rDigits)

  } else if (dList$distributionType == "beta") {

    lapply(seq_along(dList$nTrials), .getBetaVals1,
           distributionList = dList, Arms = armNumber,
           nDigits = rDigits)

  } else if (dList$distributionType == "exgauss"){

    lapply(seq_along(dList$nTrials), .getExgaussVals1,
           distributionList = dList, Arms = armNumber,
           nDigits = rDigits)

  } else {
    stop(paste(dList$distributionType, "is no valid distribution type for this function. Must be one of \"normal\", \"exp\", \"unif\", \"beta\" or \"exgauss\""))
  }

}

# helper for createBartList

.getPopVals <- function(index, distributionList){

  if (distributionList$distributionType[index] == "normal"){

    tempVals <- round(rnorm(distributionList$nBalloons[index],
                mean = distributionList$mean[index],
                sd = distributionList$sd[index]))

    tempVals[tempVals > distributionList$max.pop[index]] <- distributionList$max.pop[index]

  } else if (distributionList$distributionType[index] == "exp"){

    tempVals <- round(rexp(distributionList$nBalloons[index],
               rate = distributionList$rate[index]))

    tempVals[tempVals > distributionList$max.pop[index]] <- distributionList$max.pop[index]

  } else if (distributionList$distributionType[index] == "unif"){

    tempVals <- round(runif(distributionList$nBalloons[index],
                min = distributionList$min[index],
                max = distributionList$max[index]))

    tempVals[tempVals > distributionList$max.pop[index]] <- distributionList$max.pop[index]

  } else if (distributionList$distributionType[index] == "beta") {

    tempVals <- round(rbeta(distributionList$nBalloons[index],
                shape1 = distributionList$shape1[index],
                shape2 = distributionList$shape2[index],
                ncp = distributionList$ncp[index]))

    tempVals[tempVals > distributionList$max.pop[index]] <- distributionList$max.pop[index]

  } else if (distributionList$distributionType[index] == "exgauss"){

    tempVals <- round(retimes::rexgauss(distributionList$nBalloons[index],
                            mu = distributionList$mu[index],
                            sigma = distributionList$sigma[index],
                            tau = distributionList$tau[index],
                            positive = distributionList$positive[index]))

    tempVals[tempVals > distributionList$max.pop[index]] <- distributionList$max.pop[index]
    tempVals[tempVals < distributionList$min.pop[index]] <- distributionList$min.pop[index]

  } else {
    stop(paste(distributionList$distributionType[index], "is no valid distribution type for this function. Must be one of \"normal\", \"exp\", \"unif\", \"beta\" or \"exgauss\""))
  }

  tempVals

}


# helper for createDfeList
.getDfeSamples <- function(gambleInd, p.ind, o.ind, df, nOpt, nVals){

  lapply(seq_len(nOpt), .drawSamples, Vals = nVals, gInd = gambleInd,
         o.ind.vec = o.ind, p.ind.vec = p.ind, g.df = df)

}

.drawSamples <- function(optInd, gInd, Vals, o.ind.vec, p.ind.vec, g.df){

  outc.temp <- unlist(unname(g.df[gInd, o.ind.vec[grepl(paste0("g", optInd),
                                                        o.ind.vec)]]))

  outc.temp <- outc.temp[!is.na(outc.temp)]

  prob.temp <- unlist(unname(g.df[gInd, p.ind.vec[grepl(paste0("g", optInd),
                                                        p.ind.vec)]]))

  prob.temp <- prob.temp[!is.na(prob.temp)]

  if (sum(prob.temp) != 1){
    stop(paste("In gamble number", gInd, "option number", optInd,
               "probabilities do not add to 1."))
  }

  if (length(outc.temp) == 1){
    temp.vals <- rep(outc.temp, Vals)
  } else {
    temp.vals <- sample(outc.temp, size = Vals, prob = prob.temp, replace = TRUE)
  }

  temp.vals
}


# helper for createDfdList
.getDfdGambles <- function(gambleInd, p.ind, o.ind, df, nOpt, currency, nOutc){


  lapply(seq_len(nOpt), .printDfdGamble, gInd = gambleInd,
         o.ind.vec = o.ind, p.ind.vec = p.ind, g.df = df,
         currency.i = currency, mRows = nOutc)

}


.printDfdGamble <- function(optInd, gInd, o.ind.vec, p.ind.vec, g.df, currency.i,
                           mRows){

  # prepare vectors of outcomes and probabilities
  outc.temp <- unlist(unname(g.df[gInd, o.ind.vec[grepl(paste0("g", optInd),
                                                        o.ind.vec)]]))

  outc.temp <- outc.temp[!is.na(outc.temp)]

  prob.temp <- unlist(unname(g.df[gInd, p.ind.vec[grepl(paste0("g", optInd),
                                                        p.ind.vec)]]))

  prob.temp <- prob.temp[!is.na(prob.temp)]

  outc.temp <- outc.temp[prob.temp != 0]
  prob.temp <- prob.temp[prob.temp != 0]

  if (sum(prob.temp) != 1){
    stop(paste("In gamble number", gInd, "option number", optInd,
               "probabilities do not add to 1."))
  }

  # paste gamble outcomes and probabilities into html tags to create a table
  printedGamble <- paste(
    unlist(lapply(seq_len(mRows),
                  function(x, outc.t, prob.t, curr){
                    if (x <= length(prob.t)){
                      paste0("<tr><td>", outc.t[x], curr, "</td><td>",
                            prob.t[x] * 100, "%</td></tr>")
                    } else {
                      paste("<tr class = 'emptyRow'><td> e </td>",
                            "<td> e </td></tr>")
                    }
                  }, outc.t = outc.temp, prob.t = prob.temp,
                  curr = currency.i)), collapse = "")

  printedGamble
}


# helper for createDdList
.getDdOutcomes <- function(rowInd, df, nOpt, currency){

  lapply(seq_len(nOpt), .printDdTrial, rInd = rowInd,
         dd.df = df, currency.i = currency)

}


.printDdTrial <- function(optInd, rInd, dd.df, currency.i){

  # paste gamble outcomes and probabilities into html tags to create a table
  printedGamble <- paste0("<tr><td>", dd.df[rInd, paste0("o", optInd)],
                          currency.i, "</td></tr><tr><td>",
                          dd.df[rInd, paste0("time", optInd)], "</td></tr>")

  printedGamble
}
