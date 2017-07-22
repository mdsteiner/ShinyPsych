# helper functions
library("shiny")

# helper for saveData function
convertNull <- function(x){
  if (is.null(x)){
    val <- NA
  } else {
    val <- x
  }
  val
}

# helper for create_page_list
revArgsGsub <- function(x, pattern, replacement){
  # reverse input arguments for use in apply functions
  gsub(pattern = pattern, replacement = replacement, x = x)
}

# helper for create_page
callTag <- function(index, pageList){
  # identify tag and call it with the appropriate arguments
  
  # define lists of possible inputs
  tagList <- c("h1", "h2", "h3", "h4", "h5", "h6", "p", "emph", "i", "li",
               "small", "strong")
  multiList <- c("checkboxGroupInput", "radioButtons")
  textList <- c("passwordInput", "textInput")
  nusliList <- c("sliderInput", "numericInput")
  
  # check which function is matched to ensure correct use of arguments
  if (any(tagList == pageList$type[index])){
    # prints text, such as headers and paragraphs
    get(pageList$type[index])(pageList$text[index], width = pageList$width[index])
    
  } else if (any(multiList == pageList$type[index])){
    # creates input objects such as radio buttons and multi check boxes.
      get(pageList$type[index])(inputId = pageList$id[index], choices = pageList$choices[[index]],
                                label = pageList$text[index], selected = character(0),
                                width = pageList$width[index], inline = pageList$inline[index])
    
  } else if (any(textList == pageList$type[index])){
    # creates input objects such as password or text input.
    get(pageList$type[index])(pageList$id[index], label = pageList$text[index],
                              placeholder = pageList$placeholder[index],
                              width = pageList$width[index])
    
  } else if (any(nusliList == pageList$type[index])){
    # creates input objects such as numeric input or slider
    get(pageList$type[index])(pageList$id[index], label = pageList$text[index],
                              min = pageList$min[index], max = pageList$max[index],
                              value = pageList$value[index], width = pageList$width[index])
    
  } else if (pageList$type[index] == "img"){
    # post an image from a given source
    get(pageList$type[index])(src = pageList$text[index], width = pageList$width[index],
                              height = pageList$height)
    
  }else if (pageList$type[index] == "html"){
    
    # this is apropriate if the text is actually written html code
    tags$html(pageList$text[index])
    
  } else if (pageList$type[index] == "checkboxInput"){
    # creates a checkbox that yields FALSE if unchecked and TRUE if checked
    get(pageList$type[index])(pageList$id[index], label = pageList$text[index],
                              width = pageList$width[index])
    
  } else if (pageList$type[index] == "selectInput"){
    # creates a dropdown list from which an input can be selected
    get(pageList$type[index])(inputId = pageList$id[index], choices = pageList$choices[[index]],
                              label = pageList$text[index], selected = character(0),
                              width = pageList$width[index], multiple = pageList$inline[index])
    
  } else {
    stop("Couldn't identify function. See documentation for valid inputs. Note that spelling must match shiny functions!")
  }
}
