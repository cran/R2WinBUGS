write.model <- function(model, con = "model.bug")
{
  if (is.R()){
    model.text <- attr(model, "source")
    model.text <- sub("^\\s*function\\s*\\(\\s*\\)", "model", model.text)
  } else {
    ## In S-PLUS the source code of a function can be obtained with
    ## as.character(function_name).  This omits the "function_name <- function()" piece
    model.text <- as.character(model)
    model.text <- paste("model", model.text)
  }
  model.text <- gsub("%_%", "", model.text)
  if (!is.R()){
    ## In S-PLUS, scientific notation is different than it is in WinBUGS.
    ## Change the format of any numbers in scientific notation.
    model.text <- replaceScientificNotation(model.text)

    ## remove the "invisible()" line.  
    model.text <- gsub("invisible[ ]*\\([ ]*\\)", "", model.text)
  }
  writeLines(model.text, con = con)
}

replaceScientificNotation <- function(text){
## Change the format of any numbers in "text" that are in S-PLUS 
## scientific notation to WinBUGS scientific notation

  ## First, handle the positive exponents
  ## Find the first instance
  ## Note that the number may or may not have a decimal point.
  sciNoteLoc <- regexpr("[0-9]*\\.{0,1}[0-9]*e\\+0[0-9]{2}", text)
    
  ## For every instance, replace the number
  while(sciNoteLoc > -1){
    sciNoteEnd <- sciNoteLoc + attr(sciNoteLoc, "match.length")-1
    sciNote <- substring(text, sciNoteLoc, sciNoteEnd)
    text <- gsub(sciNote, toSingleS4(sciNote), text)
    sciNoteLoc <- regexpr("[0-9]*\\.{0,1}[0-9]*e\\+0[0-9]{2}", text)
  }

  ## Then, handle the negative exponents
  ## Find the first instance
  sciNoteLoc <- regexpr("[0-9]*.{0,1}[0-9]*e\\-0[0-9]{2}", text)

  ## For every instance, replace the number
  while(sciNoteLoc > -1){
    sciNoteEnd <- sciNoteLoc + attr(sciNoteLoc, "match.length")-1
    sciNote <- substring(text, sciNoteLoc, sciNoteEnd)
    text <- gsub(sciNote, toSingleS4(sciNote), text)
    sciNoteLoc <- regexpr("[0-9]*\\.{0,1}[0-9]*e\\-0[0-9]{2}", text)
  }

  text
}
