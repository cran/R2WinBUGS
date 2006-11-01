write.model <- function(model, con = "model.bug")
{
  model.text <- attr(model, "source")
  model.text <- sub("^\\s*function\\s*\\(\\s*\\)", "model", model.text)
  model.text <- gsub("%_%", "", model.text)
  writeLines(model.text, con = con)
}
