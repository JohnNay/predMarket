figRef <- local({
  tag <- numeric()
  created <- logical()
  used <- logical()
  function(label, caption, prefix="Fig.", sep=".",
           prefix.highlight="**") {
    i <- which(names(tag) == label)
    if(length(i) == 0){
      i <- length(tag) + 1
      tag <<- c(tag, i)
      names(tag)[length(tag)] <<- label
      used <<- c(used, FALSE)
      names(used)[length(used)] <<- label
      created <<- c(created, FALSE)
      names(created)[length(created)] <<- label
    }
    if(!missing(caption)){
      created[label] <<- TRUE
      paste0(prefix.highlight, prefix, " ", i, sep, prefix.highlight, " ", caption)
    } else {
      used[label] <<- TRUE
      paste(prefix, tag[label])
    }
  }
})
# adapted from a function on this page: http://galahad.well.ox.ac.uk/repro/
rinline <- function(code){
  sprintf('`r %s`', code)
}

tabRef <- local({
  tag <- numeric()
  created <- logical()
  used <- logical()
  function(label, caption, prefix = "Table", 
           sep = ".", prefix.highlight = "**") {
    i <- which(names(tag) == label)
    if (length(i) == 0) {
      i <- length(tag) + 1
      tag <<- c(tag, i)
      names(tag)[length(tag)] <<- label
      used <<- c(used, FALSE)
      names(used)[length(used)] <<- label
      created <<- c(created, FALSE)
      names(created)[length(created)] <<- label
    }
    if (!missing(caption)) {
      created[label] <<- TRUE
      paste0(prefix.highlight, prefix, " ", i, sep, prefix.highlight, 
             " ", caption)
    } else {
      used[label] <<- TRUE
      paste(prefix, tag[label])
    }
  }
})