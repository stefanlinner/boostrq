brq <- function(...){

  browser()
  ## HUHU das ist sicherlich nicht die eleganteste Lösung
  data <- get("data", envir = parent.frame())
  response <- get("response", envir = parent.frame())

  bl <- as.list(match.call(expand.dots = FALSE))[2][[1]]

  bl <- sapply(bl,
               function(x) {
                 as.character(x)
                 })


  ## HUHU ist das stabil?
  if(is.list(bl)){
    bl.length <- sapply(bl, function(x) {
      length(x)
      })
    bl.multiple <- which(bl.length > 1)
    bl.single <- sapply(bl.multiple, function(x) {
      paste(bl[[x]][2], bl[[x]][1], bl[[x]][3], sep = "")
    })
    bl <- c(unlist(bl[-bl.multiple]), bl.single)
  }

  if(is.matrix(bl)){
    bl <- apply(bl, MARGIN = 2, function(x){
      paste(x[2], x[1], x[3], sep = "")
    })
  }

  assert_vector(bl, strict = TRUE, any.missing = FALSE, all.missing = FALSE)
  assert_character(bl, null.ok = FALSE)

  na.omit(
    model.matrix(
      as.formula(
        paste(response, "~", paste(bl, collapse = " + "))
      ),
      data = data)
  )
}

brqss <- function(...){

}
