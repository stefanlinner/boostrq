brq <- function(vars){

  data <- get("data", envir = parent.frame())
  response <- get("response", envir = parent.frame())
  assert_subset(vars, choices = names(data))

  na.omit(
    model.matrix(
      as.formula(
        paste(response, "~", paste(vars, collapse = " + "))
      ),
      data = data)
  )
}

brqss <- function(vars){

}
