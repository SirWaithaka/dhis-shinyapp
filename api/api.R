rPython::python.load("api/api.py")

organisationUnits <- function (value=1) {
  
  response <- rPython::python.call("organisationUnits_api", value)
  
  if (is.character(response))
    response <- list(response)
  
  id <- sapply(response, "[[", "id")
  name <- sapply(response, "[[", "name")
  code <- sapply(response, "[[", 1)

  names <- c("code", "name", "id")

  df <- data.frame(code, name, id, stringsAsFactors = FALSE)
  colnames(df) <- names

  return(df)
}

indicators <- function (group=NULL, members=NULL, id=NULL) {
  param <- list(members = members, group = group, id = id)
    
  response <- rPython::python.call("indicators_api", param)
  
  if (is.character(response))
    response <- list(response)
  
  id <- sapply(response, "[[", "id")
  name <- sapply(response, "[[", "name")

  names <- c("id", "name")

  df <- data.frame(id, name, stringsAsFactors = FALSE)
  colnames(df) <- names
  
  return(df)
}


analytics <- function (dx = NULL, ou = NULL, prediction = FALSE) {
  ou <- paste(ou, collapse = ";")
  
  param <- list(dx = dx, ou = ou)
  if (prediction)
    param <- list(dx = ou, ou = dx)
  

  response <- rPython::python.call("analytics_api", param)
  
  period <- sapply(response, "[[", 2)
  values <- sapply(response, "[[", 3)
  
  
  df <- data.frame(period = period, dx = as.numeric(values), stringsAsFactors = FALSE)
  
  return(df)
}