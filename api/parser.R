library(httr)
library(jsonlite)

url <- "https://play.dhis2.org"

parseOrganisationUnits <- function(level = 2) {

  path <- "/2.25/api/organisationUnits"
  
  query <- list(paging = "false", fields = "name", fields = "code", fields = "id", level = level)
  
  end <- httr::modify_url(url, path = path, query = query)
  
  # send request to server get
  res <- httr::GET(end, httr::authenticate("admin", "district"))
  
  json <- httr::content(res)[[1]]
  
  if (is.character(json))
    json <- list(json)
  
  id <- sapply(json, "[[", "id")
  name <- sapply(json, "[[", "name")
  code <- sapply(json, "[[", 1)
  
  names <- c("code", "name", "id")
  
  df <- data.frame(code, name, id, stringsAsFactors = FALSE)
  colnames(df) <- names
  
  df <- subset(df, substr(df$code, 1, 3) == "OU_")
  
  return(df)
  
}


## This function returns indicators on DHIS2
## <type> indicators in groups
## <type> indicators under a given group
##
##
## : param <group> {bool}
## : param <id> {string}
parseIndicators <- function (id = NULL, group = TRUE) {
  
  ## function return a character vector
  ## Of ids under an Indicator Group  
  getIDs <- function (id) {
    path <- paste("/2.25/api/indicatorGroups/", id, sep = "")
    
    # query = list(paging = "false", fields = "name", fields = "id")
    
    # merge url with params
    end <- httr::modify_url(url, path = path)
    
    # send request to server
    res <- httr::GET(end, httr::authenticate("admin", "district"))
    
    json <- httr::content(res)$indicators
    
    id_vector <- sapply(json, "[[", "id")
    
    
    return(id_vector)
  }
  
  ## id = identifier for group
  ## id is passed when it is required
  ## to get the members of a group
  if (!is.null(id)) {
    
    path <- "/2.25/api/indicators/"
    query <- list(fields = "name", fields = "id")
    
    ## Empty dataframe for indicators
    indicatorsDF <- data.frame(id = character(), name = character(), stringsAsFactors = FALSE)
    chr_vector <- getIDs(id)
    
    for (indicatorId in chr_vector) {
      end <- httr::modify_url(url, path = paste(path, indicatorId), query = query)
      
      res <- httr::GET(end, httr::authenticate("admin", "district"))
      
      json <- httr::content(res)
      
      df <- data.frame(id = json$id, name = json$name, stringsAsFactors = FALSE)
      
      indicatorsDF <- rbind(indicatorsDF, df)
    }
    
    return (indicatorsDF)
  }
  
  ## check if :param {group} is TRUE
  if (group) {
    path <- "/2.25/api/indicatorGroups"
    query <- list(paging = "false", fields = "name", fields = "id")
    end <- httr::modify_url(url, path = path, query = query)
    
    res <- httr::GET(end, httr::authenticate("admin", "district"))
    
    json <- httr::content(res)[[1]]
    
    if (is.character(json))
      json <- list(json)
    
    id <- sapply(json, "[[", "id")
    name <- sapply(json, "[[", "name")
    
    names <- c("id", "name")
    
    df <- data.frame(id, name, stringsAsFactors = FALSE)
    
    colnames(df) <- names
    return(df)
    
  }
  
}


## this endpoint is for analytics
## DHIS2 api uses
## @dx for data elements and indicators
## @ou for organisation units
parseAnalytics <- function (dx = NULL, ou = NULL) {
  ou <- paste(ou, collapse = ";")
  dx <- paste(dx, collapse = ";")
  
  OU <- paste("ou:", ou, sep = "")
  DX <- paste("dx:", dx, sep = "")
  PE <- paste("pe:", "LAST_12_MONTHS", sep = "")
  
  path <- "/2.25/api/analytics"
  
  dimension <- paste(OU, PE, sep = ",")
  filter <- DX
  
  query <- list(skipMeta = "true", displayProperty = "NAME", dimension = dimension, filter = filter)
  
  end <- httr::modify_url(url, path = path, query = query)
  res <- httr::GET(end, httr::authenticate("admin", "district"))
  
  json <- httr::content(res)$rows
  
  period <- sapply(json, "[[", 2)
  values <- sapply(json, "[[", 3)
  
  df <- data.frame(dx = as.numeric(values), period = period, stringsAsFactors = FALSE)
  
  return(df)
  
}
