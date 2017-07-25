library(tidyr)
library(dplyr)
library(plyr)
library(httr)
library(leaflet)
library(ggplot2)
library(rlist)

ua <- user_agent("https://github.com/INyabuto")

dhis2_rapi <- function(path){
  # Get HTTP request
  url <- modify_url("https://play.dhis2.org", path = path)
  # pass the response
  resp <- GET(url, ua, authenticate("admin","district"))
  # Confirm the passed response, It should be a json format
  if (http_type(resp)!="application/json"){
    stop("API did not return json", call. = FALSE)
  }
  # Pass the out put into an r object
  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  # Turn apis errors into r errorsa
  if (http_error(resp)){
    stop(
      sprintf(
        "DHIS2 API request failed [%s]\n%s\n<%s>", 
        status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }
  # Return an helpful object i.e not only a list by creating an s3 object
  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "dhis2_rapi"
  )
}



# anc1 coverage resp
anc1_data_frame <- function(){
  anc1_resp <- dhis2_rapi("/release1/api/25/analytics.json?dimension=ou:O6uvpzGd5pu;PMa2VCrupOd;jUb8gELQApl;kJq2mPyFEHo;lc3eMKXaEfw&dimension=pe:LAST_12_MONTHS&filter=dx:dwEq7wi6nXV&displayProperty=NAME&skipMeta=true")
  anc1_parsed <- anc1_resp$content$rows
  orgID<- sapply(anc1_parsed,"[[",1)
  period <- sapply(anc1_parsed,"[[",2)
  anc1_coverage<- sapply(anc1_parsed,"[[",3)
  anc1_data_frame <-data.frame(orgID,period,anc1_coverage,stringsAsFactors =F)
  return(anc1_data_frame)
}


# Extract orgUnits
org_data_frame <- function(){
  org_resp <- dhis2_rapi("/release1/api/organisationUnits?paging=FALSE")
  org_parsed <- org_resp$content$organisationUnits
  orgID<- sapply(org_parsed,"[[",1)
  orgName <- sapply(org_parsed,"[[",2)
  org_data_frame <- data.frame(orgID,orgName,stringsAsFactors = F)
  return(org_data_frame)
}

# Get org ID and name that matches the selected orgunits
filtered_org_data_frame <- org_data_frame() %>% filter(orgID %in% c("O6uvpzGd5pu", "PMa2VCrupOd", "jUb8gELQApl", "kJq2mPyFEHo", "lc3eMKXaEfw"))

# documented anc1_data-fame with org_Name
documeneted_anc1_data_frame <- function(){
  left_join(filtered_org_data_frame,anc1_data_frame(),by="orgID")
}

barChart <- function(){
  bar <- ggplot(documeneted_anc1_data_frame(),aes(x=factor(period),y=as.numeric(anc1_coverage),fill=orgName))
  bar + geom_bar(stat = "identity",position = "dodge")+
    xlab("Period") +
    ylab("ANC_IPT1") +
    labs(fill="Organisaiton units") +
    ggtitle("ANC_IPT1 coverage for the last 12 months")
}

pregnancy_comp_df <- function(){
  resp <- dhis2_rapi("/release1/api/25/analytics.json?dimension=ou:O6uvpzGd5pu;PMa2VCrupOd;jUb8gELQApl;kJq2mPyFEHo;lc3eMKXaEfw&dimension=pe:LAST_12_MONTHS&filter=dx:h8vtacmZL5j&displayProperty=NAME&skipMeta=true")
  parsed_resp <- resp$content$rows
  #Extract elements from the list
  orgID <- sapply(parsed_resp,"[[",1)
  period <- sapply(parsed_resp,"[[",2)
  complications <- sapply(parsed_resp,"[[",3)
  pregnancy_comp_df <- data.frame(orgID,period,complications,stringsAsFactors = F)
  return(pregnancy_comp_df)
}

# Join pregnancy_comp_df with documented_anc1_data_frame for comparion
pregnancy_comp_anc1_df <- function(){
  left_join(documeneted_anc1_data_frame(),pregnancy_comp_df(),by=c("orgID","period"))
}

barChart_preg <- function(){
  p <- ggplot(pregnancy_comp_anc1_df(),aes(x=as.factor(period),y=as.numeric(complications),fill=orgName))
  p + geom_bar(stat = "identity",position = "dodge") + 
    xlab("period")+
    ylab("pregenancy-related complications")+
    labs(fill="Ogranisaiton units")+
    ggtitle("Pregnancy related complications for the last 12 months")
}

map <- function(){
  resp <- dhis2_rapi("/release1/api/organisationUnits.geojson?level=2") 
  shape <- resp$content$features
  filtered_shape <- list.filter(shape,id %in% c("O6uvpzGd5pu", "lc3eMKXaEfw", "jUb8gELQApl", "PMa2VCrupOd", "kJq2mPyFEHo"))
  leaflet(options = leafletOptions(minZoom= 4, maxZoom = 18)) %>%
    addTiles() %>%
    addGeoJSON(filtered_shape)
}

