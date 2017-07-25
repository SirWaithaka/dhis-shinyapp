library(shiny)
library(httr)
library(plyr)
library(dplyr)


shinyServer(function(input,output){
  orgUnits <- reactive({
    # Get organisation units from dhis2 api
    resp <- dhis2_rapi("/release1/api/organisationUnits.json?paging=false&fields=[name,code,level]")
    org <- resp$content$organisationUnits
    
    # Extract code, level and name of the organisation units from org
    Code <- sapply(org,'[[',1)
    Name<- sapply(org,'[[',"name")
    Level <- sapply(org,'[[',"level")
    
    
    # marge the vectors to form a dataframe called organisationUnits
    organisatonUnits <- data.frame(Code, Name, Level)
    
  })
  
  elements <- reactive({
    # Get the data from dhis2
    resp <- dhis2_rapi("/release1/api/dataElements.json?paging=false&fields=[name,code]")
    elements <- resp$content$dataElements
    
    # Extract Elements Code and Name
    Code <- sapply(elements, '[[', 1)
    Name <- sapply(elements, '[[', "name")
    
    # marge the vectors to forma a dataframe of dataElements
    dataElements <- data.frame(Name,Code)
  })
  
  
  # Present orgUnits data table on the ui table
  output$orgUnitTable <- renderDataTable({
    orgUnits()
    
  })
  
  
  
  # Present detaElements data tabel on the ui table
  output$dataElementsTable <- renderDataTable({
    elements()
  })
  chart <- reactive({
    switch(input$dataelement,
           "ANC1 visit"=barChart(),
           "Pregnancy-related complications"=barChart_preg())
  })
  
  # choose data set
  dataset <- reactive({
    switch(input$dataelement,
           "ANC1 visit"=documeneted_anc1_data_frame() %>% 
             select(-orgID) %>%
             spread(orgName,anc1_coverage) %>% mutate(month = 1:length(period)),
           "Pregnancy-related complications"=pregnancy_comp_anc1_df() %>%
             select(-orgID,-anc1_coverage) %>% 
             spread(orgName,complications) %>% mutate(month = 1:length(period))
    )
  })
  
  # select the data for comparison
  compare_element <- reactive({
    switch(input$dataelement2,
           "ANC1 visit"=documeneted_anc1_data_frame() %>% 
             select(-orgID) %>%
             spread(orgName,anc1_coverage),
           "Pregnancy-related complications"=pregnancy_comp_anc1_df() %>%
             select(-orgID,-anc1_coverage) %>% 
             spread(orgName,complications)
    )
  })
  
  # Get the file extensions from the radiobutton
  fileext <- reactive({
    switch(input$type_download_table,
           "Excel (CSV)" = "csv",
           "Text (TSV)" = "txt",
           "Text (Space Separated)" = "txt",
           "Doc" = "doc")
  })
  
  # choose org units,
  org <- reactive({
    selected_org <- c(input$orgUnit_select)
  })
  
  # Draw a chart based on the selected org units
  selected_chart <- function(){
    selected_dataset <- dataset()
    compared_dataset <- compare_element()
    x <- as.numeric(dataset()[,input$orgUnit_select]) # Get the data to draw a histogram
    bins <- seq(min(x),max(x),length.out = input$bins+1) # set breaks according to the data
    switch(input$chart_type,
           "bar graph"=ggplot(dataset(),aes(x=period, y=as.numeric(get(org())),fill=period))+geom_bar(stat = "identity")+ylab(input$dataelement)+ggtitle(paste("A bar graph of",input$dataelement,"for",input$orgUnit_select))+theme_light(),
           "histogram"=ggplot(dataset(),aes(x=as.numeric(get(org())),fill=..count..))+geom_histogram(breaks=bins)+xlab(input$orgUnit_select)+ggtitle(paste("Distribution of",input$dataelement,"for the last 12 months in",input$orgUnit_select))+theme_light(),
           "line graph"=ggplot(dataset(),aes(x=month,y=as.numeric(get(org()))))+geom_line(color="blue",size=2)+ylab(input$dataelement)+ggtitle(paste("A line graph",input$dataelement,"for the last 12 months for",input$orgUnit_select))+theme_light(),
           "scatter plot"= ggplot(selected_dataset,aes(x=as.numeric(get(org())),y=as.numeric(compared_dataset[,input$orgUnit_select]),color=period))+geom_point(size=4)+xlab(input$dataelement)+ylab(input$dataelement2)+ggtitle(paste("A comparison of",input$dataelement,"and",input$dataelement2,"in",input$orgUnit_select))+theme_light(),
           "density histogram"=ggplot(dataset(), aes(x=as.numeric(get(org()))))+geom_histogram(aes(y=..density..),bin=10)+stat_function(fun = dnorm, colour="red",args = list(mean=mean(as.numeric(get(org())),na.rm = T),sd=sd(as.numeric(get(org())),na.rm = T)))+theme_light()
    )
  }
  
  
  # output anc1 barchar 
  output$anc1 <- renderPlot({
    #barChart() 
    #chart()
    selected_chart()
  })
  
  #output chart table
  output$chart_table <- renderDataTable({
    dataset() %>% select(-month)
  })
  
  #'
  #' the downlaod option
  output$download <- downloadHandler(
    # specify the filename
    filename = function(){
      #ancPreg.png,
      #ancPreg.pdf
      paste("anc",input$type_download,sep = ".")
    },
    content = function(file){
      #open the device
      # plot the device
      # close the device
      # the device may either be png() or pdf()
      if (input$type_download=="png")
        png(file)
      else
        pdf(file)
      # create the plot
      
      plot(selected_chart())
      #barChart_preg()
      
      # close the device
      dev.off()
    }
  )
  
  
  # download chart table code
  output$download2 <- downloadHandler(
    filename = function(){
      paste(input$dataelement,fileext(),sep = ".") # e.g anc1.csv
    },
    content =function(file){
      # define how the content will be written
      sep <- switch(input$type_download_table,
                    "Excel (CSV)" = ",",
                    "Text (TSV)" = "\t",
                    "Text (Space Separated)" = " ",
                    "Doc" = " ")
      # Specify how to write the file
      write.table(dataset(),file,sep=sep,row.names = F)
    }
  )
  
  # summarised anc
  summary_anc <- reactive({
    ANC1_Visit <-documeneted_anc1_data_frame() %>% 
      select(-orgID) %>%
      spread(orgName,anc1_coverage) %>% mutate(month = 1:length(period)) 
    ANC1_Visit %>% summarise(Bo=sum(as.numeric(Bo),na.rm=T),
                             Bonthe=sum(as.numeric(Bonthe),na.rm=T),
                             Kailahun=sum(as.numeric(Kailahun),na.rm=T),
                             Kambia=sum(as.numeric(Kambia),na.rm=T),
                             Kenema=sum(as.numeric(Kenema),na.rm=T)) %>%gather(Organization_Unit,ANC,1:5)
  })
  
  # summarised pregnancy -related complcations
  summary_preg <- reactive({
    Pregnancy_Complications <- pregnancy_comp_anc1_df() %>%
      select(-orgID,-anc1_coverage) %>% 
      spread(orgName,complications) %>% mutate(month = 1:length(period))
    Pregnancy_Complications %>% summarise(Bo=sum(as.numeric(Bo),na.rm=T),
                                          Bonthe=sum(as.numeric(Bonthe),na.rm=T),
                                          Kailahun=sum(as.numeric(Kailahun),na.rm=T),
                                          Kambia=sum(as.numeric(Kambia),na.rm=T),
                                          Kenema=sum(as.numeric(Kenema),na.rm=T)) %>%gather(Organization_Unit,Complications,1:5)
  })
  # main dashboard - summary of anc1 covaerage
  output$org_rank_anc1 <- renderTable({
    summary_anc()
  })
  
  # main dashboard - summary of pregnancy related complications
  output$org_rank_preg <- renderTable({
    summary_preg()
  })
  
  # main dashboard - summary of best perming organization
  output$org_rank_best <- renderTable({
    combined_summary <- left_join(summary_anc(),summary_preg(),by="Organization_Unit")
    combined_summary %>% mutate(difference = ANC-Complications) %>% mutate(variance=(difference-mean(difference))^2) %>% arrange(variance) %>% mutate(Rank=1:length(variance)) %>%
      select(Rank,Organization_Unit)
  })
  
  # main dashboard - summary of ANC 1 coverage of organization units- Pie chart
  output$org_pie_anc1 <- renderPlot({
    data <- summary_anc() %>% mutate(fraction=ANC/sum(ANC)) %>% 
      arrange(fraction) %>% mutate(ymax=cumsum(fraction)) %>%
      mutate(ymin=c(0,head(ymax,n=-1)))
    ggplot(data,aes(fill=Organization_Unit,ymax=ymax,ymin=ymin,xmax=4,xmin=3))+
      geom_rect()+
      coord_polar(theta = "y")+
      xlim(c(0,4))+
      theme(panel.grid = element_blank())+
      theme(axis.text = element_blank())+
      theme(axis.ticks = element_blank())+
      annotate("text",x=0,y=0,label="ANC1 coverage")+labs(title="")+
      theme_light()
    
  })
  
  # Main dashboard - organisation unit performance
  output$org_performance_line <- renderPlot({
    # Get different data 
    ANC1_visit <- documeneted_anc1_data_frame() %>% 
      select(-orgID) %>%
      spread(orgName,anc1_coverage) %>% mutate(month = 1:length(period))
    Pregnancy_Comp <- pregnancy_comp_anc1_df() %>%
      select(-orgID,-anc1_coverage) %>% 
      spread(orgName,complications) %>% mutate(month = 1:length(period))
    # merge the data
    ANC1_Pregnancy <- left_join(ANC1_visit,Pregnancy_Comp,by="month")
    # Compute difference of ANC1 and pregancy comp
    ANC1_Pregnancy_diff <- ANC1_Pregnancy %>% mutate(Bo.diff=as.numeric(Bo.x)-as.numeric(Bo.y)) %>%
      mutate(Bonthe.diff=as.numeric(Bonthe.x)-as.numeric(Bonthe.y)) %>%
      mutate(Kailahun.diff=as.numeric(Kailahun.x)-as.numeric(Kailahun.y)) %>%
      mutate(Kambia.diff=as.numeric(Kambia.x)-as.numeric(Kambia.y)) %>%
      mutate(Kenema.diff=as.numeric(Kenema.x)-as.numeric(Kenema.y))
    # Get the variance
    ANC1_Variance <- ANC1_Pregnancy_diff %>% mutate(Bo.var=(Bo.diff-mean(Bo.diff,na.rm=T))^2) %>%
      mutate(Bonthe.var=(Bonthe.diff-mean(Bonthe.diff,na.rm=T))^2) %>%
      mutate(Kailahun.var=(Kailahun.diff-mean(Kailahun.diff,na.rm=T))^2) %>%
      mutate(Kambia.var=(Kambia.diff-mean(Kambia.diff,na.rm=T))^2) %>%
      mutate(Kenema.var=(Kenema.diff-mean(Kenema.diff,na.rm=T))^2)
    # Select org unit variance and rename to org-units and month to reconstract the dataframe and make it long
    performance_long <- ANC1_Variance %>% select(month,Bo.var,Bonthe.var,Kailahun.var,Kambia.var,Kenema.var) %>%
      mutate(Bo=Bo.var) %>% mutate(Bonthe=Bonthe.var) %>% mutate(Kailahun=Kailahun.var) %>% mutate(Kambia=Kambia.var)%>% mutate(Kenema=Kenema.var) %>%
      select(month,Bo,Bonthe,Kailahun,Kambia,Kenema) %>%
      gather(Organization_Unit,Variance,2:6) 
    # Plot the line graph
    ggplot(performance_long,aes(x=month,y=Variance,color=Organization_Unit))+geom_line(size=2)+theme_light()
    
    
  })
  
  # main dashboard - 
  output$org_pie_preg <- renderPlot({
    ggplot(summary_preg(),aes(x=Organization_Unit,y=Complications,fill=Organization_Unit))+geom_bar(stat = "identity")+theme_light()
  })
  
  
  output$map <- renderLeaflet({
    map()
  })
})




