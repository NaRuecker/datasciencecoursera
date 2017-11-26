#Coding help from https://stackoverflow.com/questions/30443625/how-do-i-build-a-reactive-dataframe-in-r-shiny

library(shiny)
library(dplyr)
library(data.table)
library(plotly)
library(rgl)
library(pca3d)
library (plyr)
library(gplots)
library(RColorBrewer)
library(reshape2)
library(ggplot2)
library(htmlwidgets)

compdata <- read.csv("DataForPCA.csv")
Cipro <- subset(compdata, compdata$Drug == "Cipro")
Etha <- subset(compdata, compdata$Drug == "Etha")
Ethi <- subset(compdata, compdata$Drug == "Ethi")
Levo <- subset(compdata, compdata$Drug == "Levo")
Line <- subset(compdata, compdata$Drug == "Line")
PAS <- subset(compdata, compdata$Drug == "PAS")
Rif <- subset(compdata, compdata$Drug == "Rif")
KasA <- subset(compdata, compdata$Drug == "KasA")


EmptyDF = compdata[FALSE,]


shinyServer(function(input, output) {

  makeReactiveBinding("Newdata")
  
  Chosendata <- reactive({

    Ciprodata <- (if(input$CiproC) {Cipro[Cipro$log2MIC %between% c(input$Conc_Cipro[1], input$Conc_Cipro[2]),]} else {EmptyDF})
    Ethadata <- (if(input$EthaC) {Etha[Etha$log2MIC %between% c(input$Conc_Etha[1], input$Conc_Etha[2]),]} else {EmptyDF})
    Ethidata <- (if(input$EthiC) {Ethi[Ethi$log2MIC %between% c(input$Conc_Ethi[1], input$Conc_Ethi[2]),]} else {EmptyDF})
    Rifdata <- (if(input$RifC) {Rif[Rif$log2MIC %between% c(input$Conc_Rif[1], input$Conc_Rif[2]),]} else {EmptyDF})
    Levodata <- (if(input$LevoC) {Levo[Levo$log2MIC %between% c(input$Conc_Levo[1], input$Conc_Levo[2]),]} else {EmptyDF})
    Linedata <- (if(input$LineC) {Line[Line$log2MIC %between% c(input$Conc_Line[1], input$Conc_Line[2]),]} else {EmptyDF})
    PASdata <- (if(input$PASC) {PAS[PAS$log2MIC %between% c(input$Conc_PAS[1], input$Conc_PAS[2]),]} else {EmptyDF})
    KasAdata <- (if(input$KasAC) {KasA[KasA$log2MIC %between% c(input$Conc_KasA[1], input$Conc_KasA[2]),]} else {EmptyDF})
    
    
    Newdata <- rbind.data.frame(Ciprodata, Ethadata, Ethidata, Rifdata, Levodata, Linedata, PASdata, KasAdata)
    
  })
  
  Drug <- reactive({
    
    Ciprodata <- (if(input$CiproC) {Cipro[Cipro$log2MIC %between% c(input$Conc_Cipro[1], input$Conc_Cipro[2]),]} else {EmptyDF})
    Ethadata <- (if(input$EthaC) {Etha[Etha$log2MIC %between% c(input$Conc_Etha[1], input$Conc_Etha[2]),]} else {EmptyDF})
    Ethidata <- (if(input$EthiC) {Ethi[Ethi$log2MIC %between% c(input$Conc_Ethi[1], input$Conc_Ethi[2]),]} else {EmptyDF})
    Rifdata <- (if(input$RifC) {Rif[Rif$log2MIC %between% c(input$Conc_Rif[1], input$Conc_Rif[2]),]} else {EmptyDF})
    Levodata <- (if(input$LevoC) {Levo[Levo$log2MIC %between% c(input$Conc_Levo[1], input$Conc_Levo[2]),]} else {EmptyDF})
    Linedata <- (if(input$LineC) {Line[Line$log2MIC %between% c(input$Conc_Line[1], input$Conc_Line[2]),]} else {EmptyDF})
    PASdata <- (if(input$PASC) {PAS[PAS$log2MIC %between% c(input$Conc_PAS[1], input$Conc_PAS[2]),]} else {EmptyDF})
    KasAdata <- (if(input$KasAC) {KasA[KasA$log2MIC %between% c(input$Conc_KasA[1], input$Conc_KasA[2]),]} else {EmptyDF})
    
    
    Newdata <- rbind.data.frame(Ciprodata, Ethadata, Ethidata, Rifdata, Levodata, Linedata, PASdata, KasAdata)
    Drug <- as.vector(Newdata$Drug)
    
  })
  
  logTwoMIC <- reactive({
    
    Ciprodata <- (if(input$CiproC) {Cipro[Cipro$log2MIC %between% c(input$Conc_Cipro[1], input$Conc_Cipro[2]),]} else {EmptyDF})
    Ethadata <- (if(input$EthaC) {Etha[Etha$log2MIC %between% c(input$Conc_Etha[1], input$Conc_Etha[2]),]} else {EmptyDF})
    Ethidata <- (if(input$EthiC) {Ethi[Ethi$log2MIC %between% c(input$Conc_Ethi[1], input$Conc_Ethi[2]),]} else {EmptyDF})
    Rifdata <- (if(input$RifC) {Rif[Rif$log2MIC %between% c(input$Conc_Rif[1], input$Conc_Rif[2]),]} else {EmptyDF})
    Levodata <- (if(input$LevoC) {Levo[Levo$log2MIC %between% c(input$Conc_Levo[1], input$Conc_Levo[2]),]} else {EmptyDF})
    Linedata <- (if(input$LineC) {Line[Line$log2MIC %between% c(input$Conc_Line[1], input$Conc_Line[2]),]} else {EmptyDF})
    PASdata <- (if(input$PASC) {PAS[PAS$log2MIC %between% c(input$Conc_PAS[1], input$Conc_PAS[2]),]} else {EmptyDF})
    KasAdata <- (if(input$KasAC) {KasA[KasA$log2MIC %between% c(input$Conc_KasA[1], input$Conc_KasA[2]),]} else {EmptyDF})
    
    
    Newdata <- rbind.data.frame(Ciprodata, Ethadata, Ethidata, Rifdata, Levodata, Linedata, PASdata, KasAdata)
    logTwoMIC <- as.vector(Newdata$log2MIC)
    
  })
  
  
  output$PCA_Choice <- renderPlotly({
    ConcValue <- logTwoMIC()
    DrugName <- Drug()
    dataForPCA <- Chosendata()
    dataForPCA$Drug <- NULL
    dataForPCA$log2MIC <- NULL
    #Remove NA columns
    dataForPCA <- dataForPCA[,colSums(is.na(dataForPCA))<nrow(dataForPCA)]
    dataForPCA <- sapply(dataForPCA, as.numeric)
    
    pcaDATA <- prcomp(dataForPCA, scale. = T)
    Coordidata <- as.data.frame(pcaDATA$x[, 1:3])
    plot_ly(Coordidata,type="scatter3d", x =~PC1, y=~PC2, z=~PC3,
            marker = list(symbol = 'sphere', sizemode = 'diameter'),
            mode = 'text',
            text = ~paste(DrugName,ConcValue) ) %>%
      add_markers() %>%
      layout(scene = list(xaxis = list(title = 'PC1',
                                       range = c(-25, 25),
                                       zerolinewidth = 1,
                                       ticklen = 5,
                                       gridwidth = 2),
                          yaxis = list(title = 'PC2',
                                       range = c(-8, 8),
                                       zerolinewidth = 1,
                                       ticklen = 5,
                                       gridwith = 2),
                          zaxis = list(title = 'PC3',
                                       range = c(-8, 8),
                                       zerolinewidth = 1,
                                       ticklen = 5,
                                       gridwith = 2))
      )
  })
  
 
  })
  
 
  
    
