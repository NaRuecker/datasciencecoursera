library(shiny)
library(plotly)
shinyUI(fluidPage(
  titlePanel("PCA from DE data"),
  sidebarLayout(
    sidebarPanel(
      h3("Chose Drug and Concentration"),
###### Checkboxes with conditional slidebar
########
      checkboxInput("CiproC", "Cipro", value = TRUE),
      conditionalPanel(
        condition = "input.CiproC == true",
        sliderInput("Conc_Cipro", "Pick Cipro Concentration [log2MIC]",
                    -3, 3, value = c(-1,1))),
      checkboxInput("EthaC", "Etha", value = TRUE),
      conditionalPanel(
        condition = "input.EthaC == true",
        sliderInput("Conc_Etha", "Pick Etha Concentration [log2MIC]",
                    -3, 3, value = c(-1,1))),
      checkboxInput("EthiC", "Ethi", value = FALSE),
      conditionalPanel(
        condition = "input.EthiC == true",
        sliderInput("Conc_Ethi", "Pick Ethi Concentration [log2MIC]",
                    -3, 3, value = c(-1,1))),
      checkboxInput("KasAC", "KasA", value = FALSE),
      conditionalPanel(
        condition = "input.KasAC == true",
        sliderInput("Conc_KasA", "Pick KasA Concentration [log2MIC]",
                    -3, 3, value = c(-1,1))),
      checkboxInput("LevoC", "Levo", value = FALSE),
      conditionalPanel(
        condition = "input.LevoC == true",
        sliderInput("Conc_Levo", "Pick Levo Concentration [log2MIC]",
                    -3, 3, value = c(-1,1))),
      checkboxInput("LineC", "Line", value = FALSE),
      conditionalPanel(
        condition = "input.LineC == true",
        sliderInput("Conc_Line", "Pick Line Concentration [log2MIC]",
                    -3, 3, value = c(-1,1))),
      checkboxInput("PASC", "PAS", value = FALSE),
      conditionalPanel(
        condition = "input.PASC == true",
        sliderInput("Conc_PAS", "Pick PAS Concentration [log2MIC]",
                    -3, 3, value = c(-1,1))),
      checkboxInput("RifC", "Rif", value = FALSE),
      conditionalPanel(
        condition = "input.RifC == true",
        sliderInput("Conc_Rif", "Pick Rif Concentration [log2MIC]",
                    -3, 3, value = c(-1,1)))
      
######    
),
    mainPanel(
      
      h3("PCA of your choice"),
 
      plotlyOutput("PCA_Choice")

    )
  )
))
