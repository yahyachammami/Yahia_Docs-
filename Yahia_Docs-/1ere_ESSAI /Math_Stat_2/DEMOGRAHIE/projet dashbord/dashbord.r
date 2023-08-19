library(shiny)
library(shinydashboard)
library(DT)  
library(dplyr)
library(plotly) 
library(ggplot2)
library(ggtext)
library(ggcorrplot)
library(shinycssloaders)
library(maptools)
library(tidyverse)
library(rgeos)
library(reshape2)
library(sp)
library(shapefiles)
library(classInt)
library(RColorBrewer)


fdc <- readShapeSpatial("D:/Projet Demographie/cartographie/Tunisie_snuts4.shp")

donnees<-read.csv( "D:/Projet Demographie/cartographie/tunisie_data.csv",header=TRUE,sep=";",dec=",",encoding="latin1")


c1 = donnees %>% 
  select(-del_nom) %>% 
  names()

c2 = donnees %>% 
  select(-del_nom, -del) %>% 
  names()


my_data1 = donnees%>% 
  mutate(del_nom = tolower(del_nom))

header <- dashboardHeader(title="Dashboard des indicateurs démographiques")
 
sidebar<- dashboardSidebar(
  sidebarMenu(
    menuItem("Dataset",tabName="data",icon= icon("database")),
    menuItem("Visualization", tabName = "viz", icon=icon("chart-line")),
    
    menuItem("Indicateur",icon=icon("location-dot"),
             tabName="indicateur",
             menuSubItem(tabName="indi",
                         selectInput(inputId="indi",
                                     label="Sélectionner un indicateur:",
                                     choices=c("Pauvreté", "Developpement", "Densité"))),
             menuSubItem(tabName="rg",
                         selectInput(inputId="reg",
                                     label="Voir les régions: ",
                                     choices=donnees$reg)
             )
             ),
    
    
    conditionalPanel("input.sidebar == 'viz' && input.t2 == 'distro'", selectInput(inputId = "var1" , label ="Select the Variable" , choices = c1,selected = "POPTO2010")),
    conditionalPanel("input.sidebar == 'viz' && input.t2 == 'trends' ", selectInput(inputId = "var2" , label ="Select the Arrest type" , choices = c2, selected = "POPTO2010")),
    conditionalPanel("input.sidebar == 'viz' && input.t2 == 'relation' ", selectInput(inputId = "var3" , label ="Select the X variable" , choices = c1, selected = "POPTO2010")),
    conditionalPanel("input.sidebar == 'viz' && input.t2 == 'relation' ", selectInput(inputId = "var4" , label ="Select the Y variable" , choices = c1, selected = "gou_nom"))
             
    
  )
)
body <- dashboardBody(
  tabItems(
    tabItem(tabName="data",
          tabBox(id="t1",width=18,
                 tabPanel("Data", dataTableOutput("dataT"), icon = icon("table")), 
                 tabPanel("Structure", verbatimTextOutput("structure"), icon=icon("uncharted")),
                 tabPanel("Summary Stats", verbatimTextOutput("summary"), icon=icon("chart-pie"))
                 )
          ),
    tabItem(tabName = "viz", 
            tabBox(id="t2",  width=12, 
                   tabPanel(" top5 by regions ", value="trends",
                            fluidRow(tags$div(align="center", box(tableOutput("top5"), title = textOutput("head1") , collapsible = TRUE, status = "primary",  collapsed = TRUE, solidHeader = TRUE)),
                                     tags$div(align="center", box(tableOutput("low5"), title = textOutput("head2") , collapsible = TRUE, status = "primary",  collapsed = TRUE, solidHeader = TRUE))
                                     
                            ),
                            withSpinner(plotlyOutput("bar"))
                   ),
                   tabPanel("Distribution", value="distro",
                            withSpinner(plotlyOutput("histplot", height = "350px"))),
                   side = "left"))),
    tabItem(tabName="rg",
            plotOutput("plot1")
            )
    
)
  



server <- function (input, output) { 
  output$dataT <- renderDataTable(donnees)
  
  
  # Rendering the box header  
  output$head1 <- renderText(
    paste("5 regions with high rate of", input$var2)
  )
  
  # Rendering the box header 
  output$head2 <- renderText(
    paste("5 regions with low rate of", input$var2)
  )
  
  
  # Rendering table with 5 states with high arrests for specific crime type
  output$top5 <- renderTable({
    
    donnees %>% 
      select(del_nom, input$var2) %>% 
      arrange(desc(get(input$var2))) %>% 
      head(5)
    
  })
  
  output$low5 <- renderTable({
    
    donnees %>% 
      select(del_nom, input$var2) %>% 
      arrange(get(input$var2)) %>% 
      head(5)
  })
  
  output$structure <- renderPrint({
    donnees %>% 
      str()
  })
  
  
  # For Summary Output
  output$summary <- renderPrint({
    donnees %>% 
      summary()
  })
  
  
  
    
  output$plot1 <-renderPlot({
    if(input$indi == "Pauvreté"){
      dfe <- donnees[donnees$reg == input$reg,]
      
      polygr <- fdc[fdc@data$id_snuts2 == input$reg,]
      
      pt <- cbind(polygr@data[, "id_snuts2"], as.data.frame(coordinates(polygr)))
      
      
      var <- as.vector(na.omit(dfe$TDP))
      nbclass <- 8
      
      distr <- classIntervals(var, nbclass)$brks
      
      colours <- brewer.pal(nbclass, "RdPu")
      colMap <- colours[(findInterval(dfe$TDP, distr, all.inside = TRUE))]
      
      plot(polygr, col = colMap, border = "black", lwd =1)
      
      legend(x = "topright", legend = leglabs(round(distr, 0.5)), fill = colours, bty = "n", pt.cex = 1, cex = 1)
      
      title(main = "Taux de pauvreté ", col = colMap, cex.sub = 0.8)
      
    }
    else if(input$indi == "Developpement"){
      dfe <- donnees[donnees$reg == input$reg,]
      
      polygr <- fdc[fdc@data$id_snuts2 == input$reg,]
      
      pt <- cbind(polygr@data[, "id_snuts2"], as.data.frame(coordinates(polygr)))
      re <- as.vector(na.omit(dfe$IDRVA2011))
      nbclass <- 8
      
      distr <- classIntervals(re, nbclass)$brks
      
      colours <- brewer.pal(nbclass, "PuBuGn")
      colMap <- colours[(findInterval(dfe$IDRVA2011, distr, all.inside = TRUE))]
      
      plot(polygr, col = colMap, border = "black", lwd =1)
      
      legend(x = "topright", legend = leglabs(round(distr, 0.5)), fill = colours, bty = "n", pt.cex = 1, cex = 1)
      
      title(main = "Developpement Regional ", col = colMap, cex.sub = 0.8) 
    }
    else{
      polygr <- fdc[fdc@data$id_snuts2 == input$reg,]
      
      pt <- cbind(polygr@data[, "id_snuts2"], as.data.frame(coordinates(polygr)))
      rfe <- don1[don1$reg == input$reg,]
      
      
      but <- as.vector(na.omit(rfe$rate))
      nbclass <- 8
      
      distr <- classIntervals(but, nbclass)$brks
      
      colours <- brewer.pal(nbclass, "Blues")
      colMap <- colours[(findInterval(rfe$rate, distr, all.inside = TRUE))]
      
      plot(polygr, col = colMap, border = "Red", lwd =1)
      
      legend(x = "topright", legend = leglabs(round(distr, 0.5)), fill = colours, bty = "n", pt.cex = 1, cex = 1)
      
      title(main = "Densité de la population ", col = colMap, cex.sub = 0.8)
      
    }
    
    
    
  })
    
    
 
  }
ui <- dashboardPage(skin = "blue" , header, sidebar, body)

# Run the application 
shinyApp(ui = ui, server = server)
