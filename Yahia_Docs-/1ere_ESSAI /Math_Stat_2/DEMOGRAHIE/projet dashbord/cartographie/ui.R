## Shiny UI component for the Dashboard

dashboardPage(
  
  dashboardHeader(title="Exploration d'une carte géorgraphique de la Tunisie avec R & Shiny Dashboard", titleWidth = 650),
  
  
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
                menuItem("Dataset", tabName = "data", icon = icon("database")),
                menuItem("Visualization", tabName = "viz", icon=icon("chart-line")),
                
                # Conditional Panel for conditional widget appearance
                # Filter should appear only for the visualization menu and selected tabs within it
                conditionalPanel("input.sidebar == 'viz' && input.t2 == 'distro'", selectInput(inputId = "var1" , label ="Select the Variable" , choices = c1,selected = "POPTO2010")),
                conditionalPanel("input.sidebar == 'viz' && input.t2 == 'trends' ", selectInput(inputId = "var2" , label ="Select the  Variable" , choices = c2, selected = "POPTO2010")),
                conditionalPanel("input.sidebar == 'viz' && input.t2 == 'relation' ", selectInput(inputId = "var3" , label ="Select the X variable" , choices = c1, selected = "POPTO2010")),
                conditionalPanel("input.sidebar == 'viz' && input.t2 == 'relation' ", selectInput(inputId = "var4" , label ="Select the Y variable" , choices = c1, selected = "gou_nom")),
                menuItem("Choropleth Map", tabName = "map", icon=icon("map"))
                
    )
  ),
  
  
  dashboardBody(
    
    tabItems(
      ## First tab item
      tabItem(tabName = "data", 
              tabBox(id="t1", width = 12, 
                     tabPanel("About", icon=icon("address-card"),
                              fluidRow(
                                column(width = 8, tags$img(src="carte-tunisie.jpg", width =200 , height = 400),
                                       tags$br() , 
                                       tags$a("Carte géographique  de la Tunisie"), align = "center"),
                                column(width = 4, tags$br() ,
                                       tags$p("Cet ensemble de données est accompagné de la base R et contient des statistiques sur la population 2010,  la population 2004, l'ndicateur de developpement regional 2011, et des autres indices et des taux pour la Tunisie.")
                                )
                              )
                              
                              
                     ), 
                     tabPanel("Data", dataTableOutput("dataT"), icon = icon("table")), 
                     tabPanel("Structure", verbatimTextOutput("structure"), icon=icon("uncharted")),
                     tabPanel("Summary Stats", verbatimTextOutput("summary"), icon=icon("chart-pie"))
              )
              
      ),  
      
      # Second Tab Item
      tabItem(tabName = "viz", 
              tabBox(id="t2",  width=12, 
                     tabPanel(" top5 by regions ", value="trends",
                              fluidRow(tags$div(align="center", box(tableOutput("top5"), title = textOutput("head1") , collapsible = TRUE, status = "primary",  collapsed = TRUE, solidHeader = TRUE)),
                                       tags$div(align="center", box(tableOutput("low5"), title = textOutput("head2") , collapsible = TRUE, status = "primary",  collapsed = TRUE, solidHeader = TRUE))
                                       
                              ),
                              withSpinner(plotlyOutput("bar"))
                     ),
                     tabPanel("Distribution", value="distro",
                              # selectInput("var", "Select the variable", choices=c("Rape", "Assault")),
                              withSpinner(plotlyOutput("histplot", height = "350px"))),
                     side = "left"
              ),
              
      ),
      
      
      # Third Tab Item
      tabItem(
        tabName = "map",
        box(withSpinner(plotOutput("map_plot")), width = 12)
        
        
        
      )
      
    )
  )
)