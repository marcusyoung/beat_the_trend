library(shiny)



ui <- fluidPage(
  
    titlePanel(title=div(img(src="bus.gif", height = 70), "Beat the Trend - The Domino Bus Group"), windowTitle = "Beat the Trend - The Domino Bus Group"),  
    
    fluidRow(
      
      column(3,
             
    wellPanel(
    
    h4("Service 1"),
    
    sliderInput(
      inputId = "s1_fare_increase",
      label = "Set fare change (%)",
      min = -100,
      max = 100,
      value = 0,
      step = 1
    ),
    sliderInput(
      inputId = "s1_mileage_increase",
      label = "Set mileage change (%)",
      min = -100,
      max = 100,
      value = 0,
      step = 1
    )
    ),
    wellPanel(
      h4("Service 2"),
      
      sliderInput(
        inputId = "s2_fare_increase",
        label = "Set fare change (%)",
        min = -100,
        max = 100,
        value = 0,
        step = 1
      ),
      sliderInput(
        inputId = "s2_mileage_increase",
        label = "Set mileage change (%)",
        min = -100,
        max = 100,
        value = 0,
        step = 1
    )
    ),
    
    wellPanel(
    conditionalPanel(
      condition = "output.bankrupt == 'no' & output.year != '25'",
    actionButton(inputId = "simulate",
                 label = "Simulate next year", class="btn btn-success action-button")),
    
      conditionalPanel(
        condition = "output.bankrupt == 'yes' || output.year == '25'",
        actionButton(inputId = "simulate",
                     label = "Simulate next year", class="btn btn-success action-button disabled")
    
  
    ),
    
    tags$br(),
    tags$button("Restart", id="restart", type="button", class="btn btn-danger action-button", onclick="history.go(0)")
    )
    ),
    
    # beginning of right side
    
    
    column(9,
           
    
           fluidRow(
             column(2, 
                    h4("Year")
             ),
             column(2,
                    h4("Gross profit")
             ),
             column(2,
                    h4("Tax paid")
             ),
             column(2, 
                    h4("Net profit")
             ),
             column(2, 
                    h4("Interest due")
             ),
             column(2, 
                    h4("Balance")
             )
           ),
           
    
    fluidRow(
      column(2, 
             wellPanel (
               div(textOutput("year"),style = "font-size:125%")
             )
             ),
      column(2,
             
             wellPanel (
               div(textOutput("grossprofit"),style = "font-size:125%")
               )
             
      ),
      column(2,
             
             wellPanel (
               div(textOutput("taxpaid"),style = "font-size:125%")
               )
             
      ), 
      column(2,
             
             wellPanel (
               div(textOutput("netprofit"),style = "font-size:125%")
               )
             
      ),     
            
      column(2, 
             wellPanel(
               div(textOutput("interestdue"),style = "font-size:125%")
               )
             ),
      column(2, 
             wellPanel(
               div(textOutput("balance"),style = "font-size:125%")
               )
             )
    ),
    
    
    fluidRow(
      column(12, 
             
             wellPanel(style = "color:red", textOutput("feedback"))
             
      )
    ),
    
    fluidRow(
      column(12,
      
      tabsetPanel(
        
        
        tabPanel("Instructions", 
                 includeHTML("include.html")),
        
        
      tabPanel("Data Table", 
               h5("Summary financial data for the Domino Bus Group"),
               downloadButton('downloadData', 'Download Data', class="btn-xs btn-info"),
               div(tableOutput("c_data_out"), style = "font-size:80%")
               ),
      
      tabPanel("Graphs",
               fluidRow(
                 column(4, 
                        plotOutput("passengers", height = 300)
                 ),
                 column(4,
                        plotOutput("miles", height = 300)
                 ),
                 column(4,
                        plotOutput("revenue", height = 300)
                 )),
               fluidRow(
                 column(4,
                        plotOutput("costs", height = 300)
                 ),
                 column(4,
                        plotOutput("profit", height = 300)
                 ),
                 column(4,
                        plotOutput("cbalance", height = 300)
                 )
               )
               
      ),
      
      tabPanel("About", 
               includeHTML("about.html"))
      
      )
    ))
    
 
    ) #column end
    
)
)