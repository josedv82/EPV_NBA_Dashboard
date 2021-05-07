
############# 

#Jose Fernandez
#Jun 2021


#############

library(shiny)
library(shinyjs)
library(sparkline)
library(tidyverse)

source("data.R")

#UI
ui <- fluidPage(
   
  #theming   
  theme = shinythemes::shinytheme("cyborg"),
  tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: black !important;}')),
  tags$head(tags$style("#modal1 .modal-body {background-color: white; padding: 10px}
                       #modal1 .modal-content  {-webkit-border-radius: 6px !important;-moz-border-radius: 6px !important;border-radius: 6px !important;}
                       #modal1 .modal-dialog { width: 880px; display: inline-block; text-align: left; vertical-align: top;}
                       #modal1 .modal-header {background-color: #404764; border-top-left-radius: 6px; border-top-right-radius: 6px}
                       #modal1 .modal { text-align: center; padding-right:0px; padding-top: 24px;}
                       #moda1 .close { font-size: 16px}")),
  
  
  #dependencies
  shinyjs::useShinyjs(),
  htmlwidgets::getDependency('sparkline'),
  
  tags$br(),
  
  #sidebar
  sidebarLayout(
    fluid = T,
    sidebarPanel(width = 2, style = "position:fixed; width:16%",
                 
                
                actionButton("info", "Info", icon = icon("info-circle"), style = "border-color: #16a085; color: #16a085"),
                actionButton("code", "Code", icon = icon("github"), style = "border-color: #16a085; color: #16a085", onclick ="window.open('https://github.com/josedv82/EPV_dashboard/blob/main/README.md', '_blank')"),
                actionButton("stat", "Stats", icon = icon("table") ,style = "border-color: #16a085; color: #16a085", onclick ="window.open('https://www.espn.com/nba/matchup?gameId=400488901', '_blank')"),
                
                tags$br(),
                tags$hr(),
                tags$br(),
                
                tags$h6("Miami Heat @ Brooklyn Nets", style = "color:white;"),
                tags$div(HTML('<i class="fa fa-user-alt" style = "color:#16a085;"></i>   Attendance: 17,332')),
                tags$div(HTML('<i class="fa fa-calendar-alt" style = "color:#16a085;"></i>   Nov 1st, 2013')),
                tags$div(HTML('<i class="fa fa-map-marker-alt" style = "color:#16a085;"></i>   Brooklyn, NY')),
                
                tags$br(),
                tags$hr(),
                tags$br(),
                 
                 #team filter
                shinyWidgets::checkboxGroupButtons(
                   inputId = "teamf",
                   label = "Select Team(s)",
                   choices = c("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/brooklyn-nets-logo.png' width=60px></img>" = "Home", 
                               "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/miami-heat-logo.png' width=60px></img>"  = "Away"),
                   checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                   justified = TRUE,
                   selected = c("Home", "Away")
                 ),
                
                tags$br(),
                tags$br(),
                
                
                #select view filter
                shinyWidgets::prettyRadioButtons(
                  inputId = "view1",
                  label = "Select View", 
                  choices = c("Play by Play", "EPV Added"),
                  icon = icon("check"), 
                  bigger = TRUE,
                  status = "info",
                  animation = "jelly",
                  selected = "Play by Play"
                ),
                
                
                
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$hr(),
                #links
                p("Designed by Jose Fernandez", style = "color: #717d7e"),
                actionButton(inputId='twitter', label="", icon = icon("twitter", lib = "font-awesome"), 
                             onclick ="window.open('https://twitter.com/jfernandez__', '_blank')"),
                actionButton(inputId='github', label="", icon = icon("github", lib = "font-awesome"), 
                             onclick ="window.open('https://github.com/josedv82', '_blank')"),
                a(actionButton(inputId = "email", label = "", icon = icon("envelope", lib = "font-awesome")),
                  href="mailto:jose.fernandezdv@gmail.com")
    
    ),
    
      # table output
      mainPanel(width = 10,
        #conditions
        conditionalPanel(condition = "input.view1",
         DT::dataTableOutput("table")
        ),
        
        conditionalPanel(condition= "input.view1",
                         reactable::reactableOutput("epva")
        )
      )
        
      )
   )

# Define server logic 
server <- function(input, output, session) {
  
  #info modal
  observeEvent(input$info, {
    
    showModal(tags$div(id="modal1", modalDialog(
      inputId = 'Dialog1', 
      title = HTML('<span style="color:white; font-size: 20px; font-weight:bold; font-family:sans-serif ">About this app<span>
                   <button type = "button" class="close" data-dismiss="modal" ">
                   <span style="color:white; ">x <span>
                   </button> '),
      tags$br(),
      p("This is a conceptual dashboard design to visualize and interact with Expected Possession Value (EPV) in the NBA.
              It intends to have some resemblance with common stock trading apps, highlighting the EPV trend and overall change from
              the start to end of the possession.", style = "color: #212f3c"),
      p("For simplicity, this app only shows one game (Heat @ Nets, Nov 1st, 2013).", style = "color: #212f3c"),
      tags$br(),
      tags$h4("What is EPV?", style = "color: black"),
      p("In brief, EPV is a framework for using optical player tracking data to estimate, in real time, the expected number of points obtained by the end of a possession (Cervone et al, 2016).", style = "color: #212f3c"),
      tags$br(),
      img(src='https://grantland.com/wp-content/uploads/2014/02/epv-chart-features.png', width="60%", align = "middle"),
      tags$br(),
      HTML(paste(tags$span("For more information about EPV visit the original", style = "color: #212f3c"), tags$span(tags$a(href= "https://arxiv.org/pdf/1408.0777.pdf", "work")), 
                 tags$span("by Cervone et al, as well as this", style = "color: #212f3c"),
                 tags$span(tags$a(href= "https://grantland.com/the-triangle/behind-databall-a-discussion-on-the-methodology-of-expected-points-value/", "interview")),
                 tags$span("by the authors.", style = "color: #212f3c"))),
      tags$br(),
      tags$br(),
      tags$h4("The data", style = "color: black"),
      HTML(paste(tags$span("The data used throughout this app was extracted from Dan Cervone's EPV demo repository", style = "color: #212f3c"),
      tags$span(tags$a(href="https://github.com/dcervone/EPVDemo ", "here.")))),
      tags$br(),
      tags$br(),
      easyClose = TRUE,
      footer = NULL )))
    
  })
  
  #reactive dataset for pbp filtering
  aa <- reactive({
    
    req(input$teamf)
    
    a %>% filter(Offense1 %in% input$teamf)
    })
                 
  aaa <- reactive({ aa() %>%   
      
    mutate(Animation = sapply(1:nrow(aa()), function(i){
      sprintf("<button id='inf%d' type='button' class='btn btn-default action-button shiny-bound-input'><i class= 'glyphicon glyphicon-plus-sign'/></button>", i)
    })) %>%
    
    formattable::formattable(
      
      list(
        `EPV Difference` = formattable::formatter("span", 
                                                  style = x ~ formattable::style(display = "block", 
                                                                                 padding = "0 4px", 
                                                                                 `border-radius` = "4px",
                                                                                 
                                                                                 `background-color` = ifelse(x > 0, "springgreen", ifelse(x < 0, "tomato", "gray"))),
                                                  x ~ ifelse(x > 0, paste("+", x, sep = ""), x))
        
        
      )
      
    )
  })
  
  
  #EPV table output
  output$table <- DT::renderDT({
    
    validate(
      need(input$teamf != "", "Please select at least one team")
    )
    
    formattable::as.datatable(aaa() %>% 
          select(-Offense1) %>%
          select(Period, 
          `Game Clock` = game_clock, 
          `Poss N` = Poss,
          Offense,
         `Play by Play` = Play_by_Play, 
          Score, 
          `EPV Trend` = EPV_Trend,
          `EPV Difference`,
          Animation), 
         
                                rownames = FALSE,
                                escape = F,
                                selection = "single",
                                class = 'white-space: nowrap',
                                #extensions = 'Responsive',
                                options = list(dom = 'tp',
                                               pageLength = 500,
                                               bSort=F,
                                               paging = F,
                                               #scrollY = "800px",
                                               #scrollX = F,
                                               drawCallback = htmlwidgets::JS('function(){debugger;HTMLWidgets.staticRender();}'),
                                               columnDefs = list(
                                                            list(className = 'dt-center', targets = c(0,1,2,3,5,6,7,8)),
                                                            list(width = '50px', targets = c(0:3))
                                                           ), 
                                                      
                                                      paging = F)) %>%
                                 
                                DT::formatStyle(c(1:9), backgroundColor = 'black') %>%
                                DT::formatStyle(c(8,9), color = 'black') %>%
                                DT::formatStyle(c(1:dim(aaa())[2]), border = '2px solid #060806')
                                 
                                  })
                                 
                                  
  
  #show EPV gif in modal on click 
  observe(
  for(i in 1:nrow(aa())){
  shinyjs::onclick(paste0("inf",i), {
      
      showModal(
        modalDialog(
          img(src = paste0("zplay_", aa()$Poss[input$table_rows_selected], ".gif", sep = ""), width="100%"),
          footer = NULL,
          size = "l",
          easyClose = T
          
        )
      )
      
    })
    
  }
  )
  
  
  #EPVA table
  output$epva <- reactable::renderReactable({
    EPVA_tab
    
  })
  
  
  #dynamic showing/hiding based on filter selection
  observeEvent(input$view1,{
  if(input$view1 == "Play by Play"){
    shinyjs::show("table")
    shinyjs::hide("epva")
  }else{
  shinyjs::hide("table")
  shinyjs::show("epva") 
  }
  })
  
  
}
  

# Run the application 
shinyApp(ui = ui, server = server)

