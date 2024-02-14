# NYMTC OD Flow Dashboard 
# Developed by Reid Haefer
# Latest Update on 1/29/2024 

# Read 'import.R' file --------------
### Imports data from processing script and GIS data
#py_install(c('pandas', 'datetime','json','requests','display'))

#reticulate::source_python('energy_gen.py')
source("prep.R")

ui <- dashboardPage(skin="black", 
                    dashboardHeader(title="Climate Tech Ideas",titleWidth = 280,
                                    tags$li(class = "dropdown",
                                            tags$a(href="", target="_blank", 
                                                   tags$img(height = "48px", alt="", src="logo.jpg")))
                    ),
                    dashboardSidebar(disable=F,
                                     uiOutput("ui_dashboard")
                    ),
                    dashboardBody(
                      tabsetPanel(id="tabs",
                                  tabPanel("Tool Info", value="info",
                                           fluidRow(
                                             column(width=6,
                                                    #box(width=12,
                                                    h2("Tool Overview"),
                                                    br(),
                                                    h4("This tool presents potential climate tech solutions and that are the outcome of the Terra.Do Software Stacks for Climate Tech Course. This information is comprised of 3 components which are accessed through the webpage tabs:"),
                                                    tags$ul(
                                                      tags$li(h4("Energy Generation")),
                                                      tags$li(h4("Electric Vehicles")),
                                                      tags$li(h4("Home DIY solutions"))
                                                    ),
                                                    br(), br(),
                                                    h4("This tool was created in Shiny and the full tool code can be found on ",
                                                       a("Github", href = "https://github.com/rhaefer/climate_tech", target = "_blank"), 
                                                       "."),br(),
                                                    h4(textOutput("app_version"), style="font-weight: normal"),
                                                    br(), br(),
                                                    img(src='logo.jpg',  height = 180, width = 250),
                                                    br(), 
                                                    #)
                                             ), 
                                             column(width=6,
                                                    #img(src='banner.png',  height = 400, width = 500) # include page main photograph
                                             )
                                           )
                                           ),
                                  tabPanel("Energy Generation", value="gen",
                                            fluidRow(dataTableOutput("generation_table") %>% withSpinner(),
                                                     plotlyOutput("generation_plot"),
                                                     dataTableOutput("energy_consumed_locally_by_source_ba"), 
                                                     plotlyOutput("energy_consumed_plot"), 
                                                     dataTableOutput("usage_by_ba_and_generation_type")#,
                                                     #plotlyOutput("usage_by_ba_plot"), 
                                           #   column(width=8,
                                           #              leafletOutput('map',height=800) %>% withSpinner()
                                           #          ),
                                           #   column(width=4,
                                           #              valueBoxOutput("selected_zone", width=12),
                                           #              dataTableOutput(outputId ="OD_flow_table"),
                                           #              #style = 'overflow-x: scroll',
                                           #            uiOutput('download_ui')
                                           #          )
                                            )
                                           ), 
                                  tabPanel("Electric Vehicles", value="ev"#,
                                           # fluidRow(
                                           #   column(width=8,
                                           #              leafletOutput('map_network',height=800) %>% withSpinner()
                                           #          )
                                           # )
                                  ), 
                                  tabPanel("DIY Solutions",  value="diy",
                                           fluidRow(box(width=12, title="",
                                             column(width = 6,
                                                    leafletOutput('home_loc_map') %>% withSpinner()
                                                    ),
                                             column(width=6,
                                                    selectInput('input_iot_measure',"Select IOT Measure",
                                                                choices=unique(iot_long$name)),
                                                    uiOutput('alert'),
                                                    plotlyOutput('iot_plot'))
                                           )
                                           ),
                                           fluidRow(dataTableOutput("iot_table")),
                                           fluidRow(
                                             box(width=12, title="Home Energy Model",
                                           plotlyOutput("energy_balance"),
                                           plotlyOutput("energy_model_plot1"),
                                           plotlyOutput("energy_model_plot2")
                                           )
                                  )
                                  )),
                      tags$style(HTML(".skin-black .main-sidebar { background-color: #000000;} .content-wrapper, .right-side {
                                background-color: #FFFFFF ;
                                }") ))
                    
) 
server <- function(input, output, session){
output$app_version  <-  renderText(paste0("Last Updated: ",Sys.Date()))
output$alert  <- renderUI({
  req(input$input_iot_measure)
  if( input$input_iot_measure == "CO2 (ppm) - median" & max(iot_long %>% filter(name== input$input_iot_measure) %>% pull(value)) > 1500 ){
valueBoxOutput('co2_alert')
  } else{
    fluidRow()
  }
})
output$co2_alert<-renderValueBox({
  valueBox(value="Unhealthy CO2 detected", subtitle = "..." , color="red")
})
output$home_loc_map <-  renderLeaflet({
    leaflet() %>% addMarkers(home_longitude +.002, home_latitude +.001) %>% addTiles()
  })
output$energy_model_plot1<- renderPlotly({
  ggplotly(
    baby_energy_model %>% ggplot(aes(timestamp, `Indoor Temperature (C)`)) + geom_col()
  )
})
output$energy_model_plot1<- renderPlotly({
  ggplotly(
    baby_energy_model %>% ggplot(aes(timestamp)) + 
      geom_line(aes(y=`Outdoor Temperature (C)`, color="blue"))+ 
      geom_line(aes(y=`Indoor Temperature (C)`, color="red"))#+ 
      #geom_line(aes(y=hvac_mode))
  )
})
output$energy_model_plot2<- renderPlotly({
  ggplotly(
    baby_energy_model %>% ggplot(aes(timestamp)) + 
      geom_line(aes(y=`Outdoor Temperature (C)`, color="blue"))+ 
      geom_line(aes(y=`Indoor Temperature (C)`, color="red"))#+ 
    #geom_line(aes(y=hvac_mode))
  )
})
output$energy_balance <- renderPlotly({
  ggplotly(
    monthly_energy_balance_tidy %>% ggplot(aes(month, value, fill=variable)) + geom_col()
  )
})
  
output$generation_plot<- renderPlotly({
 ggplotly( 
   local_generation_grid_mix %>% ggplot(aes(period, `Generation (MWh)`, fill=`type-name`)) + geom_bar(stat='identity')
 )
})
output$generation_table  <-renderDataTable({
    local_generation_grid_mix
  })
output$usage_by_ba_and_generation_type  <-renderDataTable({
  usage_by_ba_and_generation_type
})
# output$usage_by_ba_plot <- renderPlotly({
#   ggplotly( 
#     usage_by_ba_and_generation_type %>% View()
#       ggplot(aes(timestamp, `Usage (MWh)`, fill=generation_type)) + geom_area()
#   )
# })
output$energy_consumed_plot <- renderPlotly({
  ggplotly( 
    energy_consumed_locally_by_source_ba %>% ggplot(aes(timestamp, `Power consumed locally (MWh)`, fill=fromba)) + geom_bar(stat='identity')
  )
})
output$energy_consumed_locally_by_source_ba  <-renderDataTable({
  energy_consumed_locally_by_source_ba
})

output$iot_plot  <-renderPlotly({
  req(input$input_iot_measure)
  ggplotly(
  iot_long %>% filter(name== input$input_iot_measure) %>% ggplot(aes(hour, value)) + geom_line()
  )
})

output$iot_table  <-renderDataTable({
 datatable(iot_long %>% filter(name== input$input_iot_measure))
})

  output$ui_dashboard  <- renderUI({
    if(input$tabs=="gen"){
      fluidPage(
        # fluidRow(radioButtons('input_od',"Origin or Destination", choices=c("Origin","Destination"), inline=T)),
        # box(width=12,background = 'black',p(tags$br(),HTML("This map shows trips from origin and destination TAZs throughout the region. Select a TAZ on the map and select Origin or Destination from the sidebar toggle to visualize trips from associated TAZs.")))
      )
    } else if(input$tabs=="ev"){
      ### Default to 2019 data if nothing is selected
      fluidPage(
        # box(width=12,background = 'black',p(tags$br(),HTML("")))
      )
    } else if(input$tabs=="diy"){
      ### Default to 2019 data if nothing is selected
      fluidPage(
        fluidRow(),
        box(width=12,background = 'black',p(tags$br(),HTML("")))
      )
    }
  })
}
shinyApp(ui, server)

