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
                      useShinyjs(), # Initialize shinyjs
                      tags$head(
                        tags$style(HTML(
                                 ".nav-tabs {font-size: 20px} 
                                 "))), 
                      tabsetPanel(id="tabs",
                                  tabPanel("Tool Info", value="info",
                                           fluidRow(
                                             column(width=6,
                                                    #box(width=12,
                                                    h2("Tool Overview"),
                                                    br(),
                                                    h4("This tool presents potential climate tech solutions and that are the outcome of the Terra.Do Software Stacks for Climate Tech Course. This information is comprised of 3 components which are accessed through the webpage tabs:"),
                                                    tags$ul(
                                                      tags$li(h4("Home DIY solutions")),
                                                      tags$li(h4("Electric Vehicle Solutions")),
                                                    ),
                                                    br(), 
                                                    img(src='logo.jpg',  height = 280, width = 400),
                                                    h4("This tool was created in Shiny and the full tool code can be found on ",
                                                       a("Github", href = "https://github.com/rhaefer/climate_tech", target = "_blank"), 
                                                       "."),
                                                    h4(textOutput("app_version"), style="font-weight: normal"),
                                                    br(), br(),
                                                    br(), 
                                                    #)
                                             ), 
                                             column(width=6,
                                                    #img(src='banner.png',  height = 400, width = 500) # include page main photograph
                                             )
                                           )
                                           ),
                                  tabPanel("Home/DIY Solutions",  value="diy",
                                           fluidRow(box(width=12, title="",
                                                        column(width = 6,
                                                               leafletOutput('home_loc_map') %>% withSpinner()
                                                        ),
                                                        column(width=6,
                                                               fluidRow(textOutput("test")
                                                               ),
                                                               fluidRow(
                                                                 plotlyOutput('iot_plot') %>% withSpinner()))
                                           )
                                           ),
                                           #fluidRow(dataTableOutput("iot_table")),
                                           fluidRow(plotlyOutput('hist_temp_plot')),
                                           fluidRow(
                                             box(width=12, title="Home Energy Model",
                                                 plotlyOutput("energy_balance"),
                                                 # plotlyOutput("energy_model_plot1"),
                                                 plotlyOutput("energy_model_plot2")
                                             )
                                           )
                                  ),
                                  tabPanel("Electric Vehicle Solutions", value="gen",
                                           fluidRow(leafletOutput('big_map', height = 600)),
                                            fluidRow(box(title="Energy Generation", width=12,
                                                         dataTableOutput("generation_table") %>% withSpinner(),
                                                     plotlyOutput("generation_plot")),
                                                     box(title="Energy Consumed Locally (BA & Source)",width=12,
                                                     dataTableOutput("energy_consumed_locally_by_source_ba"), 
                                                     plotlyOutput("energy_consumed_plot")), 
                                                     box(title="Energy Usage (BA & Generation Type)",width=12,
                                                     dataTableOutput("usage_by_ba_and_generation_type")),
                                                     box(title="Census LODES Commute Origin Destinations",width=12,dataTableOutput('lodes')
                                                     )
                                            )
                                           ),
                                  tabPanel("More Info", value="doc",
                                           box(width = 12, status = 'primary', solidHeader = TRUE, title = "More Info",
                                               tags$iframe(style="height:800px; width:100%", src="climate_tech1.pdf")
                                           ))
                                  ),
                      tags$style(HTML(".skin-black .main-sidebar { background-color: #000000;} .content-wrapper, .right-side {
                                background-color: #FFFFFF ;
                                }") ))
                    
) 
server <- function(input, output, session){
big_map <-reactive({
  mapview(heavy_infra, col.regions="green", layer.name="Hydrogen Charging Stations") + 
    mapview(e_fuel_cor, layer.name="EV Fuel Corridors (NEVI)") + 
    mapview(charger_nevi, col.regions="red", layer.name="DC Chargers (NEVI Compliant)") + 
    mapview(charger_no_nevi, col.regions="orange" , layer.name="DC Chargers (Non NEVI)") + 
    mapview(ba_geo, col.regions="grey", layer.name="Balancing Authorities", hide = TRUE) + 
    mapview(aadt, col.regions="black", layer.name="Traffic Volumes", hide = TRUE) + 
    mapview(tracts, col.regions="yellow", layer.name="Census Tracts", hide = TRUE)
  })
  output$big_map<-renderLeaflet({
    big_map()@map
  })
  
output$lodes  <-renderDataTable({
    datatable(lodes)
  })
output$hist_temp_plot  <-renderPlotly({
  req(input$input_weather_var)
  ggplotly(
    weather %>%
      filter(name==input$input_weather_var) %>%  filter(date >= input$input_weather_date[1] & date <= input$input_weather_date[2]) %>%
      ggplot(aes(datetime, value)) + geom_line() + theme_minimal() + geom_smooth() + ggtitle(as.character(input$input_weather_var))
  )
  })

#unhealthy_co2_date<-
output$app_version  <-  renderText(paste0("Last Updated: ",Sys.Date()))
most_recent_date_unhealthy_co2 <-reactive({
  iot_long %>% filter(name== input$input_iot_measure) %>% filter(value > 1500) %>% arrange(desc(value)) %>% slice(1) %>% pull(hour)
}) 
most_recent_amount_unhealthy_co2 <-reactive({
  iot_long %>% filter(name== input$input_iot_measure) %>% filter(value > 1500) %>% arrange(desc(value)) %>% slice(1) %>% pull(value)
}) 
output$test<-renderText({
  req(input$input_iot_measure)
  most_recent_date_unhealthy_co2()
})
observe({
  req(input$input_iot_measure)
  if (input$input_iot_measure == "CO2 (ppm) - median" & max(iot_long %>% filter(name== input$input_iot_measure) %>% pull(value),na.rm = T) > 1500) {
    showModal(modalDialog(
      title = span(paste0("Unhealthy CO2 levels detected - Most recent date & level: ",most_recent_date_unhealthy_co2(),", " ,most_recent_amount_unhealthy_co2()), 
                   style = "color: white;"), # Inline style for the title
      tags$div("This is a warning that unhealthy CO2 levels have been detected (above 1500 ppm). Please ensure ventilation is adequate.",
               style = "color: white; background-color: #710505; padding: 1px;"), # Inline style for the body
      footer = modalButton("Close"),
      size = "l",
      easyClose = TRUE,
      fade = TRUE,
      # Applying inline styles to the modal dialog itself
      tags$style(HTML("
        .modal-header {background-color: #710505 !important; color: white !important;}
        .modal-footer {background-color: #710505 !important; color: white !important;}
        .modal-footer .btn {color: #710505 !important; background-color: white !important;}
      "))
    ))
  }
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
  iot_long %>% filter(name== input$input_iot_measure) %>% ggplot(aes(hour, value)) + geom_line() +ggtitle(input$input_iot_measure)
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
    } else if(input$tabs=="diy"){
      ### Default to 2019 data if nothing is selected
      fluidPage(
        fluidRow(
          selectInput('input_iot_measure',"Select Home Sensor Variable",
                      choices=unique(iot_long$name)),
          selectInput('input_weather_var',"Select Historic Weather Variable",
                      choices=unique(weather$name)),
          br(), br(), br(), 
          dateRangeInput('input_weather_date',"Select Historic Date Range",
                      start=min(weather$date, na.rm=T),
                      end=max(weather$date, na.rm=T))
          ),
        box(width=12,background = 'black',p(tags$br(),HTML("")))
      )
    }
  })
}
shinyApp(ui, server)

