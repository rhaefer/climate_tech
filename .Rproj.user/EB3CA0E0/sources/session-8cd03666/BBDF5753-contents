# source(file.path(getwd(), "config_variables.R"))

## * 3. "Model OD Comparison" ========
shinyjs::useShinyjs()

tabPanel("POI ODs & Flows",
         tabBox(width=12,
                fluidRow(
                  # column(width=3,
                         box(width=2, title="Select Parameters",
                             radioButtons('poi_year',"Select Year", choices = poi_year_choices, selected = poi_year_choices[1], inline=T),
                             selectInput('poi_zone',"Select Point of Interest", choices = poi_zone_choices, selected = poi_zone_choices[1]),
                             radioButtons('poi_traveler_type', "Select Traveler Type", choices = poi_traveler_type_choices, selected = poi_traveler_type_choices[1])
                         ),
                         
                  # ),
                  
                # ),
                # # Results ---------
                # fluidRow(
                  # Trip OD (To) ---------
                  # column(width=9,
                         box(width=5, title="Trip to zone from the selected Point of Interest",
                             leafletOutput("Map_POI_OD", height = 500) %>% withSpinner()
                             ),
                         # ), 
                         # Trip Flows ---------
                  # column(width=5,
                         box(width=5, title="Link-level trip flows from the selected Point of Interest",
                             leafletOutput("Map_POI_Flows", height = 500) # %>% withSpinner()
                         )
                  # )
                )
         ) # tabBox closing
) # tab-panel [Model OD Comparison] closing
