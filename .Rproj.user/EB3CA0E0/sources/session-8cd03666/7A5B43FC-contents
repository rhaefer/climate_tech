
observeEvent(input$poi_year,{
  renderMaps_POI_flows(sel_year = input$poi_year)
  cat("\n....... observe new poi_year:", input$poi_year, ".........\n")
})


observeEvent(input$poi_zone,{
  renderMaps_POI_flows(sel_zone = input$poi_zone)
  cat("\n....... observe new poi_zone:", input$poi_zone, ".........\n")
})

observeEvent(input$poi_traveler_type,{
  renderMaps_POI_flows(sel_type = input$poi_traveler_type)
  cat("\n....... observe new poi_traveler_type:", input$poi_traveler_type, ".........\n")
})

# sel_year = poi_year_choices[1]; sel_year
# sel_type = poi_traveler_type_choices[2]; sel_type
# sel_zone = poi_zone_choices[9]; sel_zone

renderMaps_POI_flows = function(sel_year, sel_zone, sel_type){
  
  sel_year = as.numeric(input$poi_year)
  sel_zone = input$poi_zone
  sel_type = input$poi_traveler_type
  
  sel_zone_raw = sel_zone
  if(sel_zone_raw == "Tropicana Field") {sel_zone_raw = "Tropicana Stadium"}
  if(sel_zone_raw == "Clearwater Beach") {sel_zone_raw = "Clear Water Beach"}
  
  # if(sel_zone_raw == "District 7") {sel_zone_raw = "d7"}
  # cat("...... sel_poi_zone..........", sel_zone)
  
  sel_type_lower = tolower(sel_type)
  if(sel_type_lower == "residents") {sel_type_lower = "resident"}
  if(sel_type_lower == "visitors") {sel_type_lower = "visitor"}
  
  # rename to get od (zone) data variables
  sel_type_od = paste0(sel_type_lower, "_d")
  if(sel_type_od == "total_d") {sel_type_od = "tot_d"}
  
  sel_type_flow = sel_type_lower
  if(sel_type_flow == "total") {sel_type_flow = "tot_flow"}
  
  # import data for selected year
  if(sel_year == 2019) {
    poi_od = APP_POI_OD_TABLE_2019
    poi_od_flow = APP_POI_OD_LINK_FLOWS_2019
    gis_cengeo = gis_cengeo19
    gis_d7 = gis_d7_edge19
    poi_od_flow_bins_list = APP_POI_OD_LINK_FLOWS_BINS_LIST[[1]]
  }
  if(sel_year == 2022) {
    poi_od = APP_POI_OD_TABLE_2022
    poi_od_flow = APP_POI_OD_LINK_FLOWS_2022
    gis_cengeo = gis_cengeo22
    gis_d7 = gis_d7_edge22
    poi_od_flow_bins_list = APP_POI_OD_LINK_FLOWS_BINS_LIST[[2]]
  }
  
  # filter data for selected zone
  poi_od = poi_od[[which(gsub("_od", "", names(poi_od)) %in% sel_zone_raw)]]
  poi_od_flow = poi_od_flow[[which(gsub("_od_link_flows", "", names(poi_od_flow)) %in% sel_zone_raw)]]
  poi_od_flow_bins_list = poi_od_flow_bins_list[[which(names(poi_od_flow_bins_list) %in% sel_zone_raw)]]
  
  poi_od = data.table(poi_od)
  poi_od_flow = data.table(poi_od_flow)

  poi_od = poi_od[, visitor_o := visitor_from_outside_o + visitor_from_south_atlantic_o]
  poi_od = poi_od[, visitor_d := visitor_from_outside_d + visitor_from_south_atlantic_d]
  poi_od_flow = poi_od_flow[, visitor := visitor_from_outside + visitor_from_south_atlantic]

  # setView by sel_zone_raw
  sel_lon = poi_latlon[[2]][poi_latlon$poi_name %in% sel_zone_raw]
  sel_lat = poi_latlon[[3]][poi_latlon$poi_name %in% sel_zone_raw]
  sel_zoom = 11
  
  # to add POI gis layer
  sel_gis_POI = gis_POI[gis_POI[["POI_Description"]] %in% sel_zone_raw,]
  
  # join gis file to od zone and flow datafile
  gis_poi_od_zone = merge(gis_cengeo, poi_od, by = "GEOID")
  gis_poi_od_flow = merge(gis_d7, poi_od_flow, by = "stableEdge")
  
  gis_poi_od_zone = gis_poi_od_zone %>% st_cast("MULTIPOLYGON")
  
  sel_gis_poi_od_zone = gis_poi_od_zone[, c("GEOID", sel_type_od)]
  sel_gis_poi_od_zone = sel_gis_poi_od_zone[sel_gis_poi_od_zone[[sel_type_od]]>0, ]

  sel_poi_od = poi_od[, .SD, .SDcols = c("GEOID", sel_type_od)]
  sel_poi_od = sel_poi_od[sel_poi_od[[sel_type_od]]>0, ]
  
  # get natural break points for map
  
  if(sel_type_lower =="visitor"){
    sel_poi_bins_zone <- pretty(sel_poi_od[[sel_type_od]], 4)
  } else {
    if(sel_zone == "Tropicana Field" | sel_zone == "Port Tampa Bay Cruise Terminals and Florida Aquarium" | sel_zone == "Raymond James Stadium" | sel_zone == "Weekiwachee Gardens and Wildlife Areas") {
      sel_poi_bins_zone <- pretty(sel_poi_od[[sel_type_od]], 4)
    } else {
    sel_poi_bins_zone <- round(BAMMtools::getJenksBreaks(sel_poi_od[[sel_type_od]], 6), -1)   
    }
  }

  sel_poi_colorpal_zone <- colorBin(palette = palette_OD_color1, bins= sel_poi_bins_zone)
  
  # take long time to estiamte getJenksBreaks points, thus preprocessed in "data_preparation.R" file. 
  sel_flow_bin = poi_od_flow_bins_list[[which(names(poi_od_flow_bins_list) %in% sel_type_lower)]]
  
  if(sel_type_lower =="visitor" & sel_zone == "Tropicana Field"){
    sel_flow_bin <- pretty(gis_poi_od_flow[[sel_type_flow]], 5)
    if(length(sel_flow_bin)==6) {
      sel_flow_bin = c(0, sel_flow_bin)
    }
  } 
    
  # Disregard the link with volumes less than sel_flow_bin[2] due to slower rendering time 
  sel_poi_od_flow_bins_list = list()
  for(i in 2:(length(sel_flow_bin)-1)) {
    sel_poi_od_flow_bins_list = c(sel_poi_od_flow_bins_list, paste(sel_flow_bin[i], sel_flow_bin[i+1], sep = " - "))
  }
  # poi_sel_bins_color_link = c("#dad7cd", "#778da9", "#415a77", "#1d3557",  "#0d1b2a") #"#222222", 
  poi_sel_bins_color_link = c("#d9ed92", "#34a0a4", "#1a759f", "#1e6091",  "#1d3557") #"#222222", 
  poi_legend_label = factor(unlist(sel_poi_od_flow_bins_list), levels = unlist(sel_poi_od_flow_bins_list))
  # previewColors(colorFactor(poi_sel_bins_color_link, domain = NULL), poi_legend_label)
  poi_legend_colorpal_link <- colorFactor(poi_sel_bins_color_link, domain= NULL)
  
  # Map POI OD  ----------------
  map_poi_od_loads <- reactive({
    
    leaflet() %>%
      addTiles(group = "OSM") %>%
      addProviderTiles(providers$CartoDB.Positron, group ="CartoDB.Positron") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "TonerLite", options = providerTileOptions(noWrap = TRUE)) %>%
      
      addPolygons(
        data = gis_poi_od_zone,
        fillColor = ~sel_poi_colorpal_zone(sel_poi_od[[sel_type_od]]),
        fillOpacity = 0.5,
        color = "#edf6f9",
        weight = 1,
        stroke = TRUE,
        opacity = 0.5,
        layerId = ~ GEOID,
        group = "ZONE_OD",
        # label = ~sel_poi_od[[sel_type_od]],
        highlight = highlightOptions(
          weight = 2, color = 'red', fillOpacity = 0.7, bringToFront = TRUE),
        label = sprintf("<strong>GEOID: %s</strong> <br/> Trips: %s", sel_poi_od$GEOID, sel_poi_od[[sel_type_od]]) %>% 
          lapply(htmltools::HTML)
      ) %>%
      
      # show POI layer
      addPolygons(
        data = sel_gis_POI,
        # fillColor = "red",
        color = "orange",
        fillOpacity = 0.5,
        weight = 3,
        stroke = TRUE,
        layerId = ~ POI_ID,
        group = "POI",
        label = ~ POI_Description
      ) %>%
      
      showGroup(group = "ZONE_OD") %>%
      
      addLayersControl(baseGroups = c("TonerLite", "OSM",  "CartoDB.Positron"),
                       position = "topright",
                       overlayGroups = c("ZONE_OD", "POI"),
                       options = layersControlOptions(collapOD = TRUE)) %>%
      
      setView(lng = sel_lon, lat = sel_lat , zoom = sel_zoom) %>%
      
      addLegend(data = sel_poi_od,
                pal = sel_poi_colorpal_zone, values = ~sel_type_od, title = "Trips", 
                group = "ZONE_OD", position = "bottomright") 
    
  })

  
  output$Map_POI_OD <- renderLeaflet({
    map_poi_od_loads()
  })
  

  
  # Map POI Trip Flow ----------------

  map_poi_flows_loads = reactive({
    
    leaflet() %>%
      addTiles(group = "OSM") %>%
      addProviderTiles(providers$CartoDB.Positron, group ="CartoDB.Positron") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "TonerLite", options = providerTileOptions(noWrap = TRUE)) %>%
      
    # # level 1: smallest
    # addPolylines(
    #   data = gis_poi_od_flow[gis_poi_od_flow[[sel_type_flow]] < sel_flow_bin[2], ],
    #   # fillOpacity = 1,
    #   color = ~ poi_sel_bins_color_link[1],
    #   weight = 1,
    #   stroke = TRUE,
    #   smoothFactor = 0.5,
    #   opacity = 1,
    #   group = "Flow Group 1",
    #   label = ~ tot_flow,
    # 
    #   highlight = highlightOptions(
    #     weight = 1, color = '#0077b6', fillColor = '#0077b6', fillOpacity = 0.7, bringToFront = TRUE)
    # )  %>%
    
    
    # level 2:
    addPolylines(
      data = gis_poi_od_flow[gis_poi_od_flow[[sel_type_flow]] >= sel_flow_bin[2] & gis_poi_od_flow[[sel_type_flow]] < sel_flow_bin[3], ],
      # fillOpacity = 1,
      color = ~ poi_sel_bins_color_link[1],
      weight = 1,
      stroke = TRUE,
      smoothFactor = 0.5,
      opacity = 1,
      group = "Flow Group 1",
      label = ~ tot_flow,
      
      highlight = highlightOptions(
        weight = 2, color = '#0077b6', fillColor = '#0077b6', fillOpacity = 0.7, bringToFront = TRUE)
    )  %>%
    
    # level 3:
    addPolylines(
      data = gis_poi_od_flow[gis_poi_od_flow[[sel_type_flow]] >= sel_flow_bin[3] & gis_poi_od_flow[[sel_type_flow]] < sel_flow_bin[4], ],
      # fillOpacity = 1,
      color = ~ poi_sel_bins_color_link[2],
      weight = 3,
      stroke = TRUE,
      smoothFactor = 0.5,
      opacity = 1,
      group = "Flow Group 2",
      label = ~ tot_flow,
      
      highlight = highlightOptions(
        weight = 4, color = '#0077b6', fillColor = '#0077b6', fillOpacity = 0.7, bringToFront = TRUE)
    )  %>%
    
    # level 4:
    addPolylines(
      data = gis_poi_od_flow[gis_poi_od_flow[[sel_type_flow]] >= sel_flow_bin[4] & gis_poi_od_flow[[sel_type_flow]] < sel_flow_bin[5], ],
      # fillOpacity = 1,
      color = ~ poi_sel_bins_color_link[3],
      weight = 5,
      stroke = TRUE,
      smoothFactor = 0.5,
      opacity = 1,
      group = "Flow Group 3",
      label = ~ tot_flow,
      
      highlight = highlightOptions(
        weight = 6, color = '#0077b6', fillColor = '#0077b6', fillOpacity = 0.7, bringToFront = TRUE)
    )  %>%
    
    # level 5:
    addPolylines(
      data = gis_poi_od_flow[gis_poi_od_flow[[sel_type_flow]] >= sel_flow_bin[5] & gis_poi_od_flow[[sel_type_flow]] < sel_flow_bin[6], ],
      # fillOpacity = 1,
      color = ~ poi_sel_bins_color_link[4],
      weight = 7,
      stroke = TRUE,
      smoothFactor = 0.5,
      opacity = 1,
      group = "Flow Group 4",
      label = ~ tot_flow,
      
      highlight = highlightOptions(
        weight = 8, color = '#0077b6', fillColor = '#0077b6', fillOpacity = 0.7, bringToFront = TRUE)
    )  %>%
    
    # level 6:
    addPolylines(
      data = gis_poi_od_flow[gis_poi_od_flow[[sel_type_flow]] >= sel_flow_bin[6], ],
      # fillOpacity = 1,
      color = ~ poi_sel_bins_color_link[5],
      weight = 9,
      stroke = TRUE,
      smoothFactor = 0.5,
      opacity = 1,
      group = "Flow Group 5",
      label = ~ tot_flow,
      
      highlight = highlightOptions(
        weight = 10, color = '#0077b6', fillColor = '#0077b6', fillOpacity = 0.7, bringToFront = TRUE)
    )  %>%
    
  # hideGroup(group = "Flow Group 1") %>%
    showGroup(group = "Flow Group 1") %>%
    showGroup(group = "Flow Group 2") %>%
    showGroup(group = "Flow Group 3") %>%
    showGroup(group = "Flow Group 4") %>%
    showGroup(group = "Flow Group 5") %>%
    
    addLayersControl(baseGroups = c("TonerLite", "OSM",  "CartoDB.Positron"),
                     position = "topright",
                     overlayGroups = c("Flow Group 1", "Flow Group 2", "Flow Group 3","Flow Group 4","Flow Group 5"),  
                     options = layersControlOptions(collapOD = TRUE)) %>%
    
    # setView(lng = -82.457176, lat = 27.950575, zoom = zone_zoom_level) %>%
    setView(lng = sel_lon, lat = sel_lat , zoom = sel_zoom) %>%
      
    addLegend(pal  = poi_legend_colorpal_link, 
              values = poi_legend_label, 
              title = "Total Trip Link",
              # group = "Flow Group 5", 
              position = "bottomright", 
              opacity = 1)
  })
    
  
  output$Map_POI_Flows <- renderLeaflet({
    map_poi_flows_loads()
  })
  
}

