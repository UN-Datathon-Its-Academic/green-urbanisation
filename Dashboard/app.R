library(shiny)
library(bslib)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(sf)
library(shinyjs)
library(FRK)
select <- dplyr::select

source('get-data.R')
merged_df <- merged_df |>
  filter(!city %in% c('Kinshasa', 'Lagos')) |>
  rename(system.index = `system:index`)

if(!exists('mod_list')){
  source('get-model.R')
  names(mod_list)[which(names(mod_list)=='Delhi')] <- 'New-Delhi'
}

datathon_blue <- '#469bd6'
datathon_green <- '#72e146'

bb_df <- read_csv('city-areas.csv') |>
  mutate(Area = (long_max - long_min) * (lat_max - lat_min)) |>
  mutate(Cells = Area * (1 / .05) ^ 2) |>
  mutate(center_long = (long_min + long_max)/2,
         center_lat = (lat_min + lat_max)/2) |>
  rowid_to_column() |>
  filter(!city %in% c('Lagos', 'Kinshasa'))

fullscreen_js <- paste0(readLines('fullscreen-button.js'), collapse = '\n')

ui <- page_sidebar(
  useShinyjs(),
  title =  'Green Urbanisation',
  collapsible = FALSE,
  theme = bslib::bs_theme(
    bootswatch = "flatly",
    fg = '#306d96',
    bg = 'white',
    primary = datathon_green
  ),
  sidebar = sidebar(title = "Select a city",
                    lapply(
                      1:nrow(bb_df),
                      \(x) actionButton(
                        paste0('location', x),
                        icon = icon('fas fa-plane'),
                        label = bb_df$city[[x]]
                      )
                    ),
                    hr(),
                    'Released under MIT License'
  ),
  tags$style(HTML("#floating-input-panel * { color: white; }")),
  column(12,
         tags$style(HTML(".leaflet-top.leaflet-right { top: 20px; right: 20px; }")),
         htmlOutput('map_style'),
         tags$div(
           id = "map_container",
           absolutePanel(top = "10%", right = "2.5%",
                         width = '300px', height = '70vh',
                         style = glue::glue("opacity: .75; z-index: 1000; background-color: {'#306d96'}; border-radius: 25px; overflow: auto;"),
                         column(
                           10,
                           id='floating-input-panel',
                           offset=1,
                           br(),
                           br(),
                           h4(textOutput('current_city'), style='text-align: center'),
                           sliderInput('slider_population_density','Population density',value=1,min=0.5,max=1.5,step = .1),
                           sliderInput('slider_building_area','Building area',value=1,min=0.5,max=1.5,step = .1),
                           sliderInput('slider_greenness','Greenness',value=1,min=0.5,max=1.5,step = .1),
                           radioButtons('covariate',' Mapped variable',
                                        choices=c('Predicted temperature', 'Predicted temperature change', 'Population density','Building area','Greenness')),
                           actionButton('scenario1', 'Scenario 1'),
                           actionButton('scenario2', 'Scenario 2'),
                           actionButton('reset', 'X')
                         )
                         ,
                         draggable = T),
           absolutePanel(top = "4%", right = "2.5%",
                         style = "opacity: 1; z-index: 1000;",
                         actionButton('fullscreen', '',
                                      onclick = "openFullscreen(document.getElementById('map_container'))",
                                      icon = icon('fas fa-expand-arrows-alt'))),
           leafletOutput("temperature_map", height = 'calc(100vh - 120px)')
         )
  ),
  tags$scrip(HTML(fullscreen_js))
)

bb_df2 <- bb_df |>
  rowwise() |>
  mutate(polygon = list(st_polygon(list(
    as.matrix(data.frame(
      lat = c(long_min, long_min, long_max, long_max, long_min),
      lon = c(lat_min, lat_max, lat_max, lat_min, lat_min)
    ))
  )))) |>
  ungroup() |>
  (\(x) st_sf(x |> dplyr::select(-polygon), geometry = x$polygon))(x = _)

merged_df_sf <- merged_df |>
  group_by(city) |> 
  mutate(std.population_density = population_density / max(population_density)) |>
  mutate(std.total_area = total_area / max(total_area)) |>
  mutate(std.greenness = greenness / max(greenness)) |>
  ungroup() |>
  (\(x) st_sf(data=x |> dplyr::select(-geometry), geometry = st_as_sfc(x$geometry)))(x = _) 

mean_temp_anomaly <- merged_df |>
  group_by(city) |>
  summarise(max_temp = max(temperature_anomaly),
            min_temp = min(temperature_anomaly),
            mean_temp = mean(temperature_anomaly))

server <- function(input, output, session) {
  
  observeEvent(input$reset,{
    updateSliderInput('slider_population_density', value = 1, session=session)
    updateSliderInput('slider_building_area', value = 1, session=session)
    updateSliderInput('slider_greenness', value = 1, session=session)
  })
  
  observeEvent(input$scenario1,{
      updateSliderInput('slider_population_density', value = 1, session=session)
      updateSliderInput('slider_building_area', value = 1.4, session=session)
      updateSliderInput('slider_greenness', value = 0.6, session=session)
    })
  
  observeEvent(input$scenario2,{
    updateSliderInput('slider_population_density', value = 1.4, session=session)
    updateSliderInput('slider_building_area', value = 1, session=session)
    updateSliderInput('slider_greenness', value = 1, session=session)
  })
  
  
  
  observe({
    input_btn <- paste0("location", 1:nrow(bb_df))
    lapply(input_btn,
           function(x){
             observeEvent(
               input[[x]],
               {
                 i <- as.numeric(sub("location", "", x))
                 goto_lng <- bb_df[i,'center_long'][[1]]
                 goto_lat <- bb_df[i,'center_lat'][[1]]
                 leafletProxy("temperature_map") %>%
                   flyTo(lng = goto_lng, lat = goto_lat, zoom = 10)
               }
             )
           })
  })
  
  observe({
    if(input$covariate=='Population density'){
      leafletProxy("temperature_map") %>%
        clearGroup('Population density') %>%
        clearGroup('Total area') %>%
        clearGroup('Greenness') %>%
        clearGroup('Predicted temperature') %>%
        clearGroup('Predicted temperature change') %>%
        addPolygons(
          data = merged_df_sf,
          fillColor = ~colorQuantile("YlOrRd", domain=c(0,1))(data.std.population_density),
          fillOpacity = 0.5,
          color=NA,
          weight = 1,
          group = 'Population density',
          options = pathOptions(pane = 'variables')
        )
    } else if(input$covariate=='Building area'){
      leafletProxy("temperature_map") %>%
        clearGroup('Population density') %>%
        clearGroup('Total area') %>%
        clearGroup('Greenness') %>%
        clearGroup('Predicted temperature') %>%
        clearGroup('Predicted temperature change') %>%
        addPolygons(
          data = merged_df_sf,
          fillColor = ~colorQuantile("Blues", domain=c(0,max(data.total_area)))(data.total_area),
          fillOpacity = 0.5,
          color=NA,
          weight = 1,
          group = 'Total area',
          options = pathOptions(pane = 'variables')
        )
    } else if(input$covariate=='Greenness'){
      leafletProxy("temperature_map") %>%
        clearGroup('Population density') %>%
        clearGroup('Total area') %>%
        clearGroup('Greenness') %>%
        clearGroup('Predicted temperature') %>%
        clearGroup('Predicted temperature change') %>%
        
        addPolygons(
          data = merged_df_sf,
          fillColor = ~colorQuantile("YlGn", domain=c(0,1))(data.greenness),
          fillOpacity = 0.5,
          color=NA,
          weight = 1,
          group = 'Greenness',
          options = pathOptions(pane = 'variables')
        )
    } else if(input$covariate=='Predicted temperature'){
      current_city_alt <- str_replace(current_city(), ' ', '-')
      if(length(current_city_alt)!=0) {
        pred <-
          pred_hdci(
            mod_list[[current_city_alt]],
            predict_temp(
              mod_list[[current_city_alt]],
              mult_greenness = input$slider_greenness,
              mult_total_area = input$slider_building_area,
              mult_population_density = input$slider_population_density
            )
          )
        
        non_na <- merged_df[which(merged_df$city==current_city_alt),] |>
          inner_join(pred, by = join_by(system.index)) |>
          pull(pred) |>
          na.omit() |>
          length()
        if(non_na == 0) {
          return()
        } else {
          pred_df_sf <- merged_df[which(merged_df$city==current_city_alt),] |>
            inner_join(pred, by = join_by(system.index)) |>
            rename_with(\(x) str_replace_all(x,'^\\.','')) |>
            mutate(std.pred = (pred - min(pred))/(max(pred)-min(pred))) |>
            (\(x) st_sf(data=x |> dplyr::select(-geometry), geometry = st_as_sfc(x$geometry)))(x = _)
          pred_df_sf <- cbind(pred_df_sf, st_coordinates(st_centroid(pred_df_sf$geometry)))
        }
        
        leafletProxy("temperature_map") %>%
          clearGroup('Population density') %>%
          clearGroup('Total area') %>%
          clearGroup('Greenness') %>%
          clearGroup('Predicted temperature') %>%
          clearGroup('Predicted temperature change') %>%
          addPolygons(
            data = pred_df_sf,
            fillColor = ~colorQuantile("RdYlGn", domain=c(0,1), reverse = F, n=11)(data.std.pred),
            fillOpacity = 0.5,
            color=NA,
            weight = 1,
            group = 'Predicted temperature',
            options = pathOptions(pane = 'variables')
          ) |>
          addLabelOnlyMarkers(
            data = pred_df_sf,
            label = ~round(-data.pred,3),
            lng = ~X,
            lat = ~Y,
            group = 'Predicted temperature',
            labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE),
            options = pathOptions(pane = 'variables')
          )
      }
    }else if(input$covariate=='Predicted temperature change'){
      current_city_alt <- str_replace(current_city(), ' ', '-')
      max_temp <- mean_temp_anomaly |>
        filter(city == current_city_alt) |>
        pull(max_temp)
      min_temp <- mean_temp_anomaly |>
        filter(city == current_city_alt) |>
        pull(min_temp)
      mean_temp <- mean_temp_anomaly |>
        filter(city == current_city_alt) |>
        pull(mean_temp)
      if(length(current_city_alt)!=0) {
        pred <-
          pred_hdci(
            mod_list[[current_city_alt]],
            predict_temp(
              mod_list[[current_city_alt]],
              mult_greenness = input$slider_greenness,
              mult_total_area = input$slider_building_area,
              mult_population_density = input$slider_population_density
            )
          )
        
        non_na <- merged_df[which(merged_df$city==current_city_alt),] |>
          inner_join(pred, by = join_by(system.index)) |>
          pull(pred) |>
          na.omit() |>
          length()
        if(non_na == 0) {
          return()
        } else {
          pred_df_sf <- merged_df[which(merged_df$city==current_city_alt),] |>
            inner_join(pred, by = join_by(system.index)) |>
            rename_with(\(x) str_replace_all(x,'^\\.','')) |>
            mutate(std.pred = pmin(pmax((pred/mean_temp-min_temp/mean_temp)/(max_temp/mean_temp-min_temp/mean_temp),0),1) ) |>
            mutate(temp.change = pred/mean_temp-1) |>
            mutate(temp.change.std = pmin(pmax(temp.change, -1), 2)) |>
            (\(x) st_sf(data=x |> dplyr::select(-geometry), geometry = st_as_sfc(x$geometry)))(x = _)
          pred_df_sf <- cbind(pred_df_sf, st_coordinates(st_centroid(pred_df_sf$geometry)))
        }
        
        leafletProxy("temperature_map") %>%
          clearGroup('Population density') %>%
          clearGroup('Total area') %>%
          clearGroup('Greenness') %>%
          clearGroup('Predicted temperature') %>%
          clearGroup('Predicted temperature change') %>%
          addPolygons(
            data = pred_df_sf,
            fillColor = ~colorQuantile("RdYlGn", domain=c(-1,1), reverse = F, n=11)(data.temp.change.std),
            fillOpacity = 0.5,
            color=NA,
            weight = 1,
            group = 'Predicted temperature',
            options = pathOptions(pane = 'variables')
          ) |>
          addLabelOnlyMarkers(
            data = pred_df_sf,
            label = ~glue::glue("{round(-data.temp.change*100,1)}%"),
            lng = ~X,
            lat = ~Y,
            group = 'Predicted temperature change',
            labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE),
            options = pathOptions(pane = 'variables')
          )
      }
    }
  })
  
  current_city <- reactiveVal(value = c())
  output$current_city <- renderText({current_city()})
  
  observe({
    view_center <- input$temperature_map_center
    lng <- view_center[[1]]
    lat <- view_center[[2]]
    current_city(bb_df[which.min((bb_df$center_long - lng)^2+(bb_df$center_lat - lat)^2),]$city)
  })
  
  output$temperature_map <- renderLeaflet({
    goto_lng <- bb_df[1,'center_long'][[1]]
    goto_lat <- bb_df[1,'center_lat'][[1]]
    leafletProxy("temperature_map") %>%
      flyTo(lng = goto_lng, lat = goto_lat, zoom = 10)
    
    leaflet() |>
      addTiles() |>
      addMapPane("clickable_tiles", zIndex = 430) |>
      addMapPane("variables", zIndex = 420) |>
      addPolygons(
        data = merged_df_sf,
        fillOpacity = 0,
        weight = .5,
        color = scales::alpha("white", alpha = 0.5),
        popup = ~ glue::glue(
          '
          {data.city} <br/>
          Population density: {round(data.population_density,0)} per 25 km2<br/>
          Building area: {signif(data.total_area/1000^2,2)} km2 <br/>
          Greenness index: {round(data.greenness,3)*100}%
          '
        ),
        group = 'bb',
        options = pathOptions(pane = 'clickable_tiles')
      ) |> 
      groupOptions("bb", zoomLevels = 7:20) |> 
      groupOptions("Predicted temperature", zoomLevels = 7:20) |>
      groupOptions('Predicted temperature change',  zoomLevels = 7:20) |>
      flyTo(lng = goto_lng, lat = goto_lat, zoom = 10)
      
  })
}

shinyApp(ui, server)
