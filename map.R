library("htmltools")

addLabel <- function(data) {
  data$label <- paste0(
    '<b>', ifelse(is.na(data$`Province.State`), data$`Country.Region`, data$`Province.State`), '</b><br>
    <table style="width:120px;">
    <tr><td>Confirmed:</td><td align="right">', data$confirmed, '</td></tr>
    <tr><td>Deaths:</td><td align="right">', data$deceased, '</td></tr>
    <tr><td>Recovery:</td><td align="right">', data$recovered, '</td></tr>
    <tr><td>Current Confirmed:</td><td align="right">', data$active, '</td></tr>
    </table>'
  )
  data$label <- lapply(data$label, HTML)
  
  return(data)
}

map <- leaflet(addLabel(data_latests)) %>%
  setMaxBounds(-180, -90, 180, 90) %>%
  setView(0, 20, zoom = 2) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
  addProviderTiles(providers$HERE.satelliteDay, group = "Satellite") %>%
  addLayersControl(
    baseGroups    = c("Light", "Satellite"),
    overlayGroups = c("Confirmed", "Recovered", "Deaths", "Current Confirmed")
  ) %>%
  #hideGroup("Confirmed (per capita)") %>%
  hideGroup("Recovered") %>%
  hideGroup("Deaths") %>%
  hideGroup("Current Confirmed") %>%
  #hideGroup("Active (per capita)") %>%
  addEasyButton(easyButton(
    icon    = "glyphicon glyphicon-globe", title = "Reset zoom",
    onClick = JS("function(btn, map){ map.setView([20, 0], 2); }"))) %>%
  addEasyButton(easyButton(
    icon    = "glyphicon glyphicon-map-marker", title = "Locate Me",
    onClick = JS("function(btn, map){ map.locate({setView: true, maxZoom: 6}); }")))
