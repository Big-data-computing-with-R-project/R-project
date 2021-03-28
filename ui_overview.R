body_overview <- dashboardBody(class = "site",
  tags$head(
    tags$style(type = "text/css", ".site {overflow-y: hidden; width: 100%;}"),
    tags$style(type = "text/css", "#overview_map {height: 48vh !important;}"),
    tags$style(type = 'text/css', ".slider-animate-button { font-size: 20pt !important; }"),
    tags$style(type = 'text/css', ".slider-animate-container { text-align: left !important; }"),
    tags$style(type = "text/css", "@media (max-width: 991px) { .details { display: flex; flex-direction: column; } }"),
    tags$style(type = "text/css", "@media (max-width: 991px) { .details .map { order: 1; width: 100%; } }"),
    tags$style(type = "text/css", "@media (max-width: 991px) { .details .summary { order: 3; width: 100%; } }"),
    tags$style(type = "text/css", "@media (max-width: 991px) { .details .slider { order: 2; width: 100%; } }"),
    tags$script('
      setHeight = function() {
        var window_height = $(window).height();
        var header_height = $(".main-header").height();

        var boxHeight = window_height - header_height - 30;

        $("#map_container").height(boxHeight);
        $("#map").height(boxHeight - 20);
      };

      // Set input$box_height when the connection is established
      $(document).on("shiny:connected", function(event) {
        setHeight();
      });

      // Refresh the box height on every window resize event    
      $(window).on("resize", function(){
        setHeight();
      });
    ')
  ),
  fluidRow(
    fluidRow(
      uiOutput("box_keyFigures")
    ),
    fluidRow(
      class = "details",
      column(
        box(
          width = 12,
          leafletOutput("overview_map")
        ),
        class = "map",
        width = 7,
        style = 'padding:0px;'
      ),
      column(
        uiOutput("summaryTables"),
        class = "summary",
        width = 5,
        style = 'padding:0px; height: 400px;'
      ),
      column(
      sliderInput(
        "timeSlider",
        label      = "Select date",
        min        = min(data_evolution$date),
        max        = max(data_evolution$date),
        value      = max(data_evolution$date),
        width      = "100%",
        timeFormat = "%d.%m.%Y",
        animate    = animationOptions(loop = TRUE)
      ),
      class = "slider",
      width = 12,
      style = "padding-left: 20px; padding-right: 20px; padding-bottom: 20px;"
    )
    ),
    
    
  )
   
   
)

page_overview <- dashboardPage(
  title   = "Overview",
  header  = dashboardHeader(disable = TRUE),
  sidebar = dashboardSidebar(disable = TRUE,
                             sidebar_fullCollapse = TRUE),
  body    = body_overview
)