elizabeth_ui = fluidPage(
  titlePanel(strong("Change in Carbon Dioxide by Country in 2020")),
  sidebarLayout(
    sliderInput(inputId = "date",
              label = "Date:",
              min = as.Date("2020-01-01", "%Y-%m-%d"),
              max = as.Date("2020-12-31", "%Y-%m-%d"),
              value = as.Date("2020-01-01"),
              timeFormat = "%Y-%m-%d"),
    mainPanel(
      plotOutput("map", width = 800, height = 550),
      textOutput("max_country_text")
      )
)) 
