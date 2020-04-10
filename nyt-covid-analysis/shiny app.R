library(shiny)
library(dplyr)

ui <- fluidPage(
  headerPanel("COVID-19"),
  sidebarLayout(
    sidebarPanel(
      p("County maps"),
      selectInput("state", "State", state.name)
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    day <-
      nyt_counties %>% 
      pull(date) %>% 
      unique() %>% 
      sort(decreasing = TRUE) %>% 
      .[1]
    counties_sf <- urbnmapr::get_urbn_map("counties", sf = TRUE)
    intendedstate <-
      nyt_counties %>% 
      filter(state == input$state, date == day) %>% 
      right_join(counties_sf %>% filter(state_name == input$state),
                 by = c("fips" = "county_fips")) %>% 
      mutate(cases = ifelse(is.na(cases), 0, cases),
             deaths = ifelse(is.na(deaths), 0, deaths))
    ggplot() +
      geom_sf(intendedstate,
              mapping = aes(fill = cases, geometry = geometry),
              color = "white", size = 0.05) +
      coord_sf(datum = NA) +
      labs(fill = "Confirmed Cases",
           title = paste0(input$state, " COVID-19 Cases as of ", day))
  })
}

shinyApp(ui, server)