library(rsconnect)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(gridExtra)
library(grid)
library(png)

# ---- Data ----
# headshots <- cfbfastR::cfbd_team_roster(year = 2025) |>
#   select(athlete_id, headshot_url) |>
#   mutate(athlete_id = as.numeric(athlete_id))

# write_csv(headshots, "headshots.csv")

headshots <- read.csv("headshots.csv")

data <- read.csv("26_predictions.csv") |> 
  inner_join(headshots, by = "athlete_id")

# ---- Player Graphic Function ----
create_player_graphic <- function(playername) {
  player_data <- data %>% filter(player == playername)
  if (nrow(player_data) == 0) stop("Player not found.")
  
  player_name <- player_data$player
  headshot_url <- player_data$headshot_url
  
  headshot_grob <- tryCatch({
    if (!is.na(headshot_url) && headshot_url != "") {
      temp <- tempfile(fileext = ".png")
      download.file(headshot_url, temp, mode = "wb", quiet = TRUE)
      img <- png::readPNG(temp)
      rasterGrob(img, interpolate = TRUE)
    } else {
      rectGrob(gp = gpar(fill = "grey80"))
    }
  }, error = function(e) {
    rectGrob(gp = gpar(fill = "grey80"))
  })
  
  table_data <- data.frame(
    c("Team", "Position", "Year", "Height", "Weight",
      "WAA 2025", "Projected WAA 2026"),
    c(player_data$team,
      player_data$position,
      player_data$year_in_league,
      player_data$height,
      player_data$weight,
      round(player_data$waa, 3),
      round(player_data$projected_waa, 3)),
    stringsAsFactors = FALSE
  )
  
  table_grob <- tableGrob(
    table_data, rows = NULL, cols = NULL,
    theme = ttheme_minimal(
      core = list(
        fg_params = list(
          fontface = c(rep("bold", 7), rep("plain", 7))
        )
      )
    )
  )
  
  grid.arrange(
    textGrob(player_name, gp = gpar(fontface = "bold", fontsize = 22)),
    headshot_grob,
    table_grob,
    ncol = 1,
    heights = c(0.15, 0.45, 0.4)
  )
}

# ---- UI ----
ui <- dashboardPage(
  
  dashboardHeader(title = "CFB Player Projection Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("Player Profile", tabName = "profile", icon = icon("user")),
      
      selectInput("position", "Select Position:",
                  choices = sort(unique(data$position)),
                  selected = "QB"),
      
      uiOutput("player_ui")
    )
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "profile",
              fluidRow(
                box(
                  title = "Player Card",
                  width = 12,   # <-- full width now
                  solidHeader = TRUE,
                  status = "primary",
                  plotOutput("playerPlot", height = "550px")
                )
              )
      )
    )
  )
)

## ---- Server ----
server <- function(input, output) {
  
  # Filtered data by position
  filtered_data <- reactive({
    data %>% filter(position == input$position)
  })
  
  # Dynamic player dropdown
  output$player_ui <- renderUI({
    selectInput("player", "Select Player:",
                choices = sort(unique(filtered_data()$player)))
  })
  
  # Player graphic
  output$playerPlot <- renderPlot({
    req(input$player)
    create_player_graphic(input$player)
  })
}

# ---- Run ----
shinyApp(ui = ui, server = server)