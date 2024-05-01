source("global.R")
 
# ## Shiny ui --------------------------
# Get unique state names
state_choices <- unique(df_with_states$states)

ui <- fluidPage(theme = "bootstrap.css",
  titlePanel("Spatial Distribution of Koala"),
  sidebarLayout(
    sidebarPanel(
      h5(strong(paste("Updated on ", Sys.Date()))),
      helpText(p("Please select your filtering criteria")),
      
      # Filter panel for selecting states
      pickerInput("state", "Select State:",
                  choices = state_choices,
                  ## levels is because assesor in factor
                  selected = state_choices,
                  options = list(
                    `actions-box` = TRUE,
                    size = 7,
                    `selected-text-format` = "count > 3"
                  ),
                  multiple = TRUE
      ),
                  
      # Filter panel for selecting variable (temperature or precipitation)
      selectInput("variable", "Select Variable:",
                  choices = c("Temperature" = "temp", "Precipitation" = "precip"))
    ),
    mainPanel(
      # Plot output
      plotOutput("plot", height = "500px", width = "700px")
    )
  )
)
