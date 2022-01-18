#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

# Shiny is an easy environment to develop an interactive web application
# without knowing anything about web programming! Can make an application
# containing widgets that a user can manipulate and get results.

# Google: "shiny widget gallery" to ind widget gallery that has pre-coded
# input widgets that you can use in your program

library(shiny)
library(tidyverse)
library(palmerpenguins)

# Define UI (user interface) for the application -- what do you want it to look like?
ui <- fluidPage(
    titlePanel("This is my Awesome App!"),
    sidebarLayout(
        sidebarPanel("Put my widgets here",
                     radioButtons(inputId = "penguin_species",
                                  label = "Choose penguin species:",
                                  choices = c("Awesome Adelie" = "Adelie", "Cool Chinstrap" = "Chinstrap", "Great Gentoo" = "Gentoo")),
                     selectInput(inputId = "pt_color",
                                 label = "Choose point color:",
                                 choices = c("Really Red" = "red",
                                             "Berry Blue" = "blue",
                                             "Orange Orange" = "orange"))
                     ), # end sidebar panel
        mainPanel("Put my graph here",
                  plotOutput(outputId = "penguin_plot"),
                  tableOutput(outputId = "penguin_table")
                  ) # end mainPanel
    ) # end sidebarLayout
)

# Define server logic
server <- function(input, output) {
    penguin_select <- reactive({
        penguins %>%
            filter(species == input$penguin_species) # looks specifically at input labeled penguin species (from UI)
    }) # end penguin_select reactive

    penguin_table <- reactive({
        penguins %>%
            filter(species == input$penguin_species) %>%
            group_by(sex) %>%
            summarize(mean_flip = mean(flipper_length_mm),
                      mean_mass = mean(body_mass_g))
    }) # end penguin_table reactive

# create a reactive plot that depends on 'species' widget selection:
    output$penguin_plot <- renderPlot({
        ggplot(data = penguin_select(), aes(x = flipper_length_mm, y = body_mass_g)) +
            geom_point(color = input$pt_color)
    }) # end penguin_plot output

    output$penguin_table <- renderTable({
        penguin_table()
    }) # end penguin_table output

}

# Combine into an app to run:
shinyApp(ui = ui, server = server)


