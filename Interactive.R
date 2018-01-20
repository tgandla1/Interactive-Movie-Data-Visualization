# Load shiny
library(stats)
library(shiny)
library(dplyr, warn.conflicts = FALSE)

# Create a UI
ui <- fluidPage("Hello World!")

# Create a server
server <- function(input, output) {}

# Create a shiny app
shinyApp(
    ui = ui,
    server = server)

# Create a UI with I/O controls
ui <- fluidPage(
    titlePanel("Input and Output"),
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                inputId = "number",
                label = "Choose a Number",
                min = 0,
                max = 100,
                value = 25)),
        mainPanel(
            textOutput(
                outputId = "text"))))

# Create a server that maps input to output
server <- function(input, output) {
    output$text <- renderText({
        paste(
            "You selected ",
            input$number)})
}

# Create a shiny app
shinyApp(
    ui = ui,
    server = server)

# Load the color brewer package
library(RColorBrewer)

# Load the dplyr package

# Set working directory
#setwd("/Users/abhinavk/Documents/DV Final Project/r-mastering-data-visualization/7-r-mastering-data-visualization-m7-exercise-files")

# Load Data
movies <- read.csv("Movies.csv")

colors <- brewer.pal(4, "Set1")

# Create a user interface
ui <- fluidPage(
    titlePanel("Interactive Movie Data"),
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                inputId = "year",
                label = "Year",
                min = 2000,
                max = 2015,
                value = c(2000, 2016),
                sep = ""),
            checkboxGroupInput(
                inputId = "rating",
                label = "Rating",
                choices = c("G", "PG", "PG-13", "R"),
                selected = c("G", "PG", "PG-13", "R")),
            textInput(
                inputId = "title",
                label = "Title")),
        mainPanel(
            plotOutput(
                outputId = "plot"))))

# Create a server
server <- function(input, output) {
    output$plot <- renderPlot({
        subset <- movies %>%
            filter(Year >= input$year[1] ) %>%
            filter(Year <= input$year[2]) %>%
            filter(Rating %in% input$rating) %>%
            filter(grepl(input$title, Title)) %>%
            as.data.frame()
        plot(
            x = subset$Critic.Score,
            y = subset$Box.Office,
            col = colors[as.integer(subset$Rating)],
            pch = 19,
            cex = 1.5,
            xlim = c(0, 100),
            ylim = c(0, 800),
            xlab = "Critic Score (%)",
            ylab = "Box Office Revenue ($M)")
        legend(
            x = "topleft",
            as.character(levels(movies$Rating)),
            col = colors[1:4],
            pch = 19,
            cex = 1.5)})
}
movie_tooltip <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.null(x$ID)) return(NULL)

  # Pick out the movie with this ID
  all_movies <- isolate(movies())
  movie <- all_movies[all_movies$ID == x$ID, ]

  paste0("<b>", movie$Title, "</b><br>",
         movie$Year, "<br>",
         "$", format(movie$Box.Office, big.mark = ",", scientific = FALSE)
  )
}



# Create a shiny app
shinyApp(
    ui = ui,
    server = server)
