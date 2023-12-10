library(shiny)
library(shinydashboard)
library(shinyjs)
library(data.table)
library(beepr)

# read in data
# source('system1-script2.R')
best.movies <- read.csv('best_movies.csv')
source('read-movies.R')
source('system2-source.R')
ratings.order <- sample(movies$MovieID, 120)
# ratings.movies <- c(unlist(best.movies))
# ratings.movies <- unique(ratings.movies)
# names(ratings.movies) <- NULL
# ratings.order <- sample(ratings.movies, length(ratings.movies)) ## Randomize the order listed


genre.choices = c("Choose a Genre...", 
                  "Animation","Children's","Comedy","Adventure","Fantasy",    
                  "Romance","Drama","Action","Crime","Thriller",   
                  "Horror","Sci-Fi","Documentary","War","Musical",    
                  "Mystery","Film-Noir","Western")

ui <- dashboardPage(
  skin = 'green',
  dashboardHeader(title = "Movie Recommender"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("System I", tabName = "system1"),
      menuItem("System II", tabName = "system2")
    )
  ),
  dashboardBody(
    useShinyjs(),
    div(
      id = "loading_spinner",
      style = "display: none;",  # Initially hide the loading spinner
      align = "center",
      icon("refresh fa-spin", class = "fa-3x")
    ),
    tabItems(
      tabItem(tabName = "system1",
              fluidRow(
                box(
                  width = 12,
                  title = "Select a Genre",
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  selectInput("genre_dropdown", "Select a Genre",
                              choices = genre.choices,
                              selected = genre.choices[1]),
                  uiOutput('selectedGenre')
                )
              ),
              fluidRow(
                # useShinyjs(),
                box(
                  width = 12,
                  title = "Genre Recommendations",
                  status = "success",
                  solidHeader = TRUE,
                  actionButton('system1_button', 'Click Here to See Recommendations'),
                  br(),
                  br(),
                  # textOutput("system1_output2")
                  tableOutput("system1_output")
                )
              )
      ),
      tabItem(tabName = "system2",
              fluidRow(
                box(
                  width = 12,
                  title = "Step 1: Rate Movies You Know",
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  tableOutput("system2_ratings")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  title = 'Step 2: Discover New Movies! (Will Take Several Minutes)',
                  status = 'success',
                  solidHeader = TRUE,
                  actionButton('system2_button', 'Click Here to See Recommendations'),
                  br(),
                  br(),
                  tableOutput('system2_results')
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive expression to update content based on dropdown selection
  selected_option <- eventReactive(input$system1_button, {
    input$genre_dropdown
    # output$selectedGenre
  })
  
  # Reactive button observer for System 1
  observeEvent(input$system1_button, {
    # Pull the genre when the button is clicked
    selected_value <- selected_option()
    # Print 10 recommended movies to the page
    output$system1_output <- renderUI({
      if(!is.null(selected_value) && which(genre.choices == selected_option()) != 1){
        rows <- 2
        movies.per.row <- 5
        sel.movie.ids <- best.movies[,which(genre.choices == selected_option()) - 1]
        small_image_url = "https://liangfgithub.github.io/MovieImages/"
        
        lapply(1:rows, function(i){
          list(fluidRow(lapply(1:movies.per.row, function(j){
            box(
              width = 2,
              status = 'success',
              solidHeader = TRUE,
              title = paste('#', (i - 1) * movies.per.row + j),
              div(style="text-align:center",
                a(
                img(src = paste0(small_image_url, substring(sel.movie.ids[(i-1)*movies.per.row+j],2),'.jpg?raw=true')
                    , height = 150)
                )
              ),
              div(style="text-align:center",
                  movies[movies$MovieID == sel.movie.ids[(i-1)*movies.per.row+j],2]
              )
            )
          })))
        })
      }
    })
  })
  
  user_ratings <- reactiveValues()
  # Render text output for Section 2 (you can customize this based on your needs)
  output$system2_ratings <- renderUI({
    ratings.rows <- 20
    rat.movies.per.row <- 6
    small_image_url = "https://liangfgithub.github.io/MovieImages/"
    show.movies <- lapply(1:ratings.rows, function(i){
      list(fluidRow(lapply(1:rat.movies.per.row, function(j){
        if ((i-1)*rat.movies.per.row+j <= length(ratings.order)){
          box(
            width = 2,
            div(style = 'text-align:center',
                a(
                  img(src=paste0(
                    small_image_url, 
                    substring(ratings.order[(i-1)*rat.movies.per.row+j],2),
                    '.jpg?raw=true'
                  )
                  , height = 150)
                )),
            div(style = 'text-align:center',
                movies[
                  movies$MovieID == ratings.order[(i-1)*rat.movies.per.row+j]
                  ,2
                ]),
            div(style = 'text-align:center',
                radioButtons(
                  paste0(
                    ratings.order[(i-1)*rat.movies.per.row+j],
                    '_rating'
                  ),
                  label = 'Your Rating',
                  choices = 1:5,
                  selected = NA,
                  inline = TRUE
                )
            )
          )
        }
      })))
    })
    # Make the table scrollable
    fluidPage(
      tags$style(type = 'text/css', "div.scrollable { max-height: 600px; overflow-y: auto; }"),
      div(class = "scrollable", show.movies)
    )
    
  })
  
  observeEvent(input$system2_button, {
    shinyjs::enable('loading_spinner')
    
    ratings.rows <- 20
    rat.movies.per.row <- 6
    lapply(1:ratings.rows, function(i){
      lapply(1:rat.movies.per.row, function(j){
        if ((i-1)*rat.movies.per.row+j <= length(ratings.order)){
          rating.id <- paste0(ratings.order[(i-1)*rat.movies.per.row+j], '_rating')
          if (is.null(input[[rating.id]])){
            user_ratings[[rating.id]] <- NA
          } else {
            user_ratings[[rating.id]] <- as.numeric(input[[rating.id]])
          }
        }
      })
    })
    
    tmp.newuser <- lapply(1:ratings.rows, function(i){
      lapply(1:rat.movies.per.row, function(j){
        if((i-1)*rat.movies.per.row+j <= length(ratings.order)){
          rating.id <- paste0(ratings.order[(i-1)*rat.movies.per.row+j], '_rating')
          return (user_ratings[[rating.id]])
        }
      })
    })
    tmp.newuser <- unlist(tmp.newuser)
    names(tmp.newuser) <- ratings.order
    
    tmp.newuser <- tmp.newuser[!is.na(tmp.newuser)]
    newuser[,match(names(tmp.newuser), colnames(newuser))] <- tmp.newuser
    newuser.recs <- myIBCF(newuser)
    beep(2)
    print(newuser.recs)
    
    shinyjs::disable('loading_spinner')
    
    output$system2_results <- renderUI({
      rows <- 2
      movies.per.row <- 5
      sel.movie.ids <- names(newuser.recs)
      small_image_url = "https://liangfgithub.github.io/MovieImages/"
      
      lapply(1:rows, function(i){
        list(fluidRow(lapply(1:movies.per.row, function(j){
          box(
            width = 2,
            status = 'success',
            solidHeader = TRUE,
            title = paste('#', (i - 1) * movies.per.row + j),
            div(style="text-align:center",
                a(
                  img(src = paste0(small_image_url, substring(sel.movie.ids[(i-1)*movies.per.row+j],2),'.jpg?raw=true')
                      , height = 150)
                )
            ),
            div(style="text-align:center",
                movies[movies$MovieID == sel.movie.ids[(i-1)*movies.per.row+j],2]
            )
          )
        })))
      })
    })
  })
  
  # Hide/show content based on selected tab
  observe({
    if (!is.null(input$tabName) && length(input$tabName) > 0) {
      if (input$tabName == "system1") {
        shinyjs::enable("system1_output")
        shinyjs::enable("genre_dropdown")
        shinyjs::enable("system1_button")
        shinyjs::disable("system2_ratings")
        shinyjs::disable("system2_button")
        shinyjs::disable("system2_results")
      } else if (input$tabName == "system2") {
        shinyjs::enable("system2_ratings")
        shinyjs::enable("system2_button")
        shinyjs::enable("system2_results")
        shinyjs::disable("system1_output")
        shinyjs::disable("system1_button")
        shinyjs::disable("genre_dropdown")
      }
    }
  })
}

shinyApp(ui, server)
