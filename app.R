library(shiny)
library(bslib)
# library(firebase)
library(firebase.auth.rest)
library(frstore)
library(purrr)
library(dplyr)



# UI ----------------------------------------------------------------------


ui <- page_fillable(
  theme = bs_theme(
    version = 5,
    primary = "#6a51a3",
    secondary = "#ffb347",
    success = "#4caf50",
    bg = "#f8f5ff",
    fg = "#000000",
    base_font = font_google("Caveat Brush"),
    heading_font = font_google("Fredoka One"),
    font_scale = 1.5
  ),
  lang = "en",
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")#,
    # useFirebase()
  ),
  navset_hidden(
    id = "story_nav",
    nav_panel(
      "home",
      div(
        class = "container",
        style = "max-width: 800px; margin: 0 auto; padding: 20px;",
        h1("Magical Story Weaver", class = "title"),
        p("Create enchanting tales that come alive with illustrations and narration", class = "subtitle"),

        div(
          class = "p-4 mb-4",
          # style = "border-radius: 20px;",
          h3("What story would you like?", style = "text-align: center;"),
          div(
            class = "form-group",
            tags$input(
              id = "prompt",
              type = "text",
              class = "character-input",
              value = "Tell me a story about ",
              style = "width: 100%;"
            )
          ),

          div(
            style = "text-align: center; margin: 30px 0;",
            accordion(
              open = FALSE,
              accordion_panel(
                title = "More Options",
                div(
                  style = "text-align: center; margin: 30px 0;",
                  h5("# of Paragraphs", style = "margin-bottom: 15px;"),
                  sliderInput("length", NULL, min = 2, max = 8, value = 2, step = 1, width = "100%",
                              ticks = FALSE)
                ),
                div(
                  style = "text-align: center; display: flex; justify-content: center; flex-direction: column; align-items: center; margin: 30px 0;",
                  input_switch("include_speech", h5("Add Narration"), FALSE)
                ),
                div(
                  style = "text-align: center; display: flex; justify-content: center; flex-direction: column; align-items: center; margin: 30px 0;",
                  h5("Story Genre", style = "margin-bottom: 15px;"),
                  selectInput(
                    "genre",
                    NULL,
                    c("adventure", "horror", "scifi", "mystery", "thriller", "fantasy", "comedy",
                      "drama", "detective", "satire", "supernatural", "romance", "western"),
                    selected = "adventure"
                  )
                ),
                div(
                  style = "text-align: center; display: flex; justify-content: center; flex-direction: column; align-items: center; margin: 30px 0;",
                  h5("Illustration Style", style = "margin-bottom: 15px;"),
                  selectInput(
                    "style",
                    NULL,
                    c("anime", "comics", "lego_movie", "play_doh", "ethereal_fantasy", "line_art",
                      "origami", "pixel_art", "impressionist", "watercolor", "biomechanical",
                      "retro_futuristic", "fighting_game", "mario", "pokemon", "street_fighter", "horror",
                      "manga", "space", "tilt_shift"),
                    selected = "ethereal_fantasy"
                  )
                )
              )
            )
          ),
          div(
            style = "text-align: center;",
            input_task_button("create", "Create My Story",
                         class = "btn-primary btn-story",
                         style = "font-size: 1.3rem; padding: 12px 40px;",
                         label_busy = "The story fairies are hard at work ..."),
            input_task_button("saved_stories", "Gallery",
                              class = "btn-secondary btn-story",
                              style = "font-size: 1.3rem; padding: 12px 40px;")
          )
        )
      )
    ),

    nav_panel(
      "story",
      mod_story_ui("story_section")
    ),
    nav_panel(
      "saved_stories",
      div(
        style = "text-align: center; display: flex; justify-content: center; flex-direction: column; align-items: center; margin: 30px 0;",
        h5("Genre", style = "margin-bottom: 15px;"),
        uiOutput("available_genre"),
        br(),
        h5("Title", style = "margin-bottom: 15px;"),
        uiOutput("available_titles"),
        br(),
        input_task_button("create2", "Download Story",
                          class = "btn-primary btn-story",
                          style = "font-size: 1.3rem; padding: 12px 40px;",
                          label_busy = "The story fairies are hard at work ..."),
        br(),
        mod_story_ui("downloaded_story")
      )
    )
  )
)






# Server ------------------------------------------------------------------


server <- function(input, output, session) {

  # f <- FirebaseUI$
  #   new(persistence = "local")$
  #   set_providers(
  #     email = TRUE,
  #     google = TRUE
  #   )$
  #   launch()

  admin <- reactive(sign_in(Sys.getenv("EMAIL"), Sys.getenv("PASS")))

  rv <- reactiveValues(
    story = NULL,
    title = NULL,
    images = NULL,
    speech = NULL,
    current_chapter = 1,
    image_cache = vector(mode = "list", length = 8)
  )

  # Click the button to create experience
  observeEvent(input$create, {
    req(input$prompt)
    story_data <- create_experience(input$prompt, input$length, input$genre, input$style, input$include_speech)

    rv$story <- story_data$story
    rv$title <- story_data$title
    rv$images <- story_data$images
    rv$speech <- story_data$speech
    rv$image_cache <- vector(mode = "list", length = 8)
    rv$current_chapter <- 1
    # browser()

    save_story(input$genre, story_data$title, story_data$story, story_data$images, admin()$idToken)

    nav_select("story_nav", "story")
  })


  mod_story_server("story_section", rv, session)


  observeEvent(input$saved_stories, {
    nav_select("story_nav", "saved_stories")
  })





  # Get saved stories -------------------------------------
  all_genre_title <- reactive({
    get_genre_title <- frstore_get(
        "story",
        admin()$idToken,
        fields = c("genre", "title")
      ) |>
        purrr::pluck("documents")

    purrr::map_dfr(get_genre_title, \(x){
      fields <- x$fields
      data.frame(
        genre = fields$genre$stringValue,
        title = fields$title$stringValue
      )
    })
  })

  output$available_genre <- renderUI({
    req(all_genre_title())
    selectInput(
      "genre2",
      NULL,
      unique(all_genre_title()$genre)
    )
  })


  output$available_titles <- renderUI({
    req(all_genre_title(), input$genre2)
    titles <- all_genre_title() |>
      filter(genre == input$genre2) |>
      pull(title)
    selectInput(
      "title",
      NULL,
      titles
    )
  })

  rv2 <- reactiveValues(
    story = NULL,
    title = NULL,
    images = NULL,
    speech = NULL,
    current_chapter = 1,
    image_cache = vector(mode = "list", length = 8)
  )

  observeEvent(input$create2, {
    downloaded_story <- get_story_from_frstore(input$genre2, input$title, admin()$idToken)
    rv2$story <- downloaded_story$story
    rv2$title <- input$title
    rv2$images <- downloaded_story$images
    rv2$speech <- NULL
    rv2$image_cache <- vector(mode = "list", length = 8)
    rv2$current_chapter <- 1

  }, ignoreInit = TRUE)

  mod_story_server("downloaded_story", rv2, session)

}

shinyApp(ui, server)
