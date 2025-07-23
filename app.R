library(shiny)
library(bslib)
library(storytelling)

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
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
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
                         label_busy = "The story fairies are hard at work ...")
          )
        )
      )
    ),

    nav_panel(
      "story",
      div(
        class = "container",
        style = "max-width: 800px; margin: 0 auto; padding: 20px;",
        div(
          class = "story-container",
          h1(textOutput("story_title"), class = "title"),
          div(textOutput("chapter_indicator"), class = "chapter-indicator"),
          div(imageOutput("story_image", height = "auto"), style = "text-align: center;"),
          div(textOutput("story_text"), class = "story-text"),
          uiOutput("audio_player"),
          br(),
          br(),

          div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            div(
              actionButton("prev_btn", "Previous",
                           class = "btn-secondary btn-story",
                           icon = icon("arrow-left")),
              actionButton("new_story", "New Story",
                           class = "btn-warning btn-story",
                           icon = icon("book")),
              actionButton("next_btn", "Next",
                           class = "btn-secondary btn-story",
                           icon = icon("arrow-right"))
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  rv <- reactiveValues(
    story = NULL,
    title = NULL,
    images = NULL,
    speech = NULL,
    current_chapter = 1
  )

  # Click the button to create experience
  observeEvent(input$create, {
    req(input$prompt)
    story_data <- create_experience(input$prompt, input$length, input$genre, input$style, input$include_speech)

    rv$story <- story_data$story
    rv$title <- story_data$title
    rv$images <- story_data$images
    rv$speech <- story_data$speech
    # browser()
    nav_select("story_nav", "story")
    removeModal()
  })

  # Chapter
  output$chapter_indicator <- renderText({
    if (is.null(rv$story)) return("")
    paste("Chapter", rv$current_chapter, "of", length(rv$story))
  })

  # Title
  output$story_title <- renderText({
    if (is.null(rv$story)) return("Your Story Title")
    rv$title
  })

  # Story
  output$story_text <- renderText({
    if (is.null(rv$story)) return("Your story appears here")
    rv$story[rv$current_chapter]
  })


  # Images
  output$story_image <- renderImage({
    if (is.null(rv$story)) return(list(src = ""))

    tmpfile <- tempfile(fileext = ".png")
    writeBin(rv$images[[rv$current_chapter]], tmpfile)
    list(
      src = tmpfile,
      contentType = "image/png",
      class = "story-image",
      width = "100%"
    )
  }, deleteFile = TRUE)


  # Speech
  output$audio_player <- renderUI({
    if (is.null(rv$speech)) return(NULL)

    base64_audio <- rv$speech[[rv$current_chapter]]
    data_uri <- paste0("data:audio/mp3;base64,", base64_audio)

    tags$audio(
      controls = NA,
      src = data_uri,
      type = "audio/mp3",
      style = "width: 100%; margin-top: 20px;"
    )
  })


  # Navigation
  observeEvent(input$next_btn, {
    if (!is.null(rv$story) && rv$current_chapter < length(rv$story)) {
      rv$current_chapter <- rv$current_chapter + 1
    }
  })

  observeEvent(input$prev_btn, {
    if (!is.null(rv$story) && rv$current_chapter > 1) {
      rv$current_chapter <- rv$current_chapter - 1
    }
  })

  observeEvent(input$new_story, {
    nav_select("story_nav", "home")
  })


}

shinyApp(ui, server)
