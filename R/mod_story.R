mod_story_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "container",
    style = "max-width: 800px; margin: 0 auto; padding: 20px;",
    div(
      class = "story-container",
      h1(textOutput(ns("story_title")), class = "title"),
      div(textOutput(ns("chapter_indicator")), class = "chapter-indicator"),
      div(imageOutput(ns("story_image"), height = NULL), style = "text-align: center; max-width: 100%; height: auto;"),
      div(textOutput(ns("story_text")), class = "story-text"),
      uiOutput(ns("audio_player")),
      br(),
      br(),

      div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        div(
          actionButton(ns("prev_btn"), "Previous",
                       class = "btn-secondary btn-story",
                       icon = icon("arrow-left")),
          actionButton(ns("new_story"), "New Story",
                       class = "btn-warning btn-story",
                       icon = icon("book")),
          actionButton(ns("next_btn"), "Next",
                       class = "btn-secondary btn-story",
                       icon = icon("arrow-right"))
        )
      )
    )
  )
}

mod_story_server <- function(id, rv, main_session) {
  moduleServer(
    id,
    function(input, output, session) {

      # Chapter
      output$chapter_indicator <- renderText({
        req(rv$current_chapter)
        if (is.null(rv$story)) return("")
        paste("Chapter", rv$current_chapter, "of", length(rv$story))
      })

      # Title
      output$story_title <- renderText({
        req(rv$current_chapter)
        if (is.null(rv$story)) return("Your Story Title")
        rv$title
      })

      # Story
      output$story_text <- renderText({
        req(rv$current_chapter)
        if (is.null(rv$story)) return("Your story appears here")
        rv$story[rv$current_chapter]
      })


      # Images
      output$story_image <- renderImage({
        req(rv$current_chapter)
        if (is.null(rv$story)) return(list(src = ""))

        # browser()
        chapter <- rv$current_chapter

        if (!is.null(rv$image_cache[[chapter]])) {
          return(list(
            src = rv$image_cache[[chapter]],
            contentType = "image/png",
            class = "story-image"#,
            # width = "100%"
          ))
        }

        img <- rv$images[[chapter]]
        tmpfile <- tempfile(fileext = ".png")

        if (length(img) == 1) {
          download.file(img, tmpfile, mode = "wb")
        } else {
          writeBin(img, tmpfile)
        }

        rv$image_cache[[chapter]] <- tmpfile

        list(
          src = tmpfile,
          contentType = "image/png",
          class = "story-image"#,
          # width = "100%"
        )
      }, deleteFile = FALSE)


      # Speech
      output$audio_player <- renderUI({
        req(rv$current_chapter)
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
        nav_select("story_nav", "home", session = main_session)
      })

    }
  )
}
