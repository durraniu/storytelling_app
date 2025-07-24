library(storytelling)
library(firebase.auth.rest)
library(imguR)
library(magick)
library(frstore)
library(bslib)
library(purrr)


create_experience <- function(user_prompt,
                              num_paras,
                              genre,
                              style,
                              narrate){

  story <- generate_story(user_prompt, num_paras, genre)
  title <- story$title
  image_prompts <- generate_image_prompts(story$story)
  all_images <- generate_images(image_prompts, style)

  if (narrate){
    audios <- generate_audio(story$story, model = "google")
  } else {
    audios <- NULL
  }

  list(
    "story" = story$story,
    "title" = title,
    "images" = all_images,
    "speech" = audios
  )
}





random_id <- function(length = 20) {
  chars <- c(letters, LETTERS, 0:9)
  paste0(sample(chars, length, replace = TRUE), collapse = "")
}


upload_image <- function(img){
  img <- image_read(img)
  tmp <- tempfile(fileext = ".png")
  image_write(img, path = tmp, format = "png")
  uploaded_img <- imgur_upload(tmp)
  uploaded_img$link
}

save_story <- function(genre, title, story, all_images, id_token){
  # upload images
  img_urls <- purrr::map_chr(all_images, upload_image)

  # Save to database
  frstore_create_document(
    paste0("story/", random_id()),
    id_token,
    list(
      fields = list(
        genre = list(stringValue = genre),
        title = list(stringValue = title),
        story = list(
          arrayValue = list(
            values = list(
              lapply(story, \(x) list(stringValue = x))
            )
          )
        ),
        img_urls = list(
          arrayValue = list(
            values = list(
              lapply(img_urls, \(x) list(stringValue = x))
            )
          )
        )
      )
    )
  )
}

# Function for getting a single story from Firestore when genre and title are specified
get_story_from_frstore <- function(genre, title, id_token){
  story_from_frstore <- frstore_run_query(
    "story",
    id_token,
    filters = list(
      list(type = "fieldFilter", field = "genre", op = "EQUAL",
           value_type = "stringValue", value = genre),

      list(type = "fieldFilter", field = "title", op = "EQUAL",
           value_type = "stringValue", value = title)
    )
  )
  one_story <- story_from_frstore[[1]] |> pluck("document")
  fields <- one_story$fields
  list(
    title = fields$title$stringValue,
    story = sapply(fields$story$arrayValue$values, function(x) x$stringValue),
    images = lapply(fields$img_urls$arrayValue$values, `[[`, "stringValue")
  )
}






# story_collection <- frstore_get(
#   "story",
#   admin$idToken
# )
#
# all_stories <- story_collection |>
#   purrr::pluck("documents")
#
#
#
# extracted <- lapply(all_stories, function(story) {
#   fields <- story$fields
#   list(
#     title = fields$title$stringValue,
#     story = sapply(fields$story$arrayValue$values, function(x) x$stringValue),
#     img_urls = lapply(fields$img_urls$arrayValue$values, `[[`, "stringValue")
#   )
# })
