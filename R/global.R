library(storytelling)

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
    audios <- generate_audio(story$story, model = "cloudflare")
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
