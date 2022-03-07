#' Title
#'
#' @param name A string. The name of what the user wants to search for within Spotify's database
#'
#' @param type A string. The scope of what the search is limited to. Can be one or multiple of "album", "track", "artist"
#'
#' @return
#' @export
spotify_search <- function(track_name = NULL,
                           artist_name = NULL,
                           album_name = NULL) {

  ## Validate provided values
  track_name_c <- convert_spaces(track_name)
  artist_name_c <- convert_spaces(artist_name)
  album_name_c <- convert_spaces(album_name)

  if (!is.null(track_name_c)) {
    track_param <- glue::glue("track:{track_name_c}")
  } else {
    track_param <- NULL
  }

  if (!is.null(artist_name_c)) {
    artist_param <- glue::glue("artist:{artist_name_c}")
  } else {
    artist_param <- NULL
  }

  if (!is.null(album_name_c)) {
    album_param <- glue::glue("album:{album_name_c}")
  } else {
    album_param <- NULL
  }

  ## Defining our base url
  api_url <- "https://api.spotify.com/v1/search?"

  ## Create search query
  s_params <- paste(track_param, artist_param, album_param, sep = "+") %>%
    stringr:: str_remove("\\+$")

  types <- stringr::str_extract_all(s_params, "track|artist|album") %>%
    unlist() %>%
    paste(., collapse = ",")

  search_query <- glue::glue("q={s_params}&type={types}&limit=20")

  request_url <- paste0(api_url, search_query)


  ## Make our request
  request <- httr::GET(
    url = request_url,
    httr::content_type_json()
  )

  if (request$status_code == 200) {

    request_list <- request %>%
      httr::content()

    request_list %>% jsonlite::write_json("C:/Users/Caelan/Documents/spotify.json", pretty = TRUE)

    # track_results <- request_list %>%
    #   purrr::map_dfr(
    #     .f = ~{
    #
    #       id <- .x %>% purrr::pluck("id")
    #       link <- .x %>% purrr::pluck("href")
    #       name <- .x %>% purrr::pluck("name")
    #       popularity <- .x %>% purrr::pluck("popularity")
    #
    #       track_df <- data.frame(
    #         name,
    #         link,
    #         id,
    #         popularity
    #       )
    #
    #     }
    #   )

  } else if (request$status_code == 401) {

    usethis::ui_info("Token expired. Re-authenticating ...")

    authenticate()

    request <- httr::GET(
      url = request_url,
      httr::content_type_json()
    )

    if (request$status_code == 200) {



    } else {

      rlang::abort("Token had expired. Subsequent requests could not be completed...")

    }

  }

  return()
}
