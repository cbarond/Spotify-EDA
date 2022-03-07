#' @title Authenticate with Spotify's API
#'
#' @description A function to allow the user to authenticate with Spotify's API. This will refresh expired tokens.
#' In order to interact with the Spotify API, please be sure to create an app within their system. See additional references to get started.
#'
#' @seealso Get started with the Spotify API here: https://developer.spotify.com/documentation/web-api/quick-start/
#'
#' @return `NULL`
#' @export
authenticate <- function() {

  ## Adding code that informs the user of what's happening
  usethis::ui_info("Attempting to authenticate ...")

  ## Path required to authenticate in Spotify's web service
  auth_path <- "https://accounts.spotify.com/api/token"

  ## Check if required environment variables are present
  if (nchar(Sys.getenv("SPOTIFY_APP_ID")) == 0 | is.null(Sys.getenv("SPOTIFY_APP_ID"))) {
    rlang::abort("Global environment variable 'SPOTIFY_APP_ID' not found. Please
                 create variable in global environment using 'usethis::edir_r_environ()'")
  }

  if (nchar(Sys.getenv("SPOTIFY_APP_SECRET")) == 0 | is.null(Sys.getenv("SPOTIFY_APP_SECRET"))) {
    rlang::abort("Global environment variable 'SPOTIFY_APP_SECRET' not found. Please
                 create variable in global environment using 'usethis::edir_r_environ()'")
  }

  usethis::ui_info("Creating required headers ...")

  ## Authentication headers
  auth_header <- httr::add_headers(
    "Authorization" = paste0("Basic ", RCurl::base64(
      paste0(
        Sys.getenv("SPOTIFY_APP_ID"),
        ":",
        Sys.getenv("SPOTIFY_APP_SECRET")
        )
      )
    )
  )

  usethis::ui_info("Making API request ...")

  ## Request to authenticate
  request <- httr::POST(
    auth_path,
    auth_header,
    body = list("grant_type" = "client_credentials"),
    encode = "form"
  )

  ## Check response to get valid token and expiration time limit
  if (request$status_code == 200) {

    usethis::ui_info("Request successful!")

    request_content <- request %>% httr::content()

    token_header <- httr::add_headers(
      "Authorization" = paste0("Bearer ", request_content$access_token)
    )

    usethis::ui_info("Setting headers for future requests ...")

    # Set default headers for future requests made
    httr::set_config(
      token_header
    )
  } else {

    rlang::abort(glue::glue("Request return status code {request$status_code}, please check variables used to create request ..."))

  }

  usethis::ui_done("Finished!")

  return(invisible())

}
