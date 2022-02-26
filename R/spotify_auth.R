#' @title Authenticate with Spotify's API
#'
#' @description A function to allow the user to authenticate with Spotify's API. This will refresh expired tokens.
#'
#' @return `NULL`
#' @export
spotify_auth <- function() {

  ## Path required to authenticate in Spotify's web service
  auth_path <- "https://accounts.spotify.com/api/token"

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

  content_type <- httr::content_type("application/x-www-form-urlencoded")

  body <- list(
    "grant_type" = "client_credentials"
  )

  ## Request to authenticate
  request <- httr::POST(
    auth_path,
    auth_header,
    body = list("grant_type" = "client_credentials"),
    encode = "form"
  )

  ## Check response to get valid token and expiration time limit
  if (request$status_code == 200) {
    request_content <- request %>% httr::content()

    ## TRry to create a timer to let
    # request_time <- Sys.time()
    #
    # as.numeric(as.POSIXct(Sys.time())) - as.numeric(as.POSIXct(request_time))
    #
    # # request_time <- stringr::str_extract(Sys.time(), "[[:digit:]]{2}:[[:digit:]]{2}:[[:digit:]]{2}") %>% lubridate::hms()

    token_header <- httr::add_headers(
      "Authorization" = paste0("Bearer ", request_content$access_token)
    )

    # Set default headers for future requests made
    httr::set_config(
      token_header
    )
  }

  return(invisible())

}
