track_api_path <- "https://api.spotify.com/v1/tracks/"
track_id <- "11dFghVXANMlKmJXsNCbNl"

request_url <- paste0(track_api_path, track_id)

track_request <- httr::GET(
  url = request_url,
  httr::content_type_json()
)


if (request$status_code == 200) {
  track_list <- track_request %>% httr::content()
} else if (request$status_code == 401) {
  spotify_auth()
}
