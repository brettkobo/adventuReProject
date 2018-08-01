# ---- Mountian Project API ----

get_user <- function(project = NULL, key = NULL, user_id = NULL, email = NULL, start_pos = NULL) {
  base_url <- paste0("https://www.", project, "project.com")

  required_params <- if(is.null(user_id))
  { list(key = key, email = email) }
  else
  { list(key = key, userId = user_id) }

  optional_params <- list(startPos = start_pos)

  url <- modify_url(base_url,
                    path = "/data/get-user",
                    query = c(required_params, optional_params))
  data <- jsonlite::fromJSON(txt = url) %>% data.frame() %>% select(-success)
  return(data)
}

#missing hardest & average climb level - currently only grabing ticks
get_ticks <- function(project = NULL, key = NULL, user_id = NULL, email = NULL, start_pos = NULL) {
  base_url <- paste0("https://www.", project, "project.com")

  required_params <- if(is.null(user_id))
  { list(key = key, email = email) }
  else
  { list(key = key, userId = user_id) }

  optional_params <- list(startPos = start_pos)

  url <- modify_url(base_url,
                    path = "/data/get-ticks",
                    query = c(required_params, optional_params))
  data <- jsonlite::fromJSON(txt = url)$ticks
  return(data)
}

get_routes <- function(project = NULL, key = NULL, route_ids = NULL) {
  base_url <- paste0("https://www.", project, "project.com")

  required_params <- list(key = key,
                          routeIds = paste0(route_ids, collapse = ","))

  url <- modify_url(base_url,
                    path = "/data/get-routes",
                    query = required_params)
  data <- jsonlite::fromJSON(txt = url)$routes %>% data.frame()
  return(data)
}

get_routes_for_lat_lon <- function(project = NULL, key = NULL, lat = NULL, lon = NULL, max_distance = NULL, max_results = NULL, min_diff = NULL, max_diff = NULL) {

  base_url <- paste0("https://www.", project, "project.com")

  required_params <- list(key = key,
                          lat = lat,
                          lon = lon)
  optional_params <- list(
    maxDistance = max_distance,
    maxResults = max_results,
    minDiff = min_diff,
    maxDiff = max_diff)


  url <- modify_url(base_url,
                    path = "/data/get-routes-for-lat-lon",
                    query = c(required_params, optional_params))

  data <- jsonlite::fromJSON(txt = url)$routes %>% data.frame()
  return(data)
}
