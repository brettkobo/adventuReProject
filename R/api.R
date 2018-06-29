library(httr)
library(jsonlite)
library(tidyverse)

# ---- MTB | Hiking | Trail Running | Powder API ----

get_trails <- function(project = NULL, key = NULL, lat = NULL, lon = NULL, max_distance = NULL, max_results = NULL, sort = NULL, min_length = NULL, min_stars = NULL) {

  base_url <- paste0("https://www.", project, "project.com")

  required_params <- list(key = key,
                            lat = lat,
                            lon = lon)
  optional_params <- list(
      maxDistance = max_distance,
      maxResults = max_results,
      sort = sort,
      minLength = min_length,
      minStars = min_stars)


  url <- modify_url(base_url,
    path = "/data/get-trails",
    query = c(required_params, optional_params))

  data <- jsonlite::fromJSON(txt = url)$trails %>% data.frame()

  return(data)
}

get_trails_by_id <- function(project = NULL, key = NULL, ids = NULL) {
  base_url <- paste0("https://www.", project, "project.com")

  required_params <- list(key = key,
                        ids = paste0(ids, collapse = ","))

  url <- modify_url(base_url,
                    path = "/data/get-trails-by-id",
                    query = required_params)
  data <- jsonlite::fromJSON(txt = url)  %>% data.frame() %>% select(-success)
  return(data)
}


#maxs out at 50 conditions
get_condtions <- function(project = NULL, key = NULL, ids = NULL) {
  base_url <- paste0("https://www.", project, "project.com")

  required_params <- list(key = key,
                          ids = paste0(ids, collapse = ","))

  url <- modify_url(base_url,
                    path = "/data/get-conditions",
                    query = required_params)
  data <- jsonlite::fromJSON(txt = url)
  data_mod <- data[!grepl("success", names(data))] %>%
    lapply(function(x) ifelse(x == "NULL", NA, x)) %>%
    bind_rows()
  return(url)
}


get_to_dos <- function(project = NULL, key = NULL, user_id = NULL, email = NULL, start_pos = NULL) {
  base_url <- paste0("https://www.", project, "project.com")

  required_params <- if(is.null(user_id))
    { list(key = key, email = email) }
  else
    { list(key = key, userId = user_id) }

  optional_params <- list(startPos = start_pos)

  url <- modify_url(base_url,
                    path = "/data/get-to-dos",
                    query = c(required_params, optional_params))
  data <- jsonlite::fromJSON(txt = url) %>% data.frame() %>% select(-success)
  return(data)
}

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


#different API
# https://www.mountainproject.com/data - rock climbing


#all the same API
# https://www.mtbproject.com/data - mountian biking
# https://www.hikingproject.com/data - hiking
# https://www.trailrunproject.com/data - trail running
# https://www.powderproject.com/data - ski ans snowboard
