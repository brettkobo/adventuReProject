library(httr)
library(jsonlite)
library(tidyverse)


# ---- MTB | Hiking | Trail Running | Powder ----

#' Get the trails for specific Adventure Project
#'
#' Retrieves a JSON of trails based on the the longitude and latitude.
#'
#' @param project which project you would like to retrieve data from; must be one of the following c("mtb", "hiking", "trailrun", "powder")
#' @param key your private key, sign up on adventureproject.com site for one (required)
#' @param lat latitude for a given area (required)
#' @param lon longitude for given area (required)
#' @param max_distance max distance, in miles, from lat, lon. Default: 30. Max: 200.
#' @param max_results max number of trails to return. Default: 10. Max: 500.
#' @param sort values can be 'quality', 'distance'. Default: quality.
#' @param min_length min trail length, in miles. Default: 0 (no minimum).
#' @param min_stars min star rating, 0-4. Default: 0.
#'
#' @return A data.frame containing all of the trails that match the query parameters. Data.frame will have URL used when calling API in the attributes. Can be retrieved with attr(data, "api_url").
#'
#' @examples
#' \dontrun{
#' trail_data <- get_trails(project = "mtb", key = privateKey, lat = 31.23242, lon = -118.23421, max_distance = 500, max_results = 175)
#' }
#'
#' @export
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

  get_data <- GET(url = url)

  blocked_user(get_data)
  data <- content(get_data, type = "text", encoding = "UTF-8") %>% jsonlite::fromJSON(txt = .)$trails %>% data.frame()
  attr(data, "api_url") <- url
  return(data)
}

#' Get trails for specific Adventure Project based on IDs
#'
#' Retrieves a JSON of trails based on trail IDs.
#'
#' @param project which project you would like to retrieve data from; must be one of the following c("mtb", "hiking", "trailrun", "powder")
#' @param key your private key, sign up on adventureproject.com site for one (required)
#' @param ids vector of trail IDs (required)
#'
#' @return A data.frame containing all of the trails that match the query parameters.  Data.frame will have URL used when calling API in the attributes. Can be retrieved with attr(data, "api_url").
#'
#' @examples
#' \dontrun{
#' trail_data <- get_trails_by_id(project = "mtb", key = privateKey, ids = c("1829020", "1728301"))
#' }
#'
#' @export
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

#' Get conditions for specific trails based on trail IDs
#'
#' Retrieves a JSON of the currect conditions
#'
#' @inherit get_trails_by_id return params
#'
#' @return A data.frame containing the trail conditions. Data.frame will have URL used when calling API in the attributes. Can be retrieved with attr(data, "api_url").
#'
#' @examples
#' \dontrun{
#' conditions <- get_condtions(project = project_name, key = key, ids = c("1829020", "1728301")
#' }
#'
#' @export
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


#' Get the to-dos from user by user ID or email.
#'
#' Retrives list of to-dos from a specific user.
#'
#' @inheritParams get_condtions
#' @param user_id the ID of the user to return
#' @param email the email address of the user to return
#'
#' @details Either \code{user_id} or \code{email} is required, but not both.
#' @return A data.frame of a users to-dos.
#'
#' @examples
#' \dontrun{
#' user_todos <- get_to_dos(project = project_name, key = key, email = "adventure.bro@gmail.com")
#' }
#'
#' @export
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


#different API
# https://www.mountainproject.com/data - rock climbing


#all the same API
# https://www.mtbproject.com/data - mountian biking
# https://www.hikingproject.com/data - hiking
# https://www.trailrunproject.com/data - trail running
# https://www.powderproject.com/data - ski ans snowboard


