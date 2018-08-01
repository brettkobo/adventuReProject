# ---- Mountian Project API ----


#' User information by user ID or email.
#'
#' Retrives general user information. Currently only lives under the Mountian Project but returns information about the user from all of hte projects.
#'
#' @inheritParams get_condtions
#' @param user_id the ID of the user to return
#' @param email the email address of the user to return
#'
#' @details Either user_id or email is required, but not both.
#' @return A data.frame of a users to-dos.
#'
#' @examples
#' \dontrun{
#' user <- get_user(project = project_name, key = key, email = "adventure.bro@gmail.com")
#' }
#'
#' @export
get_user <- function(key = NULL, user_id = NULL, email = NULL, start_pos = NULL) {

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

#'Returns ticks by user ID or email.
#'
#' Retrives top 200 ticks for a user. Currently missing the "hardest" and "average" data point.
#'
#' @inheritParams get_condtions
#' @param user_id the ID of the user to return
#' @param email the email address of the user to return
#'
#' @details Either user_id or email is required, but not both.
#' @return A data.frame of a users to-dos.
#'
#' @examples
#' \dontrun{
#' ticks <- get_ticks(project = project_name, key = key, email = "adventure.bro@gmail.com")
#' }
#'
#' @export
get_ticks <- function(project = NULL, key = NULL, user_id = NULL, email = NULL, start_pos = NULL) {
  #missing hardest & average climb level - currently only grabing ticks
  project_match_mountian(project)
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

#' Get routes for specific Adventure Project based on IDs
#'
#' Retrieves a JSON of routes based on trail IDs.
#'
#' @param project which project you would like to retrieve data from; must be "mountian".
#' @param key your private key, sign up on adventureproject.com site for one (required)
#' @param route_ids vector of route IDs (required)
#'
#' @return A data.frame containing all of the trails that match the query parameters.  Data.frame will have URL used when calling API in the attributes. Can be retrieved with attr(data, "api_url").
#'
#' @examples
#' \dontrun{
#' routes <- get_routes(project = "mountian", key = privateKey, route_ids = c("1829020", "1728301"))
#' }
#'
#' @export
get_routes <- function(project = NULL, key = NULL, route_ids = NULL) {

  project_match_mountian(project)
  base_url <- paste0("https://www.", project, "project.com")

  required_params <- list(key = key,
                          routeIds = paste0(route_ids, collapse = ","))

  url <- modify_url(base_url,
                    path = "/data/get-routes",
                    query = required_params)
  data <- jsonlite::fromJSON(txt = url)$routes %>% data.frame()
  return(data)
}


#' Get the routes for the Mountian Project by longitude and latitude.
#'
#' Retrieves a JSON of routes based on the the longitude and latitude.
#'
#' @param project which project you would like to retrieve data from; must be "mountian".
#' @param key your private key, sign up on adventureproject.com site for one (required)
#' @param lat latitude for a given area (required)
#' @param lon longitude for given area (required)
#' @param max_distance max distance, in miles, from lat, lon. Default: 30. Max: 200.
#' @param max_results max number of trails to return. Default: 10. Max: 500.
#' @param min_diff min difficulty of routes to return, e.g. 5.6 or V0.
#' @param max_diff max difficulty of routes to return, e.g. 5.10a or V2.
#'
#' @return A data.frame containing all of the routes that match the query parameters. data.frame will have URL used when calling API in the attributes. Can be retrieved with attr(data, "api_url").
#'
#' @examples
#' \dontrun{
#' routes_by_gps <- get_routes_for_lat_lon(project = "mtb", key = privateKey, lat = 31.23242, lon = -118.23421, max_distance = 500, max_results = 175)
#' }
#'
#' @export
get_routes_for_lat_lon <- function(project = NULL, key = NULL, lat = NULL, lon = NULL, max_distance = NULL, max_results = NULL, min_diff = NULL, max_diff = NULL) {

  project_match_mountian(project)

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
