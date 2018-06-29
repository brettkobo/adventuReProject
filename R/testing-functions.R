source("R/api.R")
key <- gsub("\n", "", readChar("personal-api-key.txt", file.info("personal-api-key.txt")$size))


project_name <- "trailrun"

temp_data <- get_trails(project = project_name,
                        key = key, lat = 31.23242,
                        lon = -118.23421,
                        max_distance = 500,
                        max_results = 200)

trail_by_id <- get_trails_by_id(project = project_name,
                                key = key,
                                ids = temp_data$id[1:10])

conditions <- get_condtions(project = project_name,
                            key = key,
                            ids = temp_data$id[1:50])

conditions[!grepl("success", names(conditions))] %>%
  lapply(function(x) ifelse(x == "NULL", NA, x)) %>%
  bind_rows()


user_todos <- get_to_dos(project = project_name,
                         key = key,
                         email = "brettkob@gmail.com")


user <- get_user(project = "mountain",
                         key = key,
                         email = "brettkob@gmail.com")

user <- get_user(project = "mountain",
                 key = key,
                 user_id = "105885115")

ticks <- get_to_dos(project = "mountain",
                    key = key,
                    user_id = "105885115")


routes_lat_lon <- get_routes_for_lat_lon(project = "mountain",
                                         key = key,
                                         lat = 31.23242,
                                         lon = -118.23421,
                                         max_distance = 500,
                                         max_results = 200)


routes_by_id <- get_routes(project = "mountain",
                           key = key,
                           route_ids = c("106008982"))
