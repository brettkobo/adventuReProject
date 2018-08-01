# ---- Common Error Handeling Functions ----
blocked_user <- function(resp) {
  if(resp$status_code == 403) {
    stop("Blocked User. Most likly your API key is wrong. Also check if you need to update your API key. Retrieve it from the {project-name}project.com/data page.",
         call. = FALSE)
  }
}

project_match_trails <- function(project_name) {
  if(!project_name %in% c("mtb", "hiking", "trailrun", "powder")) {
    stop("Incorrect project name. Try using mtb, hiking, trailrun, or powder")
  }
}

project_match_mountian <- function(project_name) {
  if(!project_name %in% c("mountain")) {
    stop("Incorrect project name. Try mountain")
  }
}
