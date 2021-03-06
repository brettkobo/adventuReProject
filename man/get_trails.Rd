% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trails_api.R
\name{get_trails}
\alias{get_trails}
\title{Get the trails for specific Adventure Project}
\usage{
get_trails(project = NULL, key = NULL, lat = NULL, lon = NULL,
  max_distance = NULL, max_results = NULL, sort = NULL,
  min_length = NULL, min_stars = NULL)
}
\arguments{
\item{project}{which project you would like to retrieve data from; must be one of the following c("mtb", "hiking", "trailrun", "powder")}

\item{key}{your private key, sign up on adventureproject.com site for one (required)}

\item{lat}{latitude for a given area (required)}

\item{lon}{longitude for given area (required)}

\item{max_distance}{max distance, in miles, from lat, lon. Default: 30. Max: 200.}

\item{max_results}{max number of trails to return. Default: 10. Max: 500.}

\item{sort}{values can be 'quality', 'distance'. Default: quality.}

\item{min_length}{min trail length, in miles. Default: 0 (no minimum).}

\item{min_stars}{min star rating, 0-4. Default: 0.}
}
\value{
A data.frame containing all of the trails that match the query parameters. data.frame will have URL used when calling API in the attributes. Can be retrieved with attr(data, "api_url").
}
\description{
Retrieves a JSON of trails based on the the longitude and latitude.
}
\examples{
\dontrun{
trail_data <- get_trails(project = "mtb", key = privateKey, lat = 31.23242, lon = -118.23421, max_distance = 500, max_results = 175)
}

}
