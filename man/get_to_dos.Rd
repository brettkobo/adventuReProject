% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trails_api.R
\name{get_to_dos}
\alias{get_to_dos}
\title{Get the to-dos from user by user ID or email.}
\usage{
get_to_dos(project = NULL, key = NULL, user_id = NULL,
  email = NULL, start_pos = NULL)
}
\arguments{
\item{project}{which project you would like to retrieve data from; must be one of the following c("mtb", "hiking", "trailrun", "powder")}

\item{key}{your private key, sign up on adventureproject.com site for one (required)}

\item{user_id}{the ID of the user to return}

\item{email}{the email address of the user to return}

\item{start_pos}{the starting index of the list to return. Defaults to 0.}
}
\value{
A data.frame of a users to-dos.
}
\description{
Retrives list of to-dos from a specific user.
}
\details{
Either user_id or email is required, but not both.
}
\examples{
\dontrun{
user_todos <- get_to_dos(project = project_name, key = key, email = "adventure.bro@gmail.com")
}

}
