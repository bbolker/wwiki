\name{loginWiki}
\alias{loginWiki}
\title{Log in to a wiki}
\usage{
  loginWiki(username = NULL, password = NULL, api.url,
    api.opts, setCookie = TRUE, verbose = FALSE)
}
\arguments{
  \item{username}{(character) user name for wiki; if not
  specified, the function will try to retrieve it via
  \code{getOption("MWUser")}}

  \item{password}{(character) wiki password; will be
  prompted for if not specified}

  \item{api.url}{(character) URL for connecting to the
  wiki}

  \item{api.opts}{options for connecting: list including
  elements \code{verbose} (logical), \code{useragent}
  (character), \code{cookie} (?)}

  \item{setCookie}{(logical) should the function cache a
  cookie in the search path after successful login?}

  \item{verbose}{(logical) verbose output?}
}
\value{
  a cookie allowing further MW actions
}
\description{
  Log in to a MediaWiki/WorkingWiki server
}
\keyword{misc}

