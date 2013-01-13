\name{pushWiki}
\alias{pushWiki}
\title{Push a source file to a Working Wiki}
\usage{
  pushWiki(file, page, project = page, display = NULL,
    filename = basename(file), readFile = TRUE,
    wiki = "bio_708",
    wikibase = "http://lalashan.mcmaster.ca/theobio/",
    autodepends = TRUE, verbose = FALSE, ...)
}
\arguments{
  \item{file}{Path to file for upload}

  \item{project}{?}

  \item{page}{?}

  \item{display}{[STUB] (character) display tag for file on
  WW: e.g. "source", "none", "link", ... ??}

  \item{filename}{Name of file to assign on WW}

  \item{readFile}{(logical) extract text to upload from a
  file?}

  \item{wiki}{WW name: either relative to \code{wikibase},
  or a full URL}

  \item{wikibase}{base URL for WW}

  \item{autodepends}{(logical) automatically detect and
  upload dependencies?}

  \item{verbose}{verbose output?}

  \item{\dots}{additional arguments (esp. username) to pass
  to \code{\link{loginWiki}}}
}
\value{
  name(s) of uploaded file(s)
}
\description{
  Push a source file to a Working Wiki
}
\details{
  \itemize{ \item In general a user name and password are
  required. If not specified, the user name will be
  retrieved via \code{\link{getOption}("MWUser")} [for
  "MediaWiki user"] if possible; see example below for
  setting the option.  Cookies will be saved (at present
  via \code{\link{options}} so that passwords need be
  entered only once per R session. (If saving unencrypted
  cookies in memory is a concern, please don't use this
  package.) \item Pushing the same file twice will
  overwrite the original version. \item If
  \code{autodepends} is \code{TRUE}, the function will try
  to detect dependencies (data files etc.) in the code and
  upload them automatically to the same page.  This will
  only work in simple cases (filename must not be specified
  explicitly; all files must be in the working directory;
  each load command must be specified on a separate line;
  etc.). The \code{autodepends} options currently detects
  \code{\link{read.table}}, \code{\link{read.csv}},
  \code{\link{load}}, and \code{\link{source}}. \item If
  \code{display} is not explicitly set, the function will
  try to guess an appropriate default.  The current
  behavior is to display R Markdown (\code{rmd}) as HTML;
  LaTeX and Sweave (\code{tex}, \code{Rnw}) as PDF; data
  files (RData, rda, CSV, txt, dat, tab) as links; and
  everything else as source. Other Working Wiki options are
  ... ? }
}
\examples{
\dontrun{
pushWiki("pulse.rmd",user="Bb",page="pushTest")
## set persistent user name (works until you start a new R session;
## you could set it in your \\code{\\link{Rprofile}})
options(MWUser="Bb")
## now username is unnecessary
pushWiki("pulse.rmd",
          page="pushTest")
}
}
\keyword{misc}

