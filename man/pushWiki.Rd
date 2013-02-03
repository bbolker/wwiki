\name{pushWiki}
\alias{pushWiki}
\title{Push a source file to a Working Wiki}
\usage{
  pushWiki(file, page, project = page, display = NULL,
    filename = basename(file), readFile = TRUE,
    wiki = "bio_708_2013",
    wikibase = "http://lalashan.mcmaster.ca/theobio/",
    autodepends = TRUE, autoopen = autodepends,
    autoRsave = TRUE, verbose = FALSE, ...)
}
\arguments{
  \item{file}{path to file for upload}

  \item{page}{wiki page to which to push the file(s)}

  \item{project}{wiki project to use (default is the same
  as the page)}

  \item{display}{(character) display tag for the file on
  WW: e.g. "myfile.html" if you want an HTML version of
  your file to be displayed.  Other typical choisces are
  "source", "none", "link". [FIXME: WW reference on display
  options?]}

  \item{filename}{Name of file to assign on WW}

  \item{readFile}{(logical) extract text to upload from a
  file?}

  \item{wiki}{WW name: either relative to \code{wikibase},
  or a full URL}

  \item{wikibase}{base URL for WW}

  \item{autodepends}{(logical) automatically detect and
  upload dependencies?}

  \item{autoopen}{(logical) automatically open modified
  page in a web browser?}

  \item{autoRsave}{(logical) add a \code{save.image()}
  command to the end of R markdown, Sweave, and R files?}

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

