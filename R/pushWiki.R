## FIXME: should there be any sort of confirmation/warning when overwriting?
## FIXME: more granularity for verbosity? (e.g. TRUE -> 1 provides base level, numeric values >1 give access to loginWiki verbosity?)
## FIXME: cache user, wiki name, etc. in options?
## FIXME: more on display options
## FIXME: additional autodepends targets (read.fwf, read.xls, ... ?) user-extendable list?
## FIXME: should there be more flexibility in whether to push a Makefile or not?
## FIXME: fix makefile rules

##' Push a source file to a Working Wiki
##' @param file path to file for upload
##' @param page wiki page to which to push the file(s)
##' @param project wiki project to use (default is the same as the page)
##' @param display (character) display tag for the file on WW: e.g. "myfile.html" if you want an HTML version of your file to be displayed.  Other typical choisces are "source", "none", "link". [FIXME: WW reference on display options?]
##' @param filename Name of file to assign on WW
##' @param readFile (logical) extract text to upload from a file?
##' @param wiki WW name: either relative to \code{wikibase}, or a full URL
##' @param wikibase base URL for WW
##' @param autodepends (logical) automatically detect and upload dependencies?
##' @param autoopen (logical) automatically open modified page in a web browser?
##' @param verbose verbose output?
##' @param \dots additional arguments (esp. username) to pass to \code{\link{loginWiki}}
##' @return name(s) of uploaded file(s)
##' @details
##' \itemize{
##' \item In general a user name and password are required. If not specified, the user name will be retrieved via \code{\link{getOption}("MWUser")} [for "MediaWiki user"] if possible; see example below for setting the option.  Cookies will be saved (at present via \code{\link{options}} so that passwords need be entered only once per R session. (If saving unencrypted cookies in memory is a concern, please don't use this package.)
##' \item Pushing the same file twice will overwrite the original version.
##' \item If \code{autodepends} is \code{TRUE}, the function will try to detect dependencies (data files etc.) in the code and upload them automatically to the same page.  This will only work in simple cases (filename must not be specified explicitly; all files must be in the working directory; each load command must be specified on a separate line; etc.). The \code{autodepends} options currently detects \code{\link{read.table}}, \code{\link{read.csv}}, \code{\link{load}}, and \code{\link{source}}.
##' \item If \code{display} is not explicitly set, the function will try to guess an appropriate default.  The current behavior is to display R Markdown (\code{rmd}) as HTML; LaTeX and Sweave (\code{tex}, \code{Rnw}) as PDF; data files (RData, rda, CSV, txt, dat, tab) as links; and everything else as source. Other Working Wiki options are ... ?
##' }
##' @keywords misc
##' @examples
##' \dontrun{
##' pushWiki("pulse.rmd",user="Bb",page="pushTest")
##' ## set persistent user name (works until you start a new R session;
##' ## you could set it in your \code{\link{Rprofile}})
##' options(MWUser="Bb")
##' ## now username is unnecessary
##' pushWiki("pulse.rmd",
##'           page="pushTest")
##' }
##' @export
##' @importFrom rjson fromJSON
##' @importFrom RCurl postForm
##' @importFrom tools file_ext file_path_sans_ext
pushWiki <- function(file,
                     ## FIXME: what is the distinction
                     ##        between project and page?
                     page,
                     project=page,
                     display=NULL,
                     filename=basename(file),
                     readFile=TRUE,
                     ## FIXME: change wiki/wikibase defaults to blank for more general use?
                     wiki="bio_708_2013",
                     wikibase="http://lalashan.mcmaster.ca/theobio/",
                     autodepends=TRUE,
                     autoopen=autodepends,
                     autoRsave=TRUE,
                     verbose=FALSE,
                     ...
                     ) {

    if (missing(wiki) && !is.null(ww <- getOption("wiki"))) wiki <- ww
    if (missing(wikibase) && !is.null(ww <- getOption("wikibase"))) wikibase <- ww   
    if (!readFile && missing(filename)) {
        stop("must specify filename explicitly if readFile is FALSE")
    }

    ## now we're logged in, do the actual import

    ## construct result URL before autodepends
    if (!grepl("http://",wiki)) {
        wiki <- paste0(wikibase,wiki)
    }
    pushedURL <- paste(wiki,"index.php",page,sep="/")

    ## display rules. rownames=file extension;
    ##                page_prefix: prefix for page -- stick in file space?
    ##                display=display attribute of source tag
    ##                ext=does the display type represent a file extension,
    ##                    i.e. display="foo.<display>" rather than display="<display>"
    ## 
    drules <- data.frame(img_prefix=rep(c(FALSE,TRUE),c(7,6)),
                         display=c("html","pdf","source","html",
                             "pdf","source","link",
                              rep("link",6)),
                         rsave=rep(c(TRUE,FALSE),c(3,10)),
                         row.names=c("rmd","rnw","r","md","tex","html","mk",
                         "rdata","rda","csv","txt","dat","tab"),
                         stringsAsFactors=FALSE)
    drules <- transform(drules,ext=display %in% c("html","pdf"))
    ## process display defaults before autodepends;
    ## we will want the result for the makefile
    if (is.null(display)) {
        if (!readFile) {
            display <- "source"
        } else {
            fileExt <- tolower(file_ext(file))
            fileBase <- file_path_sans_ext(filename)
            displayType <- drules[fileExt,"display"]
            pushTag <- drules[fileExt,"tag"]
            displayExt <- drules[fileExt,"ext"]
            display <- if (displayExt) {
                paste(fileBase,displayType,sep=".")
            } else if (!is.na(displayType)) {
                displayType
            }
        }
    }

    ## get file contents
    if (readFile) {
        raw.contents <- readLines(file, warn=FALSE)
    } else {
        if (autodepends) stop("cannot use readFile=FALSE with autodepends")
        raw.contents <- file
        file <- "<contents>"
    }
    ## convert to newline-separated string
    file.contents <- paste(raw.contents , collapse="\n")
    if (autoRsave && drules[fileExt,"rsave"]) {
        saveString <- paste0("save.image(file=\"",fileBase,".RData\")")
        if (fileExt=="rmd") saveString <- paste(c("```{r __autosave,echo=FALSE}",saveString,"```"),collapse="\n")
        if (fileExt=="rnw") saveString <- paste(c("<<__autosave,echo=FALSE>>",saveString,"@"),collapse="\n")
        file.contents <- paste(file.contents,saveString,sep="\n")
    }
    ## add final blank line
    file.contents <- paste0(file.contents,"\n")

    if (autodepends) {
        ## if "autodepends" is on, try to get
        ## may fail on tricky cases like:
        ##   * targets embedded in comments
        ##   * filenames passed in variables
        ##   * multiple commands on the same line of code
        ##   * readFile=FALSE
        ##   * filename specified explicitly
        ##   * all files not in same directory
        ##   * ?
        mCall <- match.call()
        read_targets <- c("read.csv","read.table","source","load")
        ## could use stringr::str_extract but would rather not
        ##  multiply dependencies if I can help it
        ## target string:
        ##    start-of-line + generic stuff +
        ##       |-separated list of function targets +
        ##       open-paren + open-quote + FILENAME TARGET +
        ##       close-quote + non-paren stuff + close-paren +
        ##       generic stuff + EOL
        tStr <- paste0("^.*(",paste(read_targets,collapse="|"),")",
                       "\\(['\"]([^'\"]+)['\"][^)]*\\).*$")
        tLines <- grep(tStr,raw.contents,value=TRUE)
        tVals <- gsub(tStr,"\\2",tLines)  ## extract FILENAME TARGET
        tVals <- tVals[nzchar(tVals)] ## discard empties
        tVals <- c(file,tVals)
        if (length(tVals)>1) {
            ## construct Makefile
            ## dependency line
            mktext <- paste(paste0(display,":"),
                            paste(tVals,collapse=" "))
            ## rule line
            ## FIXME: re-enable this once debugged
            ## makeRule <- switch(displayExt,
            ##                  tex=stop("no tex make rule yet"),
            ##                   rmd="$(knit_html)",
            ##                   rnw="$(knit_pdf)")
            ## mktext <- c(mktext,paste0("	",makeRule))
            makeFn <- paste(display,"mk",sep=".")
            writeLines(mktext,con=makeFn)
            tVals <- c(tVals,makeFn)
        }
        s <- sapply(tVals,
                    function(x) {
                        if (verbose) cat("running autodepends:",x,"\n")
                        tmpCall <- mCall
                        tmpCall[["file"]] <- x
                        tmpCall[["autodepends"]] <- FALSE
                        eval(tmpCall)
                    })
        if (autoopen) browseURL(pushedURL)
        return(unname(s))
    } ## if autodepends

    if (drules[fileExt,"img_prefix"]) {
        page <- paste0("Image:",project,"$",file)
    }
    
    api.url <- paste(wiki,"api.php",sep="/")
    api.opts <- list( verbose = verbose,
                     useragent = 'R wwiki package');

    ## log in to the wiki 
    ## FIXME: should cookie-checking be handled in loginWiki?

    if (is.null(cookie <- getMWCookie())) {
        cookie <- loginWiki( api.url=api.url, api.opts=api.opts, setCookie=TRUE, ... )
    } else {
        if (verbose) cat("found existing cookie\n")
    }

    if (is.null(cookie)) {
        ## this probably shouldn't happen, we should fail earlier
        stop("couldn't log in to wiki")
    } else { 
        api.opts[['cookie']] <- cookie
    }

    pList <- list(action = 'ww-import-project-files',
                  project = project,
                  filename = filename,
                  page = page,
                  'as-source-file' = 1,
                  'file-contents' = file.contents,
                  'tag-attributes' = paste('display',display,sep="="),
                  format = 'json' )

    import.result <- postForm( api.url,
                              .params = pList,
                              .opts = api.opts,
                              style = 'post' )


    import.result <- fromJSON(import.result)

    ## if MW fails to call the WW api function, it returns 'error'
    if ( ! is.null(import.result[['error']]) ) {
        stop("Error:", import.result[['error']][['info']])
    }
    if (verbose) cat("Success.\n")
    if (autoopen) browseURL(pushedURL)
    return(unname(file))
}

