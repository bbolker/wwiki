## FIXME: going crazy trying to figure out the _right_ way to store
##  cookies in the package environment.
## For now, just leave them in options(); change get/setMWCookie()
##  if/when I figure out how

## JUNK:
## when properly installed (not just loaded via devtools) I get
## Error in assign("cookie", cookie, pos = "package:wwiki") : 
##   cannot add bindings to a locked environment
## if (!exists("cookie",where="package:wwiki") ||
## is.null(cookie <- get("cookie",pos="package:wwiki")))

## .onLoad <- function(libname,pkgname) {
##     message("in onLoad")
##     assign("cookie",NULL,parent.frame())
## }

getMWCookie <- function() { getOption("MWcookie") }
setMWCookie <- function(cookie) options(MWcookie=cookie)

##' Log in to a MediaWiki/WorkingWiki server
##' @title Log in to a wiki
##' @param username (character) user name for wiki; if not specified, the function will try to retrieve it via \code{getOption("MWUser")}
##' @param password (character) wiki password; will be prompted for if not specified
##' @param api.url (character) URL for connecting to the wiki
##' @param api.opts options for connecting: list including elements \code{verbose} (logical), \code{useragent} (character), \code{cookie} (?)
##' @param setCookie (logical) should the function cache a cookie in the search path after successful login?
##' @param verbose (logical) verbose output?
##' @return a cookie allowing further MW actions
##' @keywords misc
##' @export
##' @importFrom rjson fromJSON
##' @importFrom RCurl postForm

loginWiki <- function(username=NULL, password=NULL, api.url, api.opts, setCookie=TRUE,
                      verbose=FALSE)
{
    ## FIXME: need a closeConnection() somewhere?
    ## FIXME: allow fail.option (i.e. return NULL cookie or fail here)?
    if (is.null(username) && is.null(username <- getOption("MWUser"))) {
        stop("Must specify a user name to log in to the wiki")
    }
    if (is.null(password)) password <- getPassword()
    login.postparams <- list(
                             action = 'login',
                             lgname = username,
                             lgpassword = password,
                             format = 'json' )
 
    ## try to log in to the wiki.  This will return a token and then I'll have
    ## to log in for real using the token.
    login.result <- postForm( api.url,
                             .params = login.postparams,
                             .opts = api.opts,
                             style = 'post' )
 
    login.data <- fromJSON(login.result)
    prefix <- login.data[['login']][['cookieprefix']]
    cookie <- ''

    if (login.data[['login']][['result']] == 'NeedToken')
    { # add the token and a cookie to the POST fields, and then log in
        token <- login.data[['login']][['token']]
        login.postparams[['lgtoken']] <- token
        ## RCurl should handle the cookies automatically, api.php returns suitable
        ## Set-Cookie: headers, but I've tried to use cookiefile and cookiejar and
        ## had no success.  I must be doing something wrong.  But I get this to 
        ## work by setting the cookies myself.
        cookie <- paste0(prefix, '_session=', 
                         login.data[['login']][['sessionid']])
        api.opts[['cookie']] <- cookie
        login.result <- postForm( api.url,
                                 .params = login.postparams,
                                 .opts = api.opts,
                                 style = 'post' )
        ## should be successful this time.
        login.data <- fromJSON(login.result)
        if (login.data[['login']][['result']] != 'Success') {
            with(login.data[['login']],
                 stop(paste("Error logging in to wiki:", result)))
        }
    }
    ## FIXME: repeated code, should be encapsulated
    if (login.data[['login']][['result']] != 'Success')  {
        with(login.data[['login']],
             stop(paste("Error logging in to wiki:", result)))
    }
    ## we're supposed to use these cookies as well once we're logged in
    if (cookie != '') { cookie <- paste0(cookie, ';') }
    cookie <- paste0(cookie, 
                     prefix, 'UserName=',
                     login.data[['login']][['lgusername']],';',
                     prefix, 'UserID=', login.data[['login']][['lguserid']],';',
                     prefix, 'Token=', login.data[['login']][['lgtoken']])

    if (setCookie) {
        setMWCookie(cookie)
        ## assignInNamespace("cookie", cookie, ns="wwiki", pos = "package:wwiki")
        ## assign("cookie",cookie,pos="package:wwiki")
        if (verbose) cat("set cookie\n")
    }
    return(cookie)
}

## from Barry Rowlingson
## https://stat.ethz.ch/pipermail/r-help/2008-August/170662.html
##  FIXME: Is there a good fallback for getting the password (e.g. from the console) without
##    printing? Is tcltk OK with RStudio?

##' @importFrom tcltk tktoplevel tclVar tklabel tkentry tkbind tkdestroy tkbutton tkpack tkwait.window tclvalue tktitle<-
getPassword <- function() {
    tt <- tktoplevel()
    pass <- tclVar("")
    label.widget <- tklabel(tt, text="Enter password")
    tktitle(tt) <- ""  ## no title really necessary
    password.widget <- tkentry(tt,show="*",textvariable=pass)
    ## quit on Return ...
    tkbind(password.widget,"<Return>",function(){tkdestroy(tt)})
    ## or on button-press
    ok <- tkbutton(tt,text="OK",default="active",
                        command=function() { tkdestroy(tt) })
    tkpack(label.widget, password.widget,ok)
    tkwait.window(tt)
    return(tclvalue(pass))
}

