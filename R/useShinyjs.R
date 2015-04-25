#' Set up a Shiny app to use shinyjs
#'
#' This function must be called from a Shiny app's UI in order for all other
#' \code{shinyjs} functions to work.
#'
#' @return Scripts that \code{shinyjs} requires that are automatically inserted
#' to the app's \code{<head>} tag.
#' @note When initializing the Shiny app's server, you must supply the
#' \code{session} parameter to the server function, ie. initialize the server
#' as \code{server = function(input, output, session)} rather than
#' \code{server = function(input, output)}
#' @examples
#' if (interactive()) {
#'   shiny::shinyApp(
#'     ui = shiny::fluidPage(
#'       useShinyjs(),  # Set up shinyjs
#'       shiny::actionButton("btn", "Click me"),
#'       shiny::p(id = "element", "Watch what happens to me")
#'     ),
#'     server = function(input, output, session) {
#'       shiny::observe({
#'         if (input$btn == 0) {
#'           return(NULL)
#'         }
#'         # Run a simply shinyjs function
#'         toggle("element")
#'       })
#'     }
#'   )
#' }
#' @seealso \code{\link[shinyjs]{runExample}}
#' @export
useShinyjs <- function() {
  # all the methods that should be forwarded to javascript
  jsFuncs <- c("show", "hide", "toggle", "enable", "disable", "toggleState",
               "addClass", "removeClass", "toggleClass", "text",
               "onclick", "info", "logjs")

  # grab the file with all the message handlers (javascript functions)
  handler <- system.file("srcjs", "shinyjs-message-handler.js",
                         package = "shinyjs")
  if (handler == "") {
    errMsg("could not find shinyjs message handler file")
  }

  # set up the message handlers for all functions
  # and add custom CSS for hiding elements
  setupJS(handler, jsFuncs, inlineCSS(".shinyjs-hide { display: none; }"))
}

#' Extend shinyjs with your own functions
#'
#' Add your own JavaScript functions that can be called from R as if they were
#' regular R functions. This is a more advanced technique and can only
#' be used if you know JavaScript. See 'Basic Usage' below for more information on
#' how to use this.
#'
#' @section Basic Usage:
#' If the default functions that are provided by \code{shinyjs} are not enough,
#' you can add your own JavaScript functions. As a basic example, to add a
#' JavaScript function that shows a message to the user (similar to
#' \code{\link[shinyjs]{info}}), follow these steps:
#'
#' \itemize{
#'   \item Create a JavaScript file inside the Shiny app's directory
#'
#'   \code{www/js/shinyjs-ext.js}
#'   \item Add the following JavaScript function to the file:
#'
#'     \code{shinyjs.myfunc = function(params) { alert(params); }}
#'   \item In your Shiny app's UI, add a call to
#'
#'     \code{shinyjs::extendShinyjs("www/js/shinyjs-ext.js", "myfunc")}
#'   \item Now in your Shiny app's server you can call
#'     \code{myfunc("Hello!")} and you will get a JavaScript message
#' }
#'
#' You can add more functions to the JavaScript file, but every function you
#' want to use within R has to have a name beginning with \code{shinyjs.}. When
#' calling \code{extendShinyjs()}, all function names of the functions you want
#' to use need to be given (without the leading \code{shinyjs.}, as in the
#' example above).
#'
#' @section Passing arguments from R to JavaScript:
#' \code{shinyjs} will pass a single parameter to your JavaScript function.
#' If the function in R was called without named arguments, then it will pass
#' an \code{Array} of the arguments, and if the R arguments are named then
#' it will pass an Object with key-value pairs.
#'
#' To assist with normalizing the parameters, you can call the
#' \code{shinyjs.getParams(params, defaults)} function, where \code{params}
#' are the parameters that are passed to your JavaScript function and
#' \code{defaults} is an object with key-value pairs of parameters, where
#' each key is a parameter name and each value is a default value. The order
#' of the parameters in this \code{defaults} object should match the order
#' of the parameters that users should use if they choose not to use
#' named arguments.
#'
#' For example, if a JavaScript function expects an id parameter and a length
#' parameter, this could be the first few lines of the function:
#'
#' \code{
#'   shinyjs.objlength(params) { \cr
#'     var defaultParams = { id : null, length : 5 }; \cr
#'     params = shinyjs.getParams(params, defaultParams); \cr
#'     // function body \cr}
#' }
#'
#' @param script Path to a JavaScript file that contains all the functions.
#' Each function name must begin with \code{shinyjs.}, for example
#' \code{shinyjs.myfunc}. See 'Basic Usage' below.
#' @param functions Vector of funtions to be made available by \code{shinyjs}.
#' See 'Basic Usage' below.
#' @return Scripts that \code{shinyjs} requires in order to run your JavaScript
#' functions as if they were R code.
#' @note You still need to call \code{useShinyjs()} as usual.
#' @export
extendShinyjs <- function(script, functions) {
  # add all the given functions to the shinyjs namespace so that they can be
  # called as if they were regular shinyjs functions
  lapply(functions, function(x) {
    #assign(x, jsFunc, shinyjsExtFuncs) # need to attach shinyjsExtFuncs
    #assign(x, jsFunc, as.environment("package:shinyjs"))
    #myfuncs[[x]] <- jsFunc
    #assign("myfuncs", myfuncs, as.environment("package:shinyjs"))
    assign(x, jsFunc, js)
  })
  # set up the message handlers for all functions
  setupJS(script, functions)
}

setupJS <- function(script, functions, ...) {
  # add a shiny message handler binding for each supported method
  tpl <- paste0(
    "Shiny.addCustomMessageHandler('%s', function(params) {",
    " shinyjs.%s(params);",
    "});")
  controllers <-
    lapply(functions, function(x) {
      sprintf(tpl, x, x)})
  controllers <- paste(controllers, collapse = "\n")

  shiny::tags$head(
    # add the message handler bindings
    shiny::tags$script(shiny::HTML(controllers)),
    # add the message handlers
    shiny::includeScript(script),
    ...
  )
}

