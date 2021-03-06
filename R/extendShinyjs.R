#' Extend shinyjs by calling your own JavaScript functions
#'
#' Add your own JavaScript functions that can be called from R as if they were
#' regular R functions. This is a more advanced technique and can only
#' be used if you know JavaScript. See 'Basic Usage' below for more information
#' or \href{https://github.com/daattali/shinyjs}{view the full README and demos}
#' to learn more.
#'
#' @section Basic Usage:
#' Any JavaScript function defined in your script that begins with `shinyjs.`
#' will be available to run from R through the `js$` variable. For example,
#' if you write a JavaScript function called `shinyjs.myfunc`, then you can
#' call it in R with `js$myfunc()`.
#'
#' It's recommended to write JavaScript code in a separate file and provide the
#' filename as the \code{script} argument, but it's also possible to use the
#' \code{text} argument to provide a string containing valid JavaScript code. Using the
#' \code{text} argument is meant to be used when your JavaScript code is very short
#' and simple.
#'
#' As a simple example, here is a basic example of using \code{extendShinyjs}
#' to define a function that changes the colour of the page.
#'
#' \preformatted{
#' library(shiny)
#' library(shinyjs)
#'
#' jsCode <- "shinyjs.pageCol = function(params){$('body').css('background', params);}"
#'
#' runApp(shinyApp(
#'   ui = fluidPage(
#'     useShinyjs(),
#'     extendShinyjs(text = jsCode),
#'     selectInput("col", "Colour:",
#'                 c("white", "yellow", "red", "blue", "purple"))
#'   ),
#'   server = function(input,output,session) {
#'     observeEvent(input$col, {
#'       js$pageCol(input$col)
#'     })
#'   }
#' ))
#' }
#'
#' As the example above shows, after defining the JavaScript function
#' \code{shinyjs.pageCol} and passing it to \code{extendShinyjs}, it's possible
#' to call \code{js$pageCol()}.
#'
#' You can add more functions to the JavaScript code, but remember that every
#' function you want to use in R has to have a name beginning with
#' `shinyjs.`. See the section on passing arguments and the examples below
#' for more information on how to write effective functions.
#'
#' @section Passing arguments from R to JavaScript:
#' Any \code{shinyjs} function that is called will pass a single array-like
#' parameter to its corresponding JavaScript function. If the function in R was
#' called with unnamed arguments, then it will pass an Array of the arguments;
#' if the R arguments are named then it will pass an Object with key-value pairs.
#' For example, calling \code{js$foo("bar", 5)} in R will call \code{shinyjs.foo(["bar", 5])}
#' in JS, while calling \code{js$foo(num = 5, id = "bar")} in R will call
#' \code{shinyjs.foo({num : 5, id : "bar"})} in JS. This means that the
#' \code{shinyjs.foo} function needs to be able to deal with both types of
#' parameters.
#'
#' To assist in normalizing the parameters, \code{shinyjs} provides a
#' \code{shinyjs.getParams()} function which serves two purposes. First of all,
#' it ensures that all arguments are named (even if the R function was called
#' without names). Secondly, it allows you to define default values for arguments.
#' Here is an example of a JS function that changes the background colour of an
#' element and uses \code{shinyjs.getParams()}.
#'
#' \preformatted{
#' shinyjs.backgroundCol = function(params) {
#'   var defaultParams = {
#'     id : null,
#'     col : "red"
#'   };
#'   params = shinyjs.getParams(params, defaultParams);
#'
#'   var el = $("#" + params.id);
#'   el.css("background-color", params.col);
#' }
#' }
#'
#' Note the \code{defaultParams} object that was defined and the call to
#' \code{shinyjs.getParams}. It ensures that calling \code{js$backgroundCol("test", "blue")}
#' and \code{js$backgroundCol(id = "test", col = "blue")} and
#' \code{js$backgroundCol(col = "blue", id = "test")} are all equivalent, and
#' that if the colour parameter is not provided then "red" will be the default.
#' All the functions provided in \code{shinyjs} make use of \code{shinyjs.getParams},
#' and it is highly recommended to always use it in your functions as well.
#' Notice that the order of the arguments in \code{defaultParams} in the
#' JavaScript function matches the order of the arguments when calling the
#' function in R with unnamed arguments. This means that \code{js$backgroundCol("blue", "test")}
#' will not work because the arguments are unnamed and the JS function expects
#' the id to come before the colour. See the examples below for a shiny app
#' that uses this JS function.
#' @param script Path to a JavaScript file that contains all the functions.
#' Each function name must begin with `shinyjs.`, for example
#' `shinyjs.myfunc`. See 'Basic Usage' below.
#' @param text Inline JavaScript code to use. If your JavaScript function is very
#' short and you don't want to create a separate file for it, you can provide the
#' code as a string. See 'Basic Usage' below.
#' @return Scripts that \code{shinyjs} requires in order to run your JavaScript
#' functions as if they were R code.
#' @note You still need to call \code{useShinyjs()} as usual, and you need to
#' call \code{useShinyjs()} before calling \code{extendShinyjs()}.
#' @note The \code{V8} package is required to use this function.
#' @seealso \code{\link[shinyjs]{runExample}}
#' @examples
#' \dontrun{
#'   Example 1:
#'   Change the page background to a certain colour when a button is clicked.
#'
#'     jsCode <- "shinyjs.pageCol = function(params){$('body').css('background', params);}"
#'
#'     runApp(shinyApp(
#'       ui = fluidPage(
#'         useShinyjs(),
#'         extendShinyjs(text = jsCode),
#'         selectInput("col", "Colour:",
#'                     c("white", "yellow", "red", "blue", "purple"))
#'       ),
#'       server = function(input,output,session) {
#'         observeEvent(input$col, {
#'           js$pageCol(input$col)
#'         })
#'       }
#'     ))
#'
#'   ==============
#'
#'   Example 2:
#'   Change the background colour of an element, using "red" as default
#'
#'     jsCode <- '
#'     shinyjs.backgroundCol = function(params) {
#'       var defaultParams = {
#'         id : null,
#'         col : "red"
#'       };
#'       params = shinyjs.getParams(params, defaultParams);
#'
#'       var el = $("#" + params.id);
#'       el.css("background-color", params.col);
#'     }'
#'
#'     runApp(shinyApp(
#'       ui = fluidPage(
#'         useShinyjs(),
#'         extendShinyjs(text = jsCode),
#'         p(id = "name", "My name is Dean"),
#'         p(id = "sport", "I like soccer"),
#'         selectInput("col", "Colour:",
#'                     c("white", "yellow", "red", "blue", "purple")),
#'         textInput("selector", "Element", "sport"),
#'         actionButton("btn", "Go")
#'       ),
#'       server = function(input,output,session) {
#'         observeEvent(input$btn, {
#'           js$backgroundCol(input$selector, input$col)
#'         })
#'       }
#'     ))
#'
#'   ==============
#'
#'   Example 3:
#'   Create an `increment` function that increments the number inside an HTML
#'   tag (increment by 1 by default, with an optional parameter). Use a separate
#'   file instead of providing the JS code in a string.
#'
#'   Create a JavaScript file "myfuncs.js":
#'     shinyjs.increment = function(params) {
#'       var defaultParams = {
#'         id : null,
#'         num : 1
#'       };
#'       params = shinyjs.getParams(params, defaultParams);
#'
#'       var el = $("#" + params.id);
#'       el.text(parseInt(el.text()) + params.num);
#'     }
#'
#'   And a shiny app that uses the custom function we just defined. Note how
#'   the arguments can be either passed as named or unnamed, and how default
#'   values are set if no value is given to a parameter.
#'
#'       library(shiny)
#'       runApp(shinyApp(
#'         ui = fluidPage(
#'           useShinyjs(),
#'           extendShinyjs("myfuncs.js"),
#'           p(id = "number", 0),
#'           actionButton("add", "js$increment('number')"),
#'           actionButton("add5", "js$increment('number', 5)"),
#'           actionButton("add10", "js$increment(num = 10, id = 'number')")
#'         ),
#'         server = function(input,output,session) {
#'           observeEvent(input$add, {
#'             js$increment('number')
#'           })
#'           observeEvent(input$add5, {
#'             js$increment('number', 5)
#'           })
#'           observeEvent(input$add10, {
#'             js$increment(num = 10, id = 'number')
#'           })
#'         }
#'       ))
#' }
#' @export
extendShinyjs <- function(script, text) {
  if (!requireNamespace("V8", quietly = TRUE)) {
    errMsg(paste0("V8 package is required to use `extendShinyjs`. Please install it ",
                  "with `install.packages(\"V8\")`."))
  }

  if (missing(script) && missing(text)) {
    errMsg("Either `script` or `text` need to be provided.")
  }

  # create a js context with a `shinyjs` object that user-defined functions
  # can populate
  ct <- V8::new_context(NULL, FALSE, FALSE)
  ct$assign("shinyjs", c())

  # read functions from a script
  if (!missing(script)) {
    if (!file.exists(script)) {
      errMsg(sprintf("Could not find JavaScript file `%s`.", script))
    }
    tryCatch({
      ct$source(script)
    }, error = function(err) {
      errMsg(sprintf("Error parsing the JavaScript file: %s.", err$message))
    })
  }

  # read functions from in-line text
  if (!missing(text)) {
    tryCatch({
      ct$eval(text)
    }, error = function(err) {
      errMsg(sprintf("Error parsing the JavaScript code provided.", err$message))
    })
  }

  # find out what functions the user defined
  jsFuncs <- ct$get(V8::JS("Object.keys(shinyjs)"))
  if (length(jsFuncs) == 0) {
    errMsg(paste0("Could not find any shinyjs functions in the JavaScript file. ",
                  "Did you remember to prepend every function's name with `shinyjs.`?"))
  }

  # add all the given functions to the shinyjs namespace so that they can be
  # called as if they were regular shinyjs functions
  lapply(jsFuncs, function(x) {
    assign(x, jsFunc, js)
  })

  # set up the message handlers for all functions
  setupJS(jsFuncs, script, text)
}


#' Call user-defined JavaScript functions from R
#' @seealso \code{\link[shinyjs]{extendShinyjs}}
#' @export
#' @keywords internal
js <- new.env()
