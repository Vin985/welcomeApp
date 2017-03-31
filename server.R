# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyjs)
library(ecapputils)

source("serverRender.R")




selectPage <- function(input, output, session, lang) {
  if (is.null(isolate(lang))) {
    output$main <- selectLanguageRender(input, output, session, lang)
  } else {
    output$main <- selectApplicationRender(input, output, session, lang)
  }
}




# Generate the observers for the application buttons
changeAppHandler <-
  function(input, output, session, lang) {
    # use lapply because it doesn't seem to work with a for loop
    lapply(APP_URL$id, function(x, input, lang) {
      observeEvent(input[[paste0(x, "app")]], {
        # create the javascript to redirect to the app with the selected language
        js <-
          paste0("window.location = '", APP_URL[match(x, APP_URL$id), "url"],
                 "?lang=", lang , "';")
        # print(js)
        runjs(js)
      })
    }, input, lang)

  }




shinyServer(function(input, output, session) {

  userInfo <- reactiveValues()

  checkQueryLanguage(session, userInfo)
  changeLanguageHandler(input, userInfo, event = CHANGE_LANG_EVENT)
  output$changeLanguage <- renderUI({
    changeLanguageOutput(userInfo$lang, button = TRUE)
  })
  
  observe({
    # This line is needed to make sure the observe is rerun when the language change
    userInfo$lang
    selectPage(input, output, session, userInfo$lang)
  })
})
