# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)






## Select page to display depending on if language has been selected or not
selectPage <- function(input, output, session, userInfo) {
  if (is.null(isolate(userInfo$lang))) {
    selectLanguagePage(input, output, session, userInfo)
  } else {
    selectApplicationPage(input, output, session, userInfo)
  }
}


## Generate the observers for the application buttons
selectAppHandler <- function(input, output, session, userInfo) {
  # use lapply because it doesn't seem to work with a for loop
  lapply(APP_URL$id, function(x, input, lang) {
    observeEvent(input[[paste0(x, "App")]], {
      userInfo$selectedApp <- x
    })
  }, input, userInfo$lang)

}


## Main function
shinyServer(function(input, output, session) {

  userInfo <- reactiveValues()
  userInfo$selectedApp <- NULL
  userInfo$user <- NULL

  queryArgs <- getInfoFromQueryString(parseQueryString(isolate(session$clientData$url_search)))
  if (!is.null(queryArgs)) {
    lang <- queryArgs$lang
    user <- queryArgs$user
  }
  userInfo$lang <- lang
  userInfo$user <- user

  # Language handlers
  changeLanguageHandler(input, userInfo, event = CHANGE_LANG_EVENT)
  output$changeLanguage <- renderUI({
    changeLanguageOutput(userInfo$lang, button = TRUE)
  })

  # App selection handler
  selectAppHandler(input, output, session, userInfo)

  # login logic
  loginServer(input, output, session, userInfo)

  # Change page observer
  observe({
    # This line is needed to make sure the observe is rerun when the language change
    userInfo$lang
    selectPage(input, output, session, userInfo)
  })


  # Listener to the go to app button
  observeEvent(input$goToApp, {
    app <- isolate(userInfo$selectedApp)
    lang <- isolate(userInfo$lang)
    user <- isolate(userInfo$user)
    js <- generateApplicationURL(app, lang, user)
    runjs(js)
  })

})
