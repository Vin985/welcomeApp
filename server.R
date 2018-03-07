# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)


## Select page to display depending on if language has been selected or not
selectPage <- function(input, output, session, userInfo) {
  page <- isolate(userInfo$page)
  if (page == PAGE_APP) {
    displayApplicationPage(input, output, session, userInfo)
  } else if (page == PAGE_ADMIN){
    displayAdminPage(input, output, session, userInfo)
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
  #userInfo$user <- NULL
  userInfo$page <- PAGE_LANG

  queryArgs <- getInfoFromQueryString(parseQueryString(isolate(session$clientData$url_search)))
  if (!is.null(queryArgs)) {
    lang <- queryArgs$lang
    user <- queryArgs$user
  }
  userInfo$lang <- lang

  # user$time <- as.numeric(Sys.time())
  # user$name <- "sylvain"
  # user$status <- 2

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
  adminServer(input, output, session, userInfo)

  # display language selection page at the beginning
  displayLanguagePage(input, output, session, userInfo)


  observeEvent(userInfo$page, {
    selectPage(input, output, session, userInfo)
  })

  # Change page observer
  observeEvent(userInfo$event, {
    event <- isolate(userInfo$event)
    if (event$type == CHANGE_LANG_EVENT && userInfo$page == PAGE_LANG) {
        userInfo$page <- PAGE_APP
    }
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
