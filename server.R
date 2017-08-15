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
  userInfo$logged <- FALSE
  userInfo$admin <- FALSE

  # Language handlers
  checkQueryLanguage(session, userInfo)
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
    js <-
      paste0("window.location = '",
             EC_APP_CONF[[app]]$url,
             "?lang=",
             lang ,
             "';")
    runjs(js)
  })



  # output$showlogin <- renderUI({
  #   if (USER$Logged == FALSE) {
  #     language  <- userInfo$lang
  #
  #     outTag <- uiLoginModalDialog()
  #
  #     # outTag <-   div(
  #     #   div(
  #     #     class = "modal in",
  #     #     id = "login-modal",
  #     #     tabindex = "-1",
  #     #     role = "dialog",
  #     #     "aria-labelledby" = "myModalLabel",
  #     #     "aria-hidden" = "true",
  #     #     style = "display: modal-dialog;",
  #     #     div(
  #     #       class = "modal-dialog",
  #     #       div(
  #     #         class = "loginmodal-container",
  #     #         h1(
  #     #           ifelse(
  #     #             language == "fr",
  #     #             "Connexion à GeoAviRWeb",
  #     #             "Connect to GeoAviRWeb"
  #     #           )
  #     #         ),
  #     #         br(),
  #     #         div(
  #     #           textInput(
  #     #             "userName",
  #     #             ifelse(language == "fr", "Nom utilisateur:", "Username:"),
  #     #             placeholder = ifelse(language == "fr", "Nom utilisateur:", "Username")
  #     #           ),
  #     #           passwordEncryptedInput(
  #     #             "passwd",
  #     #             ifelse(language == "fr", "Mot de passe:", "Password:")
  #     #           ),
  #     #           br(),
  #     #           actionButton(
  #     #             "Login",
  #     #             ifelse(language == "fr", "Se connecter", "Connect"),
  #     #             class = "btn btn-primary action-button login loginmodal-submit"
  #     #           ),
  #     #
  #     #           #                   if(!is.null(error))
  #     #           #                     span(error, class ="text-danger"),
  #     #
  #     #           htmlOutput("loginError")
  #     #
  #     #         ),
  #     #         div(class = "login-help"#,
  #     #             #a(href="#", "Register"), a(href="#", "Forgot Password"))
  #     #         )
  #     #       )
  #     #     ),
  #     #     div(class = "modal-backdrop in")
  #     #   ))
  #
  #       tagList(list(outTag, br()))
  #
  #   }
  #   else{
  #     print("logged in")
  #     output$main <- selectApplicationRender(input, output, session, lang)
  #   }
  # })

})
