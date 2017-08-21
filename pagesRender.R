require(shiny)
require(shinyjs)
require(ecapputils)

# Generate application selection links
generateApplicationLinks <- function(userInfo) {
  if (!is.null(EC_APP_CONF)) {
    lapply(names(EC_APP_CONF), function(x, userInfo) {
      # Display buttons if application is public or if user is logged in
      if (x != "main" && (!EC_APP_CONF[[x]]$private || isLogged(userInfo$user))) {
        actionButton(paste0(x, "App"), geti18nValue(paste0(x, ".app"), userInfo$lang), class = "appLink btn-block text-capitalize h2")
      }
    }, userInfo)
  }
}

## Language selection page
selectLanguagePage <- function(input, output, session, userInfo) {
  lang <- userInfo$lang
  output$main <- renderUI({
    fluidRow(column(
      6,
      offset = 3,
      div(class = "masthead"),
      hr(),
      div(
        class = "jumbotron",
        style = "text-align: center",
        h1("GeoAviRWeb"),
        div(
          style = "margin-top: 30px;",
          changeLanguageOutput(class = "btn btn-lg btn-success", style = "margin: 0 5px")
        )
      ),
      hr(),
      fluidRow(class = "marketing-row",
               column(
                 6,
                 i18nText("portal.desc", lang = "fr")
               ),
               column(
                 6,
                 i18nText("portal.desc", lang = "en")
               )),
      div(class = "footer")

    ))
  })
}

## Application selection page
selectApplicationPage <-
  function(input, output, session, userInfo) {

    # Toolbar
    output$toolBar <- renderUI({
      fluidRow(class = "topRow",
               column(9, offset = 1, uiOutput(class = "loginButtons", "login")),
               column(1, class = "navButtons",
                        uiOutput("changeLanguage")
                      ))
    })

    # global Layout
    output$main <- renderUI({
      tagList(
        uiOutput("toolBar"),
        fluidRow(style = "margin-bottom: 20px;", column(10, h2(
          i18nText("select.app", userInfo$lang)
        ))),
        fluidRow(column(
          3,
          div(class = "appList", generateApplicationLinks(userInfo))
        ),
        column(8, uiOutput("appDesc"))),
        div(class = "footer")
      )
    })

    # Application description
    output$appDesc <- renderUI({
      app <- userInfo$selectedApp
      msg <- if (is.null(app)) {
        "select.app"
      } else {
        paste0(app, ".desc")
      }
      tagList(div(class = "appDesc",
                  i18nText(msg, userInfo$lang)),
              div(class = "actionButtons", if (!is.null(app)) {
                actionButton(class = "actionButton",
                             "goToApp",
                             geti18nValue("go.to.app", userInfo$lang))
              }))
    })
  }


