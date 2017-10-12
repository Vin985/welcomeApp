require(shiny)
require(shinyjs)
require(ecapputils)

# Generate application selection links
generateApplicationLinks <- function(userInfo) {
  if (!is.null(EC_APP_CONF)) {
    lapply(names(EC_APP_CONF), function(x, userInfo) {
      # Display buttons if application is public or if user is logged in
      if (x != "main" &&
          (!EC_APP_CONF[[x]]$private || isLogged(userInfo$user))) {
        actionButton(paste0(x, "App"),
                     geti18nValue(paste0(x, ".app"), userInfo$lang),
                     class = "appLink btn-block text-capitalize h2")
      }
    }, userInfo)
  }
}

## Language selection page
displayLanguagePage <- function(input, output, session, userInfo) {
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
          changeLanguageOutput(class = "radioButtons btn btn-lg btn-success", style = "margin: 0 5px")
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
displayApplicationPage <-
  function(input, output, session, userInfo) {
    # Toolbar
    output$toolBar <- renderUI({
      fluidRow(class = "topRow",
               column(
                 9,
                 offset = 1,
                 uiOutput(class = "loginButtons", "loginButtons")
               ),
               column(1, class = "navButtons",
                      uiOutput("changeLanguage")))
    })

    # global Layout
    output$main <- renderUI({
      tagList(
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


displayAdminPage <- function(input, output, session, userInfo) {
  # global Layout
  output$main <- renderUI({
    if (isAdmin(userInfo$user)) {
      fluidRow(column(
        8,
        offset = 2,

        div(tags$hr()),

        div(
          class = "jumbotron",
          # display CRUD ops errors
          htmlOutput("crudErrors"),

          #data table
          DT::dataTableOutput("responses"),

          div(style = "margin-top: 50px",
            #input fields
            shinyjs::disabled(textInput("id", "Id", "0")),

            textInput("username", geti18nValue("username", userInfo$lang), ""),
            textInput("email", geti18nValue("email", userInfo$lang), ""),
            passwordEncryptedInput("password", geti18nValue("password", userInfo$lang), "")
          ),

          div(class = "evenly",
              style = "margin-top: 50px",
            #action buttons
            actionButtonStyled(
              'submit',
              geti18nValue("account.create.update", userInfo$lang),
              class = "btn btn-info action-button btn-lg btn-30"
            ),
            actionButtonStyled(
              'new',
              geti18nValue("account.init", userInfo$lang),
              class = "btn btn-success action-button btn-lg btn-30"
            ),
            actionButtonStyled(
              'delete',
              geti18nValue("account.delete", userInfo$lang),
              class = "btn btn-danger action-button btn-lg btn-30"
            )
          )
        )
      ))
    } else{
      print("----------- not admin")
      return(uiLoginMOdalDialog(error = geti18nValue("login.error4", userInfo$lang)))
    }
  })


}
