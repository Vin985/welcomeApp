library(shiny)


actionButtonStyled <-
  function(inputId,
           label,
           style = "" ,
           class = "") {
    if (class == "") {
      class <- "btn action-button btn-default"
    }

    tags$button(
      id = inputId,
      style = style,
      type = "button",
      class = class,
      label
    )
  }



passwordEncryptedInput <-
  function(inputId,
           label,
           value = "",
           width = NULL) {
    div(
      class = "form-group shiny-input-container",
      style = if (!is.null(width))
        paste0("width: ", validateCssUnit(width), ";"),
      tags$label(label, `for` = inputId),
      tags$input(
        id = inputId,
        type = "password",
        class = "form-control",
        value = value
      )
    )
  }



uiLoginHeader <- function() {
  tagList(tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "assets/css/style.css"),
    tags$script(type = "text/javascript", src = "assets/js/spark-md5.min.js"),
    tags$script(type = "text/javascript", src = "assets/js/passwdEncryptBinding.js"),
    tags$script(type = "text/javascript", src = "assets/js/message-handler.js")
  ))
}

uiTabPanelChangePassword <- function() {
  tabPanel("Modifier le mot de passe",
           fluidPage(mainPanel(
             div(
               class = "modal in",
               id = "login-modalChangePassword",
               tabindex = "-1",
               role = "dialog",
               "aria-labelledby" = "myModalLabel",
               "aria-hidden" = "true",
               style = "display: modal-dialog; margin-top:100px;",
               div(class = "modal-dialog",
                   div(
                     class = "loginmodal-container",
                     h1("Modifier le mot de passe"),
                     br(),
                     #div(id="uiLogin",
                     div(
                       #textInput("userName1", "Nom utilisateur:", placeholder ="Username"),
                       textInput("email", "Courriel:", placeholder =
                                   "courriel"),
                       passwordEncryptedInput("passwdOld", "Ancien mot de passe:"),
                       passwordEncryptedInput("passwdNew", "Nouveau mot de passe:"),
                       br(),
                       actionButton("changePasswordbutton", "Modifier le mot de passe", class =
                                      "btn btn-success action-button login loginmodal-submit"),
                       htmlOutput("loginErrorChangePassword")
                     )
                   ))
             )
           )))

}

loginModal <- function(userInfo) {
  userInfo$loginError <- NULL
  modalDialog(
    div(
      class = "loginmodal-container",
      h1(geti18nValue("portal.login", userInfo$lang)),
      br(),
      div(
        textInput(
          "userName",
          geti18nValue("username", userInfo$lang),
          placeholder = geti18nValue("username", userInfo$lang)
        ),
        passwordEncryptedInput("passwd", geti18nValue("password", userInfo$lang)),
        br(),
        actionButton("loginAction", geti18nValue("login", userInfo$lang), class = "btn btn-primary action-button login loginmodal-submit"),
        htmlOutput("loginError")
      )
    ),
    div(class = "login-help"),
    footer = NULL,
    easyClose = TRUE
  )
}

