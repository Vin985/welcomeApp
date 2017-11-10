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
    tags$script(type = "text/javascript", src = "js/spark-md5.min.js"),
    tags$script(type = "text/javascript", src = "js/passwdEncryptBinding.js"),
    tags$script(type = "text/javascript", src = "js/message-handler.js")
  ))
}

changePasswordModal <- function(userInfo) {
  modalDialog(
    div(
      class = "loginmodal-container",
      h1(geti18nValue("change.password", userInfo$lang)),
      br(),
      div(
        textInput(
          "email",
          geti18nValue("email", userInfo$lang),
          placeholder = geti18nValue("email", userInfo$lang)
        ),
        passwordEncryptedInput("passwdOld", geti18nValue("password.old", userInfo$lang)),
        passwordEncryptedInput("passwdNew", geti18nValue("password.new", userInfo$lang)),
        passwordEncryptedInput("passwdNew2", geti18nValue("password.new2", userInfo$lang)),
        br(),
        actionButton("changePasswordAction", geti18nValue("change.password", userInfo$lang), class =
                       "btn btn-success action-button login loginmodal-submit"),
        htmlOutput("inputError")
      )
    ),
    div(class = "login-help"),
    footer = NULL,
    easyClose = TRUE
  )
}

loginModal <- function(userInfo) {
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
        actionButton("checkCredentials", geti18nValue("login", userInfo$lang), class = "btn btn-primary action-button login loginmodal-submit"),
        htmlOutput("inputError")
      )
    ),
    div(class = "login-help"),
    footer = NULL,
    easyClose = TRUE
  )
}

