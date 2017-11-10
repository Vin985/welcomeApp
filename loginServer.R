

ERROR_INVALID_OLD_PASSWORD <- 1
ERROR_INVALID_NEW_PASSWORD <- 2
ERROR_INVALID_NOMATCH <- 3
ERROR_INVALID_EMAIL <- 4


## Check if an object is invalid
isInvalid <- function(toBeChecked) {
  if (is.null(toBeChecked))
    return(TRUE)
  return(gtools::invalid(toBeChecked))
}


## Email regular expression
isEmailValid <- function(emailToBeChecked) {
  if (isInvalid(emailToBeChecked))
    return(FALSE)
  regexpr(
    "^[a-zA-Z0-9._%+-]{1,64}@(?:[a-zA-Z0-9-]{1,63}\\.){1,125}[a-zA-Z]{2,63}$",
    emailToBeChecked,
    ignore.case = TRUE
  ) == TRUE
}

loginServer <- function(input, output, session, userInfo) {
  loginRender(input, output, session, userInfo)

  # Login
  observeEvent(input$login, {
    showModal(div(class = "login", loginModal(userInfo)))
  })

  # Logout
  observeEvent(input$logout, {
    userInfo$user <- NULL
  })

  # Go to admin page
  observeEvent(input$adminPage, {
    userInfo$page <- PAGE_ADMIN
  })

  # Go to application page
  observeEvent(input$appPage, {
    userInfo$page <- PAGE_APP
  })

  # Change password
  observeEvent(input$changePassword, {
    showModal(div(class = "login", changePasswordModal(userInfo)))
  })

  # Check credentials for login
  observeEvent(input$checkCredentials, {
    userInfo$inputErrors <- NULL
    print("in observe loginAction")
    if (!isLogged(isolate(userInfo$user))) {
      username <- isolate(input$userName)
      password <- isolate(input$passwd)

      creds <-
        credentialsMatch(username = username, password = password)

      if (creds > 0) {
        user <-
          list(status = creds,
               name = username,
               time = as.numeric(Sys.time()))
        userInfo$user <- user
        removeModal(session)
      } else {
        userInfo$inputErrors <- creds
      }
    }
  }, ignoreInit = TRUE)




  # Update password
  observeEvent(input$changePasswordAction, {
    print("chang password action clicked")
    userInfo$inputErrors <- NULL
    user <- isolate(userInfo$user)
    if (isLogged(user)) {
      username    <- user$name
      passwordOld <- isolate(input$passwdOld)
      passwordNew <- isolate(input$passwdNew)
      passwordNew2 <- isolate(input$passwdNew2)
      email       <- isolate(input$email)

      inputErrors <- NULL

      if (isInvalid(passwordOld) || passwordOld == "") {
        inputErrors <- c(inputErrors, ERROR_INVALID_OLD_PASSWORD)
      }
      if (isInvalid(passwordNew) || passwordNew == "") {
        inputErrors <- c(inputErrors, ERROR_INVALID_NEW_PASSWORD)
      } else if (passwordNew != passwordNew2) {
        inputErrors <- c(inputErrors, ERROR_INVALID_NOMATCH)
      }
      if (isInvalid(email) ||
          email == "" ||  !isEmailValid(email)) {
        inputErrors <- c(inputErrors, ERROR_INVALID_EMAIL)
      }

      if (length(inputErrors) > 0) {
        userInfo$inputErrors <- inputErrors
        return()
      }

      creds <-
        credentialsMatch(username = username, password = passwordOld)

      if (creds > 0) {
        updateUser(username = username,
                   email = email,
                   password = passwordNew)
        session$sendCustomMessage(type = 'jsCode', message = list(value = paste0(
          "alert('",
          geti18nValue("login.saved", userInfo$lang),
          "');"
        )))
        removeModal(session)
      } else {
        userInfo$inputErrors <- creds
      }
    }
  }, ignoreInit = TRUE)


}

loginRender <- function(input, output, session, userInfo) {
  output$loginButtons <- renderUI({
    if (!isLogged(userInfo$user)) {
      # User is not logged in, display login link
      actionLink("login", geti18nValue("login", userInfo$lang))
    } else {
      username <- isolate(userInfo$user$name)
      if (is.null(username)) {
        username <- ""
      }
      # Display admin page is user is admin
      adminPage <- if (isAdmin(userInfo$user)) {
        if (userInfo$page == PAGE_APP) {
          span(class = "link", actionLink(
            "adminPage",
            geti18nValue("admin.page", userInfo$lang)
          ))
        } else {
          span(class = "link", actionLink("appPage", geti18nValue("app.page", userInfo$lang)))
        }
      }
      # Change password link
      changePassword <- span(class = "link", actionLink(
        "changePassword",
        geti18nValue("change.password", userInfo$lang)
      ))

      # Display welcome message, change password and logut button
      tagList(
        textOutput2(
          content = paste(geti18nValue("welcome.user", userInfo$lang), username),
          inline = TRUE,
          class = "link"
        ),
        adminPage,
        changePassword,
        span(class = "link", actionLink(
          "logout", geti18nValue("logout", userInfo$lang)
        ))
      )
    }
  })

  output$inputError <- renderUI({
    errors <- userInfo$inputErrors
    lapply(errors, function(error) {
      div(geti18nValue(paste0("input.error.", error), userInfo$lang), class = "text-danger")
    })

  })

}
