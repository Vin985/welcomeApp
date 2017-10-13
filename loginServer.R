# source("sqlite3.R")

#=============================================================================================================================================
## check if an object is (in)vvalid
#
isInvalid <- function(toBeChecked) {
  if (is.null(toBeChecked))
    return(TRUE)
  return(gtools::invalid(toBeChecked))
}


#=============================================================================================================================================
## email regular expression
#
isEmailValid <- function(emailToBeChecked) {
  if (isInvalid(emailToBeChecked))
    return(FALSE)
  regexpr(
    "^[a-zA-Z0-9._%+-]{1,64}@(?:[a-zA-Z0-9-]{1,63}\\.){1,125}[a-zA-Z]{2,63}$",
    emailToBeChecked,
    ignore.case = TRUE
  ) == TRUE
}

isAdmin <- function(user) {
  if (is.null(user)) {
    return(FALSE)
  }
  return(user$status == STATUS_ADMIN)
}

loginServer <- function(input, output, session, userInfo) {

  loginRender(input, output, session, userInfo)

  observeEvent(input$login, {
    showModal(div(class = "login", loginModal(userInfo)))
  })


  #==============================================================================================================================================
  ## logout
  #
  observeEvent(input$logout, {
    userInfo$user <- NULL
  })

  observeEvent(input$checkCredentials, {
    print("in observe loginAction")
    if (!isLogged(isolate(userInfo$user))) {

      username <- isolate(input$userName)
      password <- isolate(input$passwd)

      creds <- credentialsMatch(username = username, password = password)

      if (creds > 0) {
        user <- list(status = creds, name = username, token = generateUserStatus(creds))
        userInfo$user <- user
        removeModal(session)
      } else {
        userInfo$loginError <- creds
      }
    }
  }, ignoreInit = TRUE)


  observeEvent(input$adminPage, {
    userInfo$page <- PAGE_ADMIN
    #displayAdminPage(input, output, session, userInfo)
  })

  observeEvent(input$appPage, {
    userInfo$page <- PAGE_APP
  })

  #==============================================================================================================================================
  ## login
  #




}

loginRender <- function(input, output, session, userInfo) {

  output$loginButtons <- renderUI({
    if (!isLogged(userInfo$user)) {
      actionLink("login", geti18nValue("login", userInfo$lang))
    } else {
      username <- isolate(userInfo$user$name)
      if (is.null(username)) {
        username <- ""
      }
      adminPage <- if (isAdmin(userInfo$user)) {
        if (userInfo$page == PAGE_APP) {
        span(class = "link", actionLink("adminPage", geti18nValue("admin.page", userInfo$lang)))
      } else {
        span(class = "link", actionLink("appPage", geti18nValue("app.page", userInfo$lang)))
      }
      }
      tagList(textOutput2(content = paste(geti18nValue("welcome.user", userInfo$lang), username), inline = TRUE, class = "link"),
              adminPage,
              span(class = "link", actionLink("logout", geti18nValue("logout", userInfo$lang))))
    }
  })

  output$loginError <- renderUI({
    error <- userInfo$loginError
    print("in output$loginError")
    if (!is.null(error)) {
      span(geti18nValue(paste0("login.error", error), userInfo$lang), class = "text-danger")
    }
  })

  #=============================================================================================================================================
  ## Change password button: loginErrorChangePassword
  #
  output$loginErrorChangePassword <- renderUI({
    if (USER$Logged == TRUE &
        !isInvalid(input$changePasswordbutton) &
        input$changePasswordbutton > 0) {
      #username    <- isolate(input$userName1)
      username    <- isolate(USER$username)
      passwordOld <- isolate(input$passwdOld)
      passwordNew <- isolate(input$passwdNew)
      email       <- isolate(input$email)


      if (isInvalid(username) ||
          isInvalid(passwordOld) ||
          isInvalid(passwordNew) || isInvalid(email)) {
        return(span(geti18nValue("login.error1", userInfo$lang), class = "text-danger"))
      }

      if (!isEmailValid(email)) {
        return(span(geti18nValue("login.error2", userInfo$lang), class = "text-danger"))
      }

      if (credentialsMatch(username = username, password = passwordOld)) {
        updateUser(username = username,
                   email = email,
                   password = passwordNew)
        session$sendCustomMessage(type = 'jsCode', message = list(value = paste0(
          "alert('", geti18nValue("login.saved", userInfo$lang), "');"
        )))
        return(span(
          geti18nValue("login:success"),
          class = "text-danger",
          style = ""
        ))
      } else  {
        return(span(geti18nValue("login.error3", userInfo$lang), class = "text-danger"))
      }
    }

  })

}


