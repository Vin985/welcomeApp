source("loginUI.R", encoding = "utf-8")
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


  observeEvent(input$navBarPageUser, {
    if (input$navBarPageUser != '<span id="disconnect" class="shiny-text-output"></span>') {
      return()
    }

    isolate({
      USER$Logged <- FALSE
      USER$Admin <- FALSE
      stop("'session' is not a ShinySession object.")
    })
  })


  observeEvent(input$adminPage, {
    print("input$adminPage = ")
    if (!(input$navBarPageAdmin %in% c(encode("Se déconnecter"), encode("Gérer les utilisateurs")))){
      return()
    }

    isolate({
      if (input$navBarPageAdmin != encode("Se déconnecter")){
          USER$Logged = FALSE
          stop("'session' is not a ShinySession object.")
      }
      #else{
      #  return("admin")
      #}
    })
  })

  #==============================================================================================================================================
  ## login
  #




}

loginRender <- function(input, output, session, userInfo) {

  output$login <- renderUI({
    if (!isLogged(userInfo$user)) {
      actionLink("login", geti18nValue("login", userInfo$lang))
    } else {
      username <- isolate(userInfo$user$name)
      if (is.null(username)) {
        username <- ""
      }
      tagList(textOutput2(content = paste(geti18nValue("welcome.user", userInfo$lang), username), inline = TRUE, class = "link"),
              # if (isAdmin(userInfo$user)) {
              #   span(class = "link", actionLink("adminPage", geti18nValue("admin.page", userInfo$lang)))
              # } else {
              #   ""
              # },
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


  #==============================================================================================================================================
  ## Admin section
  output$loginSection <- renderUI({
    if (USER$Logged == FALSE) {
      return(uiLoginMOdalDialog())
    }

    if (isAdmin(USER$username, USER$password)) {
      div(
        class = "container",
        div(
          class = "row",
          div(
            class = "col-sm-12 col-md-12 col-lg-12",
            shinyjs::useShinyjs(),

            # display cRUD ops errors
            htmlOutput("crudErrors")#,

            #data table
            #DT::dataTableOutput("responses")
          )
        ),

        div(
          class = "row",
          div(class = "col-sm-6 col-md-5 col-lg-6", tags$hr())
        ),

        div(
          class = "row",
          div(
            class = "col-sm-12 col-md-12 col-lg-12",
            #input fields
            shinyjs::disabled(textInput("id", "Id", "0")),

            #shinyjs::disabled(textInput("username", "Nom utilisateur", "")),
            textInput("username", geti18nValue("username", userInfo$lang), ""),
            textInput("email", geti18nValue("email", userInfo$lang), ""),
            passwordEncryptedInput("password", geti18nValue("password", userInfo$lang), "")
            #passwordEncryptedInput("password2", encode("Répéter le mot de passe"), "")
          )
        ),

        div(
          class = "row",
          div(class = "col-sm-6 col-md-5 col-lg-6", tags$hr())
        ),

        div(
          class = "row",
          div(
            class = "col-sm-12 col-md-12 col-lg-12",
            #action buttons
            actionButtonStyled(
              'submit',
              geti18nValue("account.create.update", userInfo$lang),
              style = "width:300px;",
              class = "btn btn-info action-button btn-lg"
            ),
            actionButtonStyled(
              'new',
              geti18nValue("account.init", userInfo$lang),
              style = "width:300px;",
              class = "btn btn-success action-button btn-lg"
            ),
            actionButtonStyled(
              'delete',
              geti18nValue("account.delete", userInfo$lang),
              style = "width:300px;",
              class = "btn btn-danger action-button btn-lg"
            )
          )
        )
      )

    } else{
      print("----------- not admin")
      #USER$AdminTry <- TRUE
      #USER$Logged   <- FALSE

      return(uiLoginMOdalDialog(error = geti18nValue("login.error4", userInfo$lang)))

    }

  })
}


