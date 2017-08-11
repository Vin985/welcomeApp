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
  #=============================================================================================================================================
  ## Login/logout per-session status and reactive value
  #
  Logged = FALSE
  USER <- reactiveValues(Logged = Logged, Admin = FALSE)

  #=============================================================================================================================================
  ## component-less UI object to manage the Login/logout status
  #
  output$loggedIn <- reactive({
    #print("we are in reactive logedIn")
    if (USER$Logged) {
      return("true")
    } else{
      return("false")
    }
  })

  outputOptions(output, "loggedIn", suspendWhenHidden = FALSE)


  #=============================================================================================================================================
  ## component-less UI object to manage the Login/logout status
  #
  output$role <- reactive({
    #print("we are in reactive logedIn")
    if (USER$Logged) {
      if (isAdmin(USER$username, USER$password))
        return("admin")
    }
    return("user")
  })

  outputOptions(output, "role", suspendWhenHidden = FALSE)


  #==============================================================================================================================================
  ## logout
  #
  observeEvent(input$logoutbutton, {
    USER$Logged <- FALSE
    USER$Admin  <- TRUE
    stop("'session' is not a ShinySession object.")
  })



  observeEvent(input$navBarPageUser, {
    #if (! input$navBarPageUser %in% c("Se déconnecter", "Disconnect")){
    if (input$navBarPageUser != '<span id="disconnect" class="shiny-text-output"></span>') {
      return()
    }

    isolate({
      USER$Logged <- FALSE
      USER$Admin <- FALSE
      stop("'session' is not a ShinySession object.")
    })
  })


  # observeEvent(input$navBarPageAdmin, {
  #   print("input$navBarPageAdmin = ")
  #   print(input$navBarPageAdmin)
  #   if (!(input$navBarPageAdmin %in% c(encode("Se déconnecter"), encode("Gérer les utilisateurs")))){
  #     return()
  #   }
  #
  #   isolate({
  #     if (input$navBarPageAdmin != encode("Se déconnecter")){
  #         USER$Logged = FALSE
  #         stop("'session' is not a ShinySession object.")
  #     }
  #     #else{
  #     #  return("admin")
  #     #}
  #   })
  # })

  #==============================================================================================================================================
  ## login
  #

  output$loginError <- renderUI({
    print("in output$loginError")

    if (!userInfo$logged &&
        !is.null(input$loginAction) && input$loginAction > 0) {

      username <- isolate(input$userName)
      password <- isolate(input$passwd)

      if (credentialsMatch(username = username, password = password)) {
        USER$username <- username
        USER$password <- password
        USER$Logged <- TRUE
        if (isAdmin(username, password)) {
          USER$Admin <- TRUE
          br()
        }
      } else  {
        span(geti18nValue("login.error", userInfo$lang), class = "text-danger")
      }
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
  #
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


