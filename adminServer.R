


adminServer <- function(input, output, session, userInfo) {
  #==============================================================================================================================================
  ## CRUD reactive code
  #
  # input fields are treated as a group
  formData <- reactive({
    sapply(names(getTableMetadata()$fields), function(x)
      input[[x]])
  })

  userList <- reactiveVal(getUsers())

  # Click "Submit" button -> save data
  observeEvent(input$submit, {
    if (input$id != "0") {
      # it is a selected account to be updated
      updateData(formData(), userList)
    } else {
      # create new user
      createData(formData(), userList)
      updateInputs(createDefaultRecord(), session)
    }
  }, priority = 1)


  # Press "New" button -> display empty record
  observeEvent(input$reset, {
    updateInputs(createDefaultRecord(), session)
  })


  # Press "Delete" button -> delete from data
  observeEvent(input$delete, {
    deleteData(formData(), userList, output)
    updateInputs(createDefaultRecord(), session)
  }, priority = 1)


  # Select row in table -> show details in inputs
  observeEvent(input$responses_rows_selected, {
    if (length(input$responses_rows_selected) > 0) {
      data <- userList()[input$responses_rows_selected,]
      updateInputs(data, session)
    }

  })


  # display table
  output$responses <- DT::renderDataTable({
    # require the presence of "responses" id
    req(isAdmin(userInfo$user))
    d <- userList()
  }, server = FALSE, selection = "single",
  colnames = unname(getTableMetadataToDisplayInTable()$fields))
}


#==============================================================================================================================================
## CRUD helpers
# data is of type list: list(id = "", username = "", password = "", email = "")
castData <- function(data, pwd = TRUE) {
  if (!pwd) {
    data <- data[!names(data) %in% c("password")]
  }
  datar <- data.frame(as.list(data), stringsAsFactors = FALSE)
  return(datar)
}


createDefaultRecord <- function() {
  mydefault <-
    castData(list(
      id = "0",
      username = "",
      password = "",
      email = ""
    ))
  return(mydefault)
}


updateInputs <- function(data, session) {
  updateTextInput(session, "id", value = data$id)
  updateTextInput(session, "username", value = data$username)
  updateTextInput(session, "email", value = data$email)
  updateTextInput(session, "password", value = data$password)
}


#==============================================================================================================================================
## CRUD ops
#

createData <- function(data, userList) {
  # save data to database
  addUserFromList(data)
  # add the data to the datatable responses (DT::dataTableOutput)
  userList(getUsers())
}



updateData <- function(data, userList) {
  # update the user data row in the DB
  updateUserFromListByID(data)
  userList(getUsers())
}


deleteData <- function(data, userList, output) {
  # delete from DB
  user <- getUserById(data["id"])

  if (user["admin"] == 1) {
    output$crudErrors <- renderUI({
      span("Opération non autorisée. Un compte administrateur ne peut être supprimé.",
           class = "text-danger")
    })
  } else {
    output$crudErrors <- renderUI({
    })

    deleteUserByID(data["id"])
    userList(getUsers())
  }

}

getTableMetadata <- function() {
  fields <- c(
    id = "Id",
    username = "username",
    password = "password",
    email = "email"
  )
  result <- list(fields = fields)
  return(result)
}

# password field is not displayed
getTableMetadataToDisplayInTable <- function() {
  fields <- c(id = "Id",
              username = "username",
              #password = "password",
              email = "email")
  result <- list(fields = fields)
  return(result)
}
