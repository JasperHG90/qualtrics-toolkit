# -----------------------------------------------------------------------
#
# Utility functions used to interact with qualtrics API
#
# Jasper Ginn
# 29/06/2018
#
# ------------------------------------------------------------------------

# MAIN FUNCTIONS ----

#' Authenticate your api token, data center and oauth credentials
#'
#' @param type either one of 'token' (API credentials) or 'oauth' (using secret and id)
#' @param use_keychain should qualtRics retrieve stored credentials from the keychain?
#' @param ... any of the options (api_token, data_center, client_id, client_secret) can be passed as named objects. If you pass these parameters, they will override e.g. environment variables.
#'
#' @return exits silently
#'
#' @import keyringr
#'
#' @export

qualtrics_authenticate <- function(type = c("token", "oauth"),
                                   use_keychain = FALSE,
                                   ...) {

  # Set use keychain to false
  Sys.setenv("QUALTRICS_USE_KEYCHAIN" = FALSE)

  # Plan ----

  # Depending on the type (token or oauth)
  # 1. Check environment variables for tokens (set via e.g. Rprofile)
  #    OR
  #     IF use_keychain
  #       check keychain for tokens (using keyringr package)
  #    OR
  #    Check the optional args. If client_secret, client_auth, data_center or api_token passed
  #    then use these.
  # 2. Set environment variables

  # Code ----

  # Get type
  type <- match.arg(type)

  # Get optional args
  opts <- list(...)
  if("client_secret" %in% names(opts)) {

    user_passed_secret <- opts$client_secret

    if(use_keychain) {

      warning(
        "'use_keychain' is set to TRUE and options passed to this function will not be saved
        in the local environment. If you want to override the default options, set 'use_keychain'
        to FALSE."
      )

    }

  } else {

    user_passed_secret <- NULL

  }

  if("client_id" %in% names(opts)) {

    user_passed_id <- opts$client_id

    if(use_keychain) {

      warning(
        "'use_keychain' is set to TRUE and options passed to this function will not be saved
        in the local environment. If you want to override the default options, set 'use_keychain'
        to FALSE."
      )

    }

  } else {

    user_passed_id <- NULL

  }

  if("api_token" %in% names(opts)) {

    user_passed_token <- opts$api_token

    if(use_keychain) {

      warning(
        "'use_keychain' is set to TRUE and options passed to this function will not be saved
        in the local environment. If you want to override the default options, set 'use_keychain'
        to FALSE."
      )

    }

  } else {

    user_passed_token <- NULL

  }

  if("data_center" %in% names(opts)) {

    user_passed_datacenter <- opts$data_center

    if(use_keychain) {

      warning(
        "'use_keychain' is set to TRUE and options passed to this function will not be saved
        in the local environment. If you want to override the default options, set 'use_keychain'
        to FALSE."
      )

    }

  } else {

    user_passed_datacenter <- NULL

  }

  # Split on auth type
  if(type == "oauth") {

    client_id <- Sys.getenv("QUALTRICS_CLIENT_ID")
    client_secret <- Sys.getenv("QUALTRICS_CLIENT_SECRET")
    data_center <- Sys.getenv("QUALTRICS_DATA_CENTER")

    # Check keychain
    if(use_keychain) {

      cred <- qualtrics_helper_keychain_credentials("oauth")

      check <- vapply(cred, function(x) {

        if(!is.null(x)) {
          if(length(x) > 0) {
            if(x > 0) {
              TRUE
            } else {
              FALSE
            }
          } else {
            FALSE
          }
        } else {
          FALSE
        }
      },
      TRUE)

      if(!all(check)) {

        stop(
          "An error occurred while retrieving your credentials from the keychain. Ensure that
          you set your credentials correctly or use another method to authenticate."
        )

      }

      Sys.setenv("QUALTRICS_USE_KEYCHAIN" = TRUE)

    }

    # If user passed credentials, override
    if(!is.null(user_passed_secret)) {

      client_secret <- user_passed_secret

    }
    if(!is.null(user_passed_id)) {

      client_id <- user_passed_id

    }
    if(!is.null(user_passed_datacenter)) {

      data_center <- user_passed_datacenter

    }

    # Set envs
    Sys.setenv("QUALTRICS_CLIENT_ID" = client_id)
    Sys.setenv("QUALTRICS_CLIENT_SECRET" = client_secret)
    Sys.setenv("QUALTRICS_DATA_CENTER" = data_center)

    # Set auth type
    Sys.setenv("QUALTRICS_AUTH_TYPE" = "oauth")

    # Check
    qualtrics_helper_envs_set("oauth")

    } else if(type == "token") {

      api_token <- Sys.getenv("QUALTRICS_API_TOKEN")
      data_center <- Sys.getenv("QUALTRICS_DATA_CENTER")

      # Check keychain
      if(use_keychain) {

        cred <- qualtrics_helper_keychain_credentials("token")

        check <- vapply(cred, function(x) {

          if(!is.null(x)) {
            if(length(x) > 0) {
              if(x > 0) {
                TRUE
              } else {
                FALSE
              }
            } else {
              FALSE
            }
          } else {
            FALSE
          }
        },
        TRUE)

        if(!all(check)) {

          stop(
            "An error occurred while retrieving your credentials from the keychain.\nEnsure that you set your credentials correctly or use another method to authenticate."
          )

        }

        Sys.setenv("QUALTRICS_USE_KEYCHAIN" = TRUE)

      }

      # If user passed credentials, override
      if(!is.null(user_passed_token)) {

        api_token <- user_passed_token

      }
      if(!is.null(user_passed_datacenter)) {

        data_center <- user_passed_datacenter

      }

      # Set envs
      Sys.setenv("QUALTRICS_API_TOKEN" = api_token)
      Sys.setenv("QUALTRICS_DATA_CENTER" = data_center)

      # Set auth type
      Sys.setenv("QUALTRICS_AUTH_TYPE" = "token")

      # Check
      qualtrics_helper_envs_set("token")

    }

  # Exit

  }

# HELPER FUNCTIONS ----

# Helper function. Retrieves API credentials from keychain
qualtrics_helper_keychain_credentials <- function(type = c("oauth", "token")) {

  type <- match.arg(type)

  # Get OS variable
  os <- Sys.getenv("QUALTRICS_SYS_OS")

  # If empty, call function to retrieve OS
  if(os == "") {

    os <- get_os()

    Sys.setenv("QUALTRICS_SYS_OS" = os)

  }

  if(type == "oauth") {

    client_id <- switch(
      os,
      "osx" = nchar(keyringr::decrypt_kc_pw("qualtrics_api_client")),
      "linux" = nchar(keyringr::decrypt_gk_pw("key qualtrics_api_client"))
    )

    client_secret <- switch(
      os,
      "osx" = nchar(keyringr::decrypt_kc_pw("qualtrics_api_secret")),
      "linux" = nchar(keyringr::decrypt_gk_pw("key qualtrics_api_secret"))
    )

    # Data center
    data_center <- switch(
      os,
      "osx" = nchar(keyringr::decrypt_kc_pw("qualtrics_data_center")),
      "linux" = nchar(keyringr::decrypt_gk_pw("key qualtrics_data_center"))
    )

    # Return
    list(
      "client_id" = client_id,
      "client_secret" = client_secret,
      "data_center" = data_center
    )

  } else if(type == "token") {

    # Api & root url
    api_token <- switch(
      os,
      "osx" = nchar(keyringr::decrypt_kc_pw("qualtrics_api_token")),
      "linux" = nchar(keyringr::decrypt_gk_pw("key qualtrics_api_token"))
    )

    # Data center
    data_center <- switch(
      os,
      "osx" = nchar(keyringr::decrypt_kc_pw("qualtrics_data_center")),
      "linux" = nchar(keyringr::decrypt_gk_pw("key qualtrics_data_center"))
    )

    # Return
    list(
      "token" = api_token,
      "data_center" = data_center
    )

  }

}

# Helper function. Raises warning if one or more environment variables are not set
qualtrics_helper_envs_set <- function(type = c("oauth", "token")) {

  type <- match.arg(type)

  # If token
  if(type == "token") {

    if(Sys.getenv("QUALTRICS_API_TOKEN") == "" & Sys.getenv("QUALTRICS_USE_KEYCHAIN") == FALSE) {

      warning("Qualtrics api token is not registered")

    }

  } else if (type == "oauth") {

    if(Sys.getenv("QUALTRICS_CLIENT_ID") == "" & Sys.getenv("QUALTRICS_USE_KEYCHAIN") == FALSE) {

      warning("Qualtrics client id is not registered")

    }

    if(Sys.getenv("QUALTRICS_CLIENT_SECRET") == "" & Sys.getenv("QUALTRICS_USE_KEYCHAIN") == FALSE) {

      warning("Qualtrics client secret is not registered")

    }

  }

  if(Sys.getenv("QUALTRICS_DATA_CENTER") == "" & Sys.getenv("QUALTRICS_USE_KEYCHAIN") == FALSE) {

    warning("Qualtrics data center is not registered")

  }

}

# Helper function to determine type of OS
# Todo: add windows
get_os <- function(){

  sysinf <- Sys.info()

  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }

  tolower(os)

}

# Helper that fetches a bearer token for user if they use oauth method
qualtrics_set_bearer_token <- function() {

  # body for requests
  body <- list(
    grant_type = "client_credentials"
  )

  # If use keychain
  if(Sys.getenv("QUALTRICS_USE_KEYCHAIN") == "") {

    stop("You must first register your credentials using 'qualtrics_authenticate()'")

  } else if(Sys.getenv("QUALTRICS_USE_KEYCHAIN")) {

    # Retrieve OS
    os <- Sys.getenv("QUALTRICS_SYS_OS")

    # Set data center
    data_center <- switch (

      os,
      osx = keyringr::decrypt_kc_pw("qualtrics_data_center"),
      linux = keyringr::decrypt_gk_pw("key qualtrics_data_center")

    )

    # Set as env. variable
    Sys.setenv("QUALTRICS_DATA_CENTER" = data_center)

    res <- switch(
      os,
      # mac
      osx = httr::POST(paste0("https://",
                              Sys.getenv("QUALTRICS_DATA_CENTER"),
                              ".qualtrics.com/oauth2/token"),
                       encode = "form",
                       body = body,
                       httr::authenticate(keyringr::decrypt_kc_pw("qualtrics_api_client"),
                                          keyringr::decrypt_kc_pw("qualtrics_api_secret"),
                                          type = "basic")),
      # linux
      linux = httr::POST(paste0("https://",
                                Sys.getenv("QUALTRICS_DATA_CENTER"),
                                ".qualtrics.com/oauth2/token"),
                         encode = "form",
                         body = body,
                         httr::authenticate(keyringr::decrypt_gk_pw("key qualtrics_api_client"),
                                            keyringr::decrypt_gk_pw("key qualtrics_api_secret"),
                                            type = "basic"))
    )

  } else { # User does not use keychain

    # Get bearer token
    res <- httr::POST(paste0("https://",
                             Sys.getenv("QUALTRICS_DATA_CENTER"),
                             ".qualtrics.com/oauth2/token"),
                      encode = "form",
                      body = body,
                      httr::authenticate(Sys.getenv("QUALTRICS_CLIENT_ID"),
                                         Sys.getenv("QUALTRICS_CLIENT_SECRET"),
                                         type = "basic"))

  }

  ### CHECK FOR WARNINGS AND OR ERRORS
  httr::warn_for_status(res)

  # Bearer token
  bt <- httr::content(res)$access_token

  # Set as env variable
  Sys.setenv("QUALTRICS_OAUTH_TOKEN" = bt)

}

# Create headers for an API request
qualtrics_create_header <- function() {

  if(Sys.getenv("QUALTRICS_AUTH_TYPE") == "token") {

    # If keychain
    if(Sys.getenv("QUALTRICS_USE_KEYCHAIN")) {

      os <- Sys.getenv("QUALTRICS_SYS_OS")

      # Set data center as env variable
      data_center <- switch(

        os,
        osx = keyringr::decrypt_kc_pw("qualtrics_data_center"),
        linux = keyringr::decrypt_gk_pw("key qualtrics_data_center")

      )

      Sys.setenv("QUALTRICS_DATA_CENTER" = data_center)

      switch(

        os,
        osx = c(
          'X-API-TOKEN' = keyringr::decrypt_kc_pw("qualtrics_api_token"),
          'Content-Type' = "application/json",
          'Accept' = '*/*',
          'accept-encoding' = 'gzip, deflate'
        ),
        linux = c(
          'X-API-TOKEN' = keyringr::decrypt_gk_pw("key qualtrics_api_token"),
          'Content-Type' = "application/json",
          'Accept' = '*/*',
          'accept-encoding' = 'gzip, deflate'
        )
      )

    } else { # not use keychain

      c(
        'X-API-TOKEN' = Sys.getenv("QUALTRICS_API_TOKEN"),
        'Content-Type' = "application/json",
        'Accept' = '*/*',
        'accept-encoding' = 'gzip, deflate'
      )

    }

  } else {

    c(
      'authorization' = paste("bearer",
                              Sys.getenv("QUALTRICS_OAUTH_TOKEN")),
      'Content-Type' = "application/json",
      'Accept' = '*/*',
      'accept-encoding' = 'gzip, deflate'
    )

  }

}

# Helper function that communicates a request to qualtrics API
qualtrics_handle_request <- function(verb = c("GET", "POST"),
                                     endpoint,
                                     body = NULL,
                                     url = NULL) {
  # Match arg
  verb <- match.arg(verb)

  # If use oauth ...
  if(Sys.getenv("QUALTRICS_AUTH_TYPE") == "oauth") {

    # If empty, set
    if(Sys.getenv("QUALTRICS_OAUTH_TOKEN") == "") {

      # Set bearer
      qualtrics_set_bearer_token()

    }

  }

  # Must set data center
  if(Sys.getenv("QUALTRICS_DATA_CENTER") == "") {

    stop("Data center not found. Did you register your credentials using 'qualtrics_authenticate()'?")

  }

  # if url is NULL ...
  if(is.null(url)) {

    url <- paste0("https://",
                  Sys.getenv("QUALTRICS_DATA_CENTER"),
                  ".qualtrics.com/API/v3/",
                  endpoint)

  }

  # Construct header
  header <- qualtrics_create_header()

  # Send request to qualtrics API
  res <- httr::VERB(verb,
                    url = url,
                    httr::add_headers(
                      header
                    ),
                    body = body)

  # Look for situation where the bearer token is no longer valid
  # this happens after 60 minutes
  # simply register new token and re-send request

  if(httr::http_error(res)) {

    if(httr::status_code(res) == 401) {

      if(httr::has_content(res)) {

        tmp <- httr::content(res)

        if(!is.null(tmp$meta$error$errorCode)) {

          if(tmp$meta$error$errorCode == "AUTH_6.0") {

            # Get new bearer token
            message("Retrieving new bearer token ... ")
            qualtrics_set_bearer_token()

            # Construct header
            header <- qualtrics_create_header()

            # Send request to qualtrics API
            res <- httr::VERB(verb,
                              url = paste0("https://",
                                           Sys.getenv("QUALTRICS_DATA_CENTER"),
                                           ".qualtrics.com/API/v3/",
                                           endpoint),
                              httr::add_headers(
                                header
                              ),
                              body = body)


          }

        }

      }

    }

  }

  #browser()
  # Check if response type is OK
  cnt <- qualtRicsResponseCodes(res)

  # Check if OK
  #if(cnt$OK) {

  # If notice occurs, raise warning
  #w <- checkForWarnings(cnt)
  # return content
  return(cnt$content)

  #}

}

# Checks responses against qualtrics response codes and returns error message.
#
# @param res response from httr::GET
# @param raw if TRUE, add 'raw' flag to httr::content() function.
#
# @author Jasper Ginn

qualtRicsResponseCodes <- function(res, raw=FALSE) {
  # Check status code and raise error/warning
  if(res$status_code == 200) {
    if(raw) {
      result <- httr::content(res, "raw")
    } else {
      result <- httr::content(res)
    }
    return(list(
      "content" = result,
      "OK" = TRUE
    )
    )
  } else if(res$status_code == 401) {
    stop("Qualtrics API raised an authentication (401) error - you may not have the\nrequired authorization. Please check your API key and root url.") # nolint
  } else if(res$status_code == 400) {
    stop("Qualtrics API raised a bad request (400) error - Please report this on\nhttps://github.com/ropensci/qualtRics/issues") # nolint
  } else if(res$status_code == 404) {
    stop("Qualtrics API complains that the requested resource cannot be found (404 error).\nPlease check if you are using the correct survey ID.") # nolint
  } else if(res$status_code == 500) {
    stop(paste0("Qualtrics API reports an internal server (500) error. Please contact\nQualtrics Support (https://www.qualtrics.com/contact/) and provide the instanceId and errorCode below.", "\n", # nolint
                "\n",
                "instanceId:", " ",
                httr::content(res)$meta$error$instanceId,
                "\n",
                "errorCode: ",
                httr::content(res)$meta$error$errorCode))
    return(list(
      "content" = httr::content(res),
      "OK"= FALSE
    ))
  } else if(res$status_code == 503) {
    stop(paste0("Qualtrics API reports a temporary internal server (500) error. Please\ncontact Qualtrics Support (https://www.qualtrics.com/contact/) with the instanceId and\nerrorCode below or retry your query.", "\n", # nolint
                "\n",
                "instanceId:", " ", httr::content(res)$meta$error$instanceId,
                "\n",
                "errorCode: ", httr::content(res)$meta$error$errorCode))
    return(list(
      "content" = httr::content(res),
      "OK"= FALSE
    )
    )
  } else if(res$status_code == 413) {
    stop("The request body was too large. This can also happen in cases where a\nmultipart/form-data request is malformed.") # nolint
  } else if(res$status_code == 429) {
    stop("You have reached the concurrent request limit.")
  }
}

# Check if httr GET result contains a warning
#
# @param resp object returned by 'qualtRicsResponseCodes()'
#
# @author Jasper Ginn

checkForWarnings <- function(resp) {
  # Raise warning if resp contains notice
  if(!is.null(resp$content$meta)) {
    if(!is.null(resp$content$meta$notice)) {
      warning(resp$content$meta$notice)
    }
  }
  NULL
}
