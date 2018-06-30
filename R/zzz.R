# Loads credentials automatically when package is loaded

# On load
.onLoad <- function(libname = find.package("qualtricsToolkit"), pkgname="qualtricsToolkit") {

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

  # Get OS
  Sys.setenv("QTOOLKIT_SYS_OS" = get_os())

}

# On unload
.onUnload <- function(libname = find.package("qualtricsToolkit"), pkgname="qualtricsToolkit") {

  # If user unloads/detaches package make sure that these values are erased
  Sys.setenv("QTOOLKIT_ROOT_URL" = "")
  Sys.setenv("QTOOLKIT_API_KEY" = "")

}
