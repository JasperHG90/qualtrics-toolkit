# Loads qualtRics credentials automatically when package is loaded
# and ".qualtRics.yml" file is present in working directory. User
# needs to have #qualtRics API key and root url stored in a configuration
# file in working directory. For an example of a configuration file,
# execute "qualtRicsCo#nfigFile()". See:
# https://github.com/ropensci/qualtRics/blob/master/README.md#using-a-configuration-file # nolint


.onLoad <- function(libname = find.package("qualtRics"), pkgname="qualtRics") {

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
  Sys.setenv("QUALTRICS_SYS_OS" = get_os())

  # Trycatch register options
  rop <- tryCatch({
    #qualtrics_register_options()
  }, error = function(e) {
    NULL
  })

}

# On unload
.onUnload <- function(libname = find.package("qualtRics"), pkgname="qualtRics") {

  # If user unloads/detaches package make sure that these values are erased
  Sys.setenv("QUALTRICS_ROOT_URL" = "")
  Sys.setenv("QUALTRICS_API_KEY" = "")

}
