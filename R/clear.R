# Just a tiny function to tydy up the workspace since I seem to end up doing this a lot

clear <- function(env = globalenv()){

  # Clear the memory of the environment selected, defaults to global

  rm(list = ls(envir = env), envir = env)

  # Clear the console. This doesn't actually remove the old text, just makes it scroll
  # down a bunch
  cat("\014")

}
