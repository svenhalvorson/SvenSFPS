# Let's see if we can create some function that makes it look like we're working
# when we're not

pretend_working <- function(minutes = 5){
  library(rvest)
  start_time = Sys.time()

  # So we want some other functions here like a loading bar

  while(difftime(time1 = Sys.time(), time2 = start_time, units = "mins") < minutes){
    func <- sample(x = c("bar", "flashy", "wiki", "stack", "headlines", "message"), size = 1, prob = c(0.2, 0.2, 0.2, 0.1, 0.1, 0.2))

    switch(EXPR = func,
           bar = progress_bar(),
           flashy = flashy_bar(),
           wiki = rand_wiki(),
           stack = rand_stack(),
           message = buncha_messages(),
           headlines = headlines())

    cat("\n\n\n\n")

  }


}
