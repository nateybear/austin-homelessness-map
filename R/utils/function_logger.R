withLogging <- function(f) {
  function_name <- deparse(substitute(f))
  
  # create a log file
  log_file <- glue("logs/{function_name}.log")
  if (!file.exists(log_file)) {
    file.create(log_file)
  }
  
  
  logger <- getLogger(function_name)
  addHandler(writeToFile, logger = logger, file = log_file)
  
  
  # secret sauce: use condition handling in rlang to divert
  # messages and warnings to our logger
  handle_with <- function(log_fun) {
    calling(function(cond) {
      cond %>% cnd_message() %>% log_fun(logger = logger)
      cnd_muffle(cond)
    })
  }
  
  # return a function, but now decorated to add logging
  function(...) {
    with_handlers(
      f(...),
      message = handle_with(loginfo),
      warning = handle_with(logwarn)
    )
  }
}