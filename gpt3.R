gpt3_fun <- function(myurl, gpt_prompt, apikey) {
  
  output <- httr::POST(myurl,
                       body = gpt_prompt,
                       add_headers(Authorization = paste("Bearer", apikey)),
                       encode = "json")
 
  
  return(output) 
}



gpt3_fun_it <- function(myurl, gpt_prompt, apikey) {
  out <- tryCatch(
    {
      # Just to highlight: if you want to use more than one 
      # R expression in the "try" part then you'll have to 
      # use curly brackets.
      # 'tryCatch()' will return the last evaluated expression 
      # in case the "try" part was completed successfully
      
      # message("This is the 'try' part")
      
      gpt3_fun(myurl, gpt_prompt, apikey)
      # The return value of `readLines()` is the actual value 
      # that will be returned in case there is no condition 
      # (e.g. warning or error). 
      # You don't need to state the return value via `return()` as code 
      # in the "try" part is not wrapped inside a function (unlike that
      # for the condition handlers for warnings and error below)
    },
    error=function(cond) {
      # message(paste("URL does not seem to exist:", url))
      message("Here's the original error message:")
      print(as.character(cond))
      
      bot$send_message(1009844052, as.character(cond))
      
      # Choose a return value in case of error
      return("error")
    },
    # warning=function(cond) {
    #   # message(paste("URL caused a warning:", url))
    #   message("Here's the original warning message [tiktok]:")
    #   print(cond)
    #   # Choose a return value in case of warning
    #   return(NULL)
    # },
    finally={
      # NOTE:
      # Here goes everything that should be executed at the end,
      # regardless of success or error.
      # If you want more than one expression to be executed, then you 
      # need to wrap them in curly brackets ({...}); otherwise you could
      # just have written 'finally=<expression>' 
      # message(paste("Processed URL:", url))
      print("Run finished")
    }
  )    
  return(out)
}
