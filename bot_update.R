
library(telegram.bot)
library(httr)
library(purrr)
library(stringr)
# source("gdrive.R")
source("dalle.R")
source("tiktok_tts.R")
source("gpt3.R")

# library(rvest)
# html1 <- read_html("html/thelatestkate Shop _ Redbubble.html")
# 
# img_links <- html1 %>% 
#   html_nodes("img") %>%
#   html_attr("src") %>% na.omit() %>% 
#   .[str_detect(., "jpg")] %>% 
#   .[str_detect(., "avatar", negate = T)] %>% 
#   unique()
# saveRDS(img_links, file = "data/img_links.rds")

manual_update <- F


# print(Sys.getenv("r_telegram_bot_arnold"))

bot <<- Bot(token = Sys.getenv("r_telegram_bot_arnold"))

updater <<- Updater(token = Sys.getenv("r_telegram_bot_arnold"))

source("polling.R")

# run <- possibly(run, otherwise = "error", quiet = F)

running <<- "yes"

run_it <- function() {
  out <- tryCatch(
    {
      # Just to highlight: if you want to use more than one 
      # R expression in the "try" part then you'll have to 
      # use curly brackets.
      # 'tryCatch()' will return the last evaluated expression 
      # in case the "try" part was completed successfully
      
      print("Initiate Polling")
      
      run()
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
    #   message("Here's the original warning message [top level]:")
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
      # message("Some other message at the end")
    }
  )    
  return(out)
}

# The following will time out after 5 hours
tryCatch( { res <- R.utils::withTimeout({
  
  
  # run()
  # while (running == "error") {
  
  while (running == "yes") {
    running <<- run_it()
  }
    
  # }
    ## Try 2
    if(running == "error"){
      print("Try 2")
      running <<- run_it()
    }
    ## Try 3
    if(running == "error"){
      print("Try 3")
      running <<- run_it()
    }
    ## Try 4
    if(running == "error"){
      print("Try 4")
      running <<- run_it()
    }
    ## Try 5
    if(running == "error"){
      print("Try 5")
      running <<- run_it()
    }
    ## Try 6
    if(running == "error"){
      print("Try 6")
      running <<- run_it()
    }
  
  },
                                timeout = 60*60*4.9) },
          TimeoutException = function(ex) print("Timed out\n"))


updater$stop_polling()

# possibly(run, )

# start_time <- Sys.time()
# 
# seconds_past <- 0
# 
# while(seconds_past < 60*60*4.9){
#   
# current_time <- Sys.time()
# 
# update_list <- bot$get_updates()
# 
# # debugonce(get_update_id)
# 
#   if(length(update_list)!=0){
#     
#     if(length(update_list) == 100){
#       OFFSET <- get_update_id(update_list)
#       
#       update_list <- get_updates(bot, OFFSET)
#       # ww <- update_list[[length(update_list)]]
#       
#       # ww$message$from_user      
#     }
#     
# 
#     
#     
#     
#     
#     update_dat <- data.frame()
#     
#     for (.x in update_list) {
#       
#       the_text <- .x$message$text
#       the_date <- .x$message$date
#       the_chat_id <- .x$message$chat_id
#       the_user_id <- .x$message$from_user
#       the_message_id <- .x$message$message_id
#       
#       if(is.null(the_text)){
#         the_text <- NA
#       }
#       if(is.null(the_date)){
#         the_date <- NA
#       }
#       
#       # print(c(the_text, the_date))
#       
#       if(is.na(the_text) & is.na(the_date)){
#         NULL 
#       } else {
#         each_row <- data.frame(text = the_text,
#                                date = the_date,
#                                chat_id = the_chat_id,
#                                user_id = the_user_id,
#                                message_id = the_message_id)      
#         
#         update_dat <- rbind(update_dat, each_row)
#       }  
#       
#     }
#     
#     # if()
#     
#     
#     # write.csv(update_dat %>% slice(1:3) %>% mutate(action = ""), file = "data/update_dat.csv")
#     
#     
#     already_done <- read.csv("data/update_dat.csv")
#     
#     new_updates <- setdiff(update_dat$date, already_done$date)
#     
#     update_dat <- na.omit(update_dat[update_dat$date %in% new_updates,])
#     
#     update_dat <- update_dat[update_dat$date!=1652122844,]
#     
#     update_dat <- update_dat[stringr::str_detect(update_dat$text, "That's great, bebi!", negate = T),]
#     
#     
#     if(nrow(update_dat)!=0){
#       
#       iterate_l <- split(update_dat, 1:nrow(update_dat))
#       
#       # .x <- iterate_l[[1]]
#       
#       for (.x in iterate_l) {
#         
#         try(bot_action(bot, .x, img_links, manual_update, data_dat))
# 
#       }
#       
#     }
#     
#     
#   }
# 
#   seconds_past <- as.numeric(current_time) - as.numeric(start_time)  
# 
#   Sys.sleep(5)
# 
# }

# debugonce(bot_action)
# # 
# bot_action(bot, iterate_l[[2]], img_links, manual_update, data_dat)
# 
# 
# debugonce(wombo_start)

