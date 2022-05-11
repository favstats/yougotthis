
library(telegram.bot)
library(httr)

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

source("utils.R")

img_links <- readRDS("data/img_links.rds")

# print(Sys.getenv("r_telegram_bot_arnold"))

bot <- Bot(token = Sys.getenv("r_telegram_bot_arnold"))


start_time <- Sys.time()

seconds_past <- 0

while(seconds_past < 60*60*4.99){
  
current_time <- Sys.time()

update_list <- bot$get_updates()

# debugonce(get_update_id)

OFFSET <- get_update_id(update_list)

update_list <- get_updates(bot, OFFSET)
# ww <- update_list[[length(update_list)]]

# ww$message$from_user


  

update_dat <- data.frame()

for (.x in update_list) {
  
  the_text <- .x$message$text
  the_date <- .x$message$date
  the_chat_id <- .x$message$chat_id
  the_user_id <- .x$message$from_user
  the_message_id <- .x$message$message_id
  
  if(is.null(the_text)){
    the_text <- NA
  }
  if(is.null(the_date)){
    the_date <- NA
  }
  
  # print(c(the_text, the_date))
  
  if(is.na(the_text) & is.na(the_date)){
    NULL 
  } else {
    each_row <- data.frame(text = the_text,
                           date = the_date,
                           chat_id = the_chat_id,
                           user_id = the_user_id,
                           message_id = the_message_id)      
    
    update_dat <- rbind(update_dat, each_row)
  }  
  
}



# write.csv(update_dat %>% slice(1:3) %>% mutate(action = ""), file = "data/update_dat.csv")

  
already_done <- read.csv("data/update_dat.csv")

new_updates <- setdiff(update_dat$date, already_done$date)

update_dat <- na.omit(update_dat[update_dat$date %in% new_updates,])
  
update_dat <- update_dat[update_dat$date!=1652122844,]
  


if(nrow(update_dat)!=0){
  
  iterate_l <- split(update_dat, 1:nrow(update_dat))
  
  # .x <- iterate_l[[1]]
  
  for (.x in iterate_l) {
    
    try(bot_action(bot, .x, img_links, manual_update))
  
  }
  
}

seconds_past <- as.numeric(current_time) - as.numeric(start_time) 

Sys.sleep(5)

}



