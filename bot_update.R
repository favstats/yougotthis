
library(telegram.bot)

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

img_links <- readRDS("data/img_links.rds")

# print(Sys.getenv("r_telegram_bot_arnold"))

bot <- Bot(token = Sys.getenv("r_telegram_bot_arnold"))


start_time <- Sys.time()

seconds_past <- 0

while(seconds_past < 60*60*4.7){
  
current_time <- Sys.time()

update_list <- bot$get_updates()

update_dat <- data.frame()

for (.x in update_list) {
  
  the_text <- .x$message$text
  the_date <- .x$message$date
  the_chat_id <- .x$message$chat_id
  
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
                           chat_id = the_chat_id)      
    
    update_dat <- rbind(update_dat, each_row)
  }  
  
}



# write.csv(update_dat %>% slice(1:3) %>% mutate(action = ""), file = "data/update_dat.csv")

  
already_done <- read.csv("data/update_dat.csv")

new_updates <- setdiff(update_dat$date, already_done$date)

update_dat <- na.omit(update_dat[update_dat$date %in% new_updates,])
  
  
if(nrow(update_dat)!=0){
  
  iterate_l <- split(update_dat, 1:nrow(update_dat))
  
  for (.x in iterate_l) {
    if(.x$text %in% c("/send_image", "/send_motivation")){
      
      img_list <- readLines("img_list.txt")
      img_list <- img_list[img_list != ""]
      
      the_images <- as.character(na.omit(setdiff(img_links, img_list)))
      
      img_to_sent <- sample(the_images, 1)
      
      bot$send_photo(.x$chat_id, img_to_sent)
      
      
      save_dat <- .x
      
      save_dat$action <- img_to_sent
      
      write.table(save_dat, file = "data/update_dat.csv", append = T, sep = ",", col.names=F)
      
      writeLines(as.character(as.numeric(readLines("img_counter.txt")) + 1), "img_counter.txt")

      cat(img_to_sent, file = "img_list.txt", sep = "\n", append = T)
      
            
    } else if (.x$text == "/reset"){
      
      bot$send_message(.x$chat_id, "Roger that. Reset all images.")
      
      save_dat <- .x
      
      save_dat$action <- "done"
      
      write.table(save_dat, file = "data/update_dat.csv", append = T, sep = ",", col.names=F)
      
      writeLines("0", "img_counter.txt")
      
      writeLines("", "img_list.txt")
      
      
    } else if (.x$text == "/progress"){
      
      how_many <- as.numeric(readLines("img_counter.txt"))
      that_many <- length(img_links) - how_many
      
      bot$send_message(.x$chat_id, paste0("You alreay saw ", how_many, " images. There are ", that_many, " images left."))
      
      save_dat <- .x
      
      save_dat$action <- "done"
      
      write.table(save_dat, file = "data/update_dat.csv", append = T, sep = ",", col.names=F)
      
    }    
  }
  
}

seconds_past <- as.numeric(current_time) - as.numeric(start_time) 

Sys.sleep(5)

}