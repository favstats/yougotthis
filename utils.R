
bot_action <- function(bot, update_dat, img_links, manual_update, data_dat) {
  if(startsWith(.x$text, "/send_image") | startsWith(.x$text, "/send_motivation")){
    print("send motivation")
    
    img_list <- readLines("img_list.txt")
    img_list <- img_list[img_list != ""]
    
    the_images <- as.character(na.omit(setdiff(img_links, img_list)))
    
    img_to_sent <- sample(the_images, 1)
    
    if(!manual_update){
      bot$send_photo(.x$chat_id, img_to_sent, reply_to_message_id = .x$message_id)
    }
    
    save_dat <- .x
    
    save_dat$action <- img_to_sent
    
    write.table(save_dat, file = "data/update_dat.csv", append = T, sep = ",", col.names=F)
    
    writeLines(as.character(as.numeric(readLines("img_counter.txt")) + 1), "img_counter.txt")
    
    cat(img_to_sent, file = "data/img_list.txt", sep = "\n", append = T)
    
    data_dat_save <- data_dat[data_dat$local %in% c("data/img_list.txt", "data/update_dat.csv"),]
    
    walk2(data_dat_save$g_id, data_dat_save$local,
         ~ drive_update(file = .x, media = .y))
    
  } else if (startsWith(.x$text, "/reset")){
    
    print("reset")
    
    if(!manual_update){
      bot$send_message(.x$chat_id, "Roger that. Reset all images.", reply_to_message_id = .x$message_id)
    }      
    
    save_dat <- .x
    
    save_dat$action <- "done"
    
    
    write.table(save_dat, file = "data/update_dat.csv", append = T, sep = ",", col.names=F)
    
    writeLines("0", "img_counter.txt")
    
    writeLines("", "img_list.txt")
    
    data_dat_save <- data_dat[!(data_dat$local %in% c("data/img_links.rds")),]
    
    walk2(data_dat_save$g_id, data_dat_save$local,
          ~ drive_update(file = .x, media = .y))
    
    
  } else if (startsWith(.x$text, "/progress")){
    
    print("image progress")
    
    how_many <- as.numeric(readLines("img_counter.txt"))
    that_many <- length(img_links) - how_many
    
    if(!manual_update){
      bot$send_message(.x$chat_id, paste0("You already saw ", how_many, " images. There are ", that_many, " images left."), reply_to_message_id = .x$message_id)
    }
    
    save_dat <- .x
    
    save_dat$action <- "done"
    
    write.table(save_dat, file = "data/update_dat.csv", append = T, sep = ",", col.names=F)
    
    data_dat_save <- data_dat[data_dat$local == "data/update_dat.csv",]
    
    walk2(data_dat_save$g_id, data_dat_save$local,
          ~ drive_update(file = .x, media = .y))
    
  } else if (startsWith(.x$text, "/gpt3") | startsWith(.x$text, "/hey_arnold")){
    
    print("gpt3")
    
    # .x$text <- "can you tell us a story about the bees and the birds? [2500]"
    
    the_prompt <- gsub("/gpt3 ", "", .x$text)
    the_prompt <- gsub("/hey_arnold ", "", the_prompt)
    
    n_tokens <- as.numeric(regmatches(the_prompt, gregexpr( "(?<=\\[).+?(?=\\])", the_prompt, perl = T))[[1]])
    
    if(length(n_tokens)==0) {
      n_tokens <- 250
    } else {
      the_prompt <- gsub(paste0(" \\[", n_tokens, "\\]"), "", the_prompt)
    }
    
    print(the_prompt)
    
    gpt_prompt <- list(
      prompt = the_prompt,
      temperature = 1,
      max_tokens = n_tokens,
      top_p = 1,
      frequency_penalty = 0.5,
      presence_penalty = 0.5
    ) 
    
    myurl <- "https://api.openai.com/v1/engines/text-davinci-002/completions"
    
    apikey <- Sys.getenv("gpt3")
    
    output <- httr::POST(myurl, 
                         body = gpt_prompt, 
                         add_headers(Authorization = paste("Bearer", apikey)), 
                         encode = "json")
    
    
    message_to_sent <- content(output)$choices[[1]]$text
    
    print(message_to_sent)
    
    if(!manual_update){
      bot$send_message(.x$chat_id, message_to_sent, reply_to_message_id = .x$message_id)
    }
    
    save_dat <- .x
    
    save_dat$action <- "done"
    
    write.table(save_dat, file = "data/update_dat.csv", append = T, sep = ",", col.names=F)
    
    data_dat_save <- data_dat[data_dat$local == "data/update_dat.csv",]
    
    walk2(data_dat_save$g_id, data_dat_save$local,
          ~ drive_update(file = .x, media = .y))
    
  }      
}



get_updates <- function(bot, OFFSET) {
  return(bot$get_updates(offset = OFFSET))
}

get_update_id <- function(updates) {
  num_updates = length(updates)
  last_update = num_updates - 1
  update_id = updates[[last_update]]$update_id
  return (update_id)
  
}
