
run <- function() {
  
  # updater <<- Updater(token = Sys.getenv("r_telegram_bot_arnold"))
  
  # bot <<- Bot(token = Sys.getenv("r_telegram_bot_arnold"))
  
  source("utils.R")
  
  img_links <<- readRDS("img_links.rds")
  
  
  send_motivation <- function(bot, update){
    this_is <<- update
    
    print("send motivation")
    
    img_list <- readLines("data/img_list.txt")
    img_list <- img_list[img_list != ""]
    
    the_images <- as.character(na.omit(setdiff(img_links, img_list)))
    
    img_to_sent <<- sample(the_images, 1)
    
    # save_dat <- update_dat
    # 
    # save_dat$action <- img_to_sent
    # 
    # write.table(save_dat, file = "data/update_dat.csv", append = T, sep = ",", col.names=F)
    
    writeLines(as.character(as.numeric(readLines("data/img_counter.txt")) + 1), "data/img_counter.txt")
    
    cat(img_to_sent, file = "data/img_list.txt", sep = "\n", append = T)
    
    # data_dat_save <- data_dat[data_dat$local %in% c("data/img_list.txt", "data/update_dat.csv"),]
    # 
    # walk2(data_dat_save$g_id, data_dat_save$local,
    #       ~ drive_update(file = .x, media = .y))
    
    bot$send_photo(update$message$chat_id, img_to_sent, reply_to_message_id = update$message$message_id)
  }
  
  updater <<- updater + CommandHandler("send_motivation", send_motivation)
  
  
  # bot$send_photo(update_dat$chat_id, img_to_sent, reply_to_message_id = update_dat$message_id)
  
  
  
  caps <- function(bot, update, args){
    if (length(args > 0L)){
      text_caps <- toupper(paste(args, collapse = " "))
      bot$sendMessage(chat_id = update$message$chat_id,
                      text = text_caps) 
    }
  }
  
  updater <<- updater + CommandHandler("caps", caps, pass_args = TRUE)
  
  
  
  
  
  progress <- function(bot, update){
    this_is <<- update
    
    print("image progress")
    
    how_many <- as.numeric(readLines("data/img_counter.txt"))
    that_many <- length(img_links) - how_many
    
    # save_dat <- update_dat
    
    # save_dat$action <- "done"
    
    # write.table(save_dat, file = "data/update_dat.csv", append = T, sep = ",", col.names=F)
    
    # data_dat_save <- data_dat[data_dat$local == "data/update_dat.csv",]
    
    # walk2(data_dat_save$g_id, data_dat_save$local,
    # ~ drive_update(file = .x, media = .y))
    
    bot$send_message(update$message$chat_id, 
                     paste0("You already saw ", how_many, " images. There are ", that_many, " images left."), 
                     reply_to_message_id = update$message$message_id)
    
  }
  
  updater <<- updater + CommandHandler("progress", progress)
  
  
  
  
  
  hey_arnold <- function(bot, update, args){
    this_is <<- update
    
    print("gpt3")
    
    if (length(args > 0L)){
      the_prompt <- paste(args, collapse = " ")
      
      
      n_tokens <- as.numeric(regmatches(the_prompt, gregexpr( "(?<=\\[).+?(?=\\])", the_prompt, perl = T))[[1]])
      
      if(length(n_tokens)==0) {
        n_tokens <- 250
      } else {
        the_prompt <- gsub(paste0(" \\[", n_tokens, "\\]"), "", the_prompt)
      }
      
      print(the_prompt)
      print(n_tokens)
      
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
      
      # print(message_to_sent)
      
      # save_dat <- update_dat
      
      # save_dat$action <- "done"
      
      # write.table(save_dat, file = "data/update_dat.csv", append = T, sep = ",", col.names=F)
      
      # data_dat_save <- data_dat[data_dat$local == "data/update_dat.csv",]
      
      # walk2(data_dat_save$g_id, data_dat_save$local,
      # ~ drive_update(file = .x, media = .y))
      
      # if(!manual_update){
      bot$send_message(update$message$chat_id, message_to_sent, reply_to_message_id = update$message$message_id)
      # }
    }
    
    # update_dat$text <- "can you tell us a story about the bees and the birds? [2500]"
    
    
  }
  
  updater <<- updater + CommandHandler("hey_arnold", hey_arnold, pass_args = T)
  
  
  
  image_prompt <- function(bot, update, args){
    this_is <<- update
    
    print("wombo")
    
    if (length(args > 0L)){
      # the_prompt <- "A giant flying Walrus [Psychedelic]"
      the_prompt <- paste(args, collapse = " ")
      
      style <- regmatches(the_prompt, gregexpr( "(?<=\\[).+?(?=\\])", the_prompt, perl = T))[[1]]
      
      if(length(style)==0) {
        style <- names(sample(opts, 1))
      } else {
        
        if(!(style %in% names(opts))){
          
          the_prompt <- gsub(paste0(" \\[", style, "\\]"), "", the_prompt)
          
          style <- names(sample(opts, 1))
        } else {
          the_prompt <- gsub(paste0(" \\[", style, "\\]"), "", the_prompt)
        }
        
      }
      
      the_prompt <- substr(the_prompt, 1, 100)   
      
      des <- wombo_start(the_prompt, style)
      
      # save_dat <- update_dat
      
      # save_dat$action <- "done"
      
      # write.table(save_dat, file = "data/update_dat.csv", append = T, sep = ",", col.names=F)
      
      # data_dat_save <- data_dat[data_dat$local == "data/update_dat.csv",]
      
      # walk2(data_dat_save$g_id, data_dat_save$local,
      # ~ drive_update(file = .x, media = .y))
      
      # if(!manual_update){
      bot$send_photo(update$message$chat_id, des, caption = glue::glue("{the_prompt}. Style: {style}."), reply_to_message_id = update$message$message_id)
      # }
    }
    
    # update_dat$text <- "can you tell us a story about the bees and the birds? [2500]"
    
    
  }
  
  updater <<- updater + CommandHandler("image_prompt", image_prompt, pass_args = T)
  
  
  
  
  image_ai <- function(bot, update, args){
    this_is <<- update
    
    print("wombo")
    
    if (length(args > 0L)){
      # the_prompt <- "A giant flying Walrus [Psychedelic]"
      the_prompt <- paste(args, collapse = " ")
      
      style <- regmatches(the_prompt, gregexpr( "(?<=\\[).+?(?=\\])", the_prompt, perl = T))[[1]]
      
      if(length(style)==0) {
        style <- names(sample(opts, 1))
      } else {
        
        if(!(style %in% names(opts))){
          
          the_prompt <- gsub(paste0(" \\[", style, "\\]"), "", the_prompt)
          
          style <- names(sample(opts, 1))
        } else {
          the_prompt <- gsub(paste0(" \\[", style, "\\]"), "", the_prompt)
        }
        
      }
      
      the_prompt <- substr(the_prompt, 1, 100)   
      
      des <- wombo_start(the_prompt, style)
      
      # save_dat <- update_dat
      
      # save_dat$action <- "done"
      
      # write.table(save_dat, file = "data/update_dat.csv", append = T, sep = ",", col.names=F)
      
      # data_dat_save <- data_dat[data_dat$local == "data/update_dat.csv",]
      
      # walk2(data_dat_save$g_id, data_dat_save$local,
      # ~ drive_update(file = .x, media = .y))
      
      # if(!manual_update){
      bot$send_photo(update$message$chat_id, des, caption = glue::glue("{the_prompt}. Style: {style}."), reply_to_message_id = update$message$message_id)
      # }
    }
    
    # update_dat$text <- "can you tell us a story about the bees and the birds? [2500]"
    
    
  }
  
  updater <<- updater + CommandHandler("image_ai", image_ai, pass_args = T)
  
  
  wombo <- function(bot, update, args){
    this_is <<- update
    
    print("wombo")
    
    if (length(args > 0L)){
      # the_prompt <- "A giant flying Walrus [Psychedelic]"
      the_prompt <- paste(args, collapse = " ")
      
      style <- regmatches(the_prompt, gregexpr( "(?<=\\[).+?(?=\\])", the_prompt, perl = T))[[1]]
      
      if(length(style)==0) {
        style <- names(sample(opts, 1))
      } else {
        
        if(!(style %in% names(opts))){
          
          the_prompt <- gsub(paste0(" \\[", style, "\\]"), "", the_prompt)
          
          style <- names(sample(opts, 1))
        } else {
          the_prompt <- gsub(paste0(" \\[", style, "\\]"), "", the_prompt)
        }
        
      }
      
      the_prompt <- substr(the_prompt, 1, 100)   
      
      des <- wombo_start(the_prompt, style)
      
      print(des)
      
      # save_dat <- update_dat
      
      # save_dat$action <- "done"
      
      # write.table(save_dat, file = "data/update_dat.csv", append = T, sep = ",", col.names=F)
      
      # data_dat_save <- data_dat[data_dat$local == "data/update_dat.csv",]
      
      # walk2(data_dat_save$g_id, data_dat_save$local,
      # ~ drive_update(file = .x, media = .y))
      
      # if(!manual_update){
      bot$send_photo(update$message$chat_id, des, caption = glue::glue("{the_prompt}. Style: {style}."), reply_to_message_id = update$message$message_id)
      # }
    }
    
    # update_dat$text <- "can you tell us a story about the bees and the birds? [2500]"
    
    
  }
  
  updater <<- updater + CommandHandler("wombo", wombo, pass_args = T)
  
  
  image_options <- function(bot, update){
    this_is <<- update
    
    print("wombo options")
    
    # save_dat <- update_dat
    
    # save_dat$action <- "done"
    
    # write.table(save_dat, file = "data/update_dat.csv", append = T, sep = ",", col.names=F)
    
    # data_dat_save <- data_dat[data_dat$local == "data/update_dat.csv",]
    
    # walk2(data_dat_save$g_id, data_dat_save$local,
    # ~ drive_update(file = .x, media = .y))
    
    # if(!manual_update){
    bot$send_photo(update$message$chat_id, "img/options.png", caption = glue::glue("Following options are available"), reply_to_message_id = update$message$message_id)
    # }
    
  }
  
  updater <<- updater + CommandHandler("image_options", image_options)
  
  
  
  wombo_opts <- function(bot, update){
    this_is <<- update
    
    print("wombo options")
    
    # save_dat <- update_dat
    
    # save_dat$action <- "done"
    
    # write.table(save_dat, file = "data/update_dat.csv", append = T, sep = ",", col.names=F)
    
    # data_dat_save <- data_dat[data_dat$local == "data/update_dat.csv",]
    
    # walk2(data_dat_save$g_id, data_dat_save$local,
    # ~ drive_update(file = .x, media = .y))
    
    # if(!manual_update){
    bot$send_photo(update$message$chat_id, "img/options.png", caption = glue::glue("Following options are available"), reply_to_message_id = update$message$message_id)
    # }
    
  }
  
  updater <<- updater + CommandHandler("wombo_opts", wombo_opts)
  
  
  
  reset <- function(bot, update){
    this_is <<- update
    
    print("reset")
    
    writeLines("0", "img_counter.txt")
    
    writeLines("", "img_list.txt")
    
    # if(!manual_update){
    bot$send_message(update$message$chat_id, "Roger that. Reset all images.", reply_to_message_id = update$message$message_id)
    # }     
    
  }
  
  updater <<- updater + CommandHandler("reset", reset)
  
  
  
  
  
  
  
  unknown <- function(bot, update){
    bot$sendMessage(chat_id = update$message$chat_id,
                    text = "Sorry, I didn't understand that command.")
  }
  
  updater <<- updater + MessageHandler(unknown, MessageFilters$command)
  
  
  
  on.exit(return("yes"))
  
  updater$start_polling(clean = T)
  
  
}
