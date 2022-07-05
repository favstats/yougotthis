
opts <<- c("Synthwave" = 1,
          "Ukiyoe" = 2,
          "No Style" = 3,
          "Steampunk" = 4,
          "Fantasy Art" = 5,
          "Vibrant" = 6,
          "HD" = 7,
          "Pastel" = 8,
          "Psychic" = 9,
          "Dark Fantasy" = 10,
          "Mystical" = 11,
          "Festive" = 12,
          "Baroque" = 13,
          "Etching" = 14,
          "S.Dali" = 15,
          "Wuhtercuhler" = 16,
          "Provenance" = 17,
          "Rose Gold" = 18,
          "Moonwalker" = 19,
          "Blacklight" = 20,
          "Psychedelic" = 21,
          "Ghibli" = 22,
          "Surreal" = 23,
          "Love" = 24,
          "Death" = 25,
          "Robots" = 26,
          "Radioactive" = 27
)


bot_action <- function(bot, update_dat, img_links, manual_update, data_dat) {
  if(startsWith(update_dat$text, "/send_image ") | startsWith(update_dat$text, "/send_motivation ")){
    print("send motivation")
    
    img_list <- readLines("data/img_list.txt")
    img_list <- img_list[img_list != ""]
    
    the_images <- as.character(na.omit(setdiff(img_links, img_list)))
    
    img_to_sent <- sample(the_images, 1)
    
    save_dat <- update_dat
    
    save_dat$action <- img_to_sent
    
    write.table(save_dat, file = "data/update_dat.csv", append = T, sep = ",", col.names=F)
    
    writeLines(as.character(as.numeric(readLines("data/img_counter.txt")) + 1), "data/img_counter.txt")
    
    cat(img_to_sent, file = "data/img_list.txt", sep = "\n", append = T)
    
    data_dat_save <- data_dat[data_dat$local %in% c("data/img_list.txt", "data/update_dat.csv"),]
    
    walk2(data_dat_save$g_id, data_dat_save$local,
         ~ drive_update(file = .x, media = .y))
    
    if(!manual_update){
      bot$send_photo(update_dat$chat_id, img_to_sent, reply_to_message_id = update_dat$message_id)
    }
    
  } else if (startsWith(update_dat$text, "/reset ")){
    
    print("reset")
    
    if(!manual_update){
      bot$send_message(update_dat$chat_id, "Roger that. Reset all images.", reply_to_message_id = update_dat$message_id)
    }      
    
    save_dat <- update_dat
    
    save_dat$action <- "done"
    
    
    write.table(save_dat, file = "data/update_dat.csv", append = T, sep = ",", col.names=F)
    
    writeLines("0", "img_counter.txt")
    
    writeLines("", "img_list.txt")
    
    data_dat_save <- data_dat[!(data_dat$local %in% c("data/img_links.rds")),]
    
    walk2(data_dat_save$g_id, data_dat_save$local,
          ~ drive_update(file = .x, media = .y))
    
    
  } else if (startsWith(update_dat$text, "/progress")){
    
    print("image progress")
    
    how_many <- as.numeric(readLines("data/img_counter.txt"))
    that_many <- length(img_links) - how_many

    save_dat <- update_dat
    
    save_dat$action <- "done"
    
    write.table(save_dat, file = "data/update_dat.csv", append = T, sep = ",", col.names=F)
    
    data_dat_save <- data_dat[data_dat$local == "data/update_dat.csv",]
    
    walk2(data_dat_save$g_id, data_dat_save$local,
          ~ drive_update(file = .x, media = .y))
        
    if(!manual_update){
      bot$send_message(update_dat$chat_id, paste0("You already saw ", how_many, " images. There are ", that_many, " images left."), reply_to_message_id = update_dat$message_id)
    }

    
  } else if (startsWith(update_dat$text, "/gpt3 ") | startsWith(update_dat$text, "/hey_arnold ")){
    
    print("gpt3")
    
    # update_dat$text <- "can you tell us a story about the bees and the birds? [2500]"
    
    the_prompt <- gsub("/gpt3 ", "", update_dat$text)
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
    
    output <- gpt3_fun_it(myurl, gpt_prompt, apikey)
    
    
    
    message_to_sent <- content(output)$choices[[1]]$text
    
    print(message_to_sent)
    
    save_dat <- update_dat
    
    save_dat$action <- "done"
    
    write.table(save_dat, file = "data/update_dat.csv", append = T, sep = ",", col.names=F)
    
    data_dat_save <- data_dat[data_dat$local == "data/update_dat.csv",]
    
    walk2(data_dat_save$g_id, data_dat_save$local,
          ~ drive_update(file = .x, media = .y))
    
    if(!manual_update){
      bot$send_message(update_dat$chat_id, message_to_sent, reply_to_message_id = update_dat$message_id)
    }
    

    
  } else if (startsWith(update_dat$text, "/image_prompt ")){

    print("wombo")
    
    # the_prompt <- "A giant flying Walrus [Psychedelic]"
    the_prompt <- gsub("/image_prompt ", "", update_dat$text)
    
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
    
    save_dat <- update_dat
    
    save_dat$action <- "done"
    
    write.table(save_dat, file = "data/update_dat.csv", append = T, sep = ",", col.names=F)
    
    data_dat_save <- data_dat[data_dat$local == "data/update_dat.csv",]
    
    walk2(data_dat_save$g_id, data_dat_save$local,
          ~ drive_update(file = .x, media = .y))
    
    if(!manual_update){
      bot$send_photo(update_dat$chat_id, des, caption = glue::glue("{the_prompt}. Style: {style}."), reply_to_message_id = update_dat$message_id)
    }
    
    
  } else if (startsWith(update_dat$text, "/image_options")){
    print("wombo options")
    
    save_dat <- update_dat
    
    save_dat$action <- "done"
    
    write.table(save_dat, file = "data/update_dat.csv", append = T, sep = ",", col.names=F)
    
    data_dat_save <- data_dat[data_dat$local == "data/update_dat.csv",]
    
    walk2(data_dat_save$g_id, data_dat_save$local,
          ~ drive_update(file = .x, media = .y))
    
    if(!manual_update){
      bot$send_photo(update_dat$chat_id, "img/options.png", caption = glue::glue("Following options are available"), reply_to_message_id = update_dat$message_id)
    }
    
  }
      
}



wombo_start <- function(pro, sty) {
  
  opts <- c("Synthwave" = 1,
             "Ukiyoe" = 2,
             "No Style" = 3,
             "Steampunk" = 4,
             "Fantasy Art" = 5,
             "Vibrant" = 6,
             "HD" = 7,
             "Pastel" = 8,
             "Psychic" = 9,
             "Dark Fantasy" = 10,
             "Mystical" = 11,
             "Festive" = 12,
             "Baroque" = 13,
             "Etching" = 14,
             "S.Dali" = 15,
             "Wuhtercuhler" = 16,
             "Provenance" = 17,
             "Rose Gold" = 18,
             "Moonwalker" = 19,
             "Blacklight" = 20,
             "Psychedelic" = 21,
             "Ghibli" = 22,
             "Surreal" = 23,
             "Love" = 24,
             "Death" = 25,
             "Robots" = 26,
             "Radioactive" = 27
  )
  
  heads_up <- add_headers(`accept-encoding` = "gzip",
                          Authorization = glue::glue("Bearer {get_token()}"),
                          connection = "keep-alive",
                          `content-type` = "application/json; charset=utf-8",
                          host = "paint.api.wombo.ai",
                          `user-agent` = "okhttp/3.14.9")
  
  
  url <- "https://paint.api.wombo.ai/api/tasks/"
  
  posted = POST(url, heads_up, body = list(premium = "false"), encode = "json")
  
  task_id <-content(posted)$id
  
  # content(posted)
  # print(sty)
  # print(opts[sty])
  # print(as.character(opts[sty]))
  
  # pro = "your mom"
  # sty <- "Death"
  
  print(pro)
  
  first_request_payload <- list(input_spec = list(prompt = pro, style = as.numeric(opts[sty])))
  
  PUT(url=paste0(url, task_id) , heads_up,
      body = first_request_payload, encode = "json")
  
  
  Sys.sleep(1.5)
  
  task_state <- content(GET(url=paste0(url, task_id), heads_up))$state
  
  

  
  destination <- glue::glue("img/{snakecase::to_snake_case(as.character(Sys.time()))}_{snakecase::to_snake_case(pro)}_{snakecase::to_snake_case(sty)}.png")
  
  
  if(task_state == "completed"){
    image_url <- content(GET(url=paste0(url, task_id), heads_up))$result$final
    
    download.file(image_url, destfile = destination, mode = 'wb', quiet  = T)
  } else if (task_state %in% c("generating")) {
    Sys.sleep(10)
    
    image_url <-content(GET(url=paste0(url, task_id), heads_up))$result$final
    
    print(image_url)
    
    download.file(image_url, destfile = destination, mode = 'wb', quiet  = T)
  } else if (task_state %in% c("pending")){
    
    Sys.sleep(20)
    
    image_url <-content(GET(url=paste0(url, task_id), heads_up))$result$final
    
    if(is.null(image_url)){
      posted = POST(url, heads_up, body = list(premium = "false"), encode = "json")
      
      task_id <-content(posted)$id
      
      print(pro)
      
      first_request_payload <- list(input_spec = list(prompt = pro, style = as.numeric(opts[sty])))
      
      PUT(url=paste0(url, task_id) , heads_up,
          body = first_request_payload, encode = "json")
      
      Sys.sleep(10)
      
      image_url <-content(GET(url=paste0(url, task_id), heads_up))$result$final
      
      print(image_url)
      
      download.file(image_url, destfile = destination, mode = 'wb', quiet  = T)
    }
    
    print(image_url)
    
    if(!is.null(image_url)){
      download.file(image_url, destfile = destination, mode = 'wb', quiet  = T)
    }
    
    
    
  }
  
  
  return(destination)
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



get_token <- function() {

  x_goog_api_key = "AIzaSyDxCoSRCFvdsYcJalNfBQQfGl0-YycRkdE"
  url = glue::glue("https://www.googleapis.com/identitytoolkit/v3/relyingparty/signupNewUser?key={x_goog_api_key}")

  heads_up <- add_headers(`accept-encoding` = "gzip",
                          `accept-language` = "en-DE, en-US",
                          connection = "keep-alive",
                          `content-encoding` = "gzip",
                          `content-type` = "application/json",
                          host = "www.googleapis.com",
                          `user-agent` = "Dalvik/2.1.0 (Linux; U; Android 12; POCO F1 Build/SD1A.210817.036)",
                          `x-android-package` = "com.womboai.wombodream",
                          `x-android-cert` = "659AA1EACE253B8667AA28414BF5E21ACD798A4D",
                          `x-client-version` = "Android/Fallback/X21000001/FirebaseCore-Android"
                          )

  response <- POST(url, heads_up, json = list())

  conn <- content(response)

  token <- conn$idToken

  return(token)
}

# get_token()


# wombo_start("How are you doing?", sty = "Death")
