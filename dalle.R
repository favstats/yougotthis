library(httr)
library(base64enc)
library(snakecase)
library(purrr)
library(jpeg)

paint_dalle <- function(prmpt, folder = here::here(), combine = T, delete = T) {
  
  
  n_images_ <- 1
  
  url <- "https://bf.dallemini.ai/generate"
  
  heads_up <- add_headers(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:93.0) Gecko/20100101 Firefox/93.0",
                          Accept = 'application/json',
                          `Accept-Language` = 'en-US,en;q=0.5',
                          `Accept-Encoding` = "gzip, deflate, br",
                          `Content-Type` = "application/json",
                          Connection = "keep-alive"
                          )
  
  print("send request")
  
  posted = POST(url, heads_up, body = list(prompt = prmpt), encode = "json")
  
  # stop("i am an error")
  
  
  print("retrieve images")
  
  des <- map_chr( content(posted)$images, ~{
      imgs <- download_img(.x, prmpt, folder, n_images_)
      n_images_ <<- n_images_+ 1
      return(imgs)
      })
  
  
  if(combine){
    print("combine it")
    imgs <- des %>% 
      map(readJPEG)
    
    
    combined_pic <- paste0(folder, snakecase::to_snake_case(prmpt), ".png")
    
    png(file=combined_pic, width=700, height=700)
    
    par(mfrow=c(3,3), mar=c(0, 0, 0, 0))
    for(i in 1:9){
      plot.new()
      plot.window(xlim=c(0, 1), ylim=c(0, 1), asp=NA)
      rasterImage(imgs[[i]], 0, 0, 1, 1)
    }
    
    
    print("save pls")
    # dev.print(png, combined_pic, width = 700, height = 700)    
    
    dev.off()
    print("saved")
    
    return(combined_pic)
  }
  
    
}


download_img <- function(x, prompt, folder, n_images_){
  
  rand_num <- round(runif(1, 0, 10000000))
  temp_bin <- paste0(rand_num, ".bin")
  
  des <- paste0(folder, snakecase::to_snake_case(prompt), "_", n_images_, ".jpeg")
  
  conn <- file(temp_bin,"wb")
  writeBin(x, conn)
  close(conn)
  
  inconn <- file(temp_bin,"rb")
  outconn <- file(des, "wb")
  base64enc::base64decode(what=inconn, output=outconn)
  close(inconn)
  close(outconn)  
  
  file.remove(temp_bin)
  
  return(des)
  
  # n_images_ <<- n_images_ + 1
}



# paint_dalle("Elizabeth II as Steam Punk", "dalle/")


paint_dalle_it <- function(prmpt, folder = here::here(), combine = T, delete = T) {
  out <- tryCatch(
    {
      # Just to highlight: if you want to use more than one 
      # R expression in the "try" part then you'll have to 
      # use curly brackets.
      # 'tryCatch()' will return the last evaluated expression 
      # in case the "try" part was completed successfully
      
      # message("This is the 'try' part")
      
      paint_dalle(prmpt, folder, combine, delete)
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
      # message("Some other message at the end")
    }
  )    
  return(out)
}
