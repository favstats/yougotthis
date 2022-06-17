
library(httr)
library(base64enc)
library(snakecase)
library(purrr)
library(jpeg)
library(stringr)
library(dplyr)
library(phonfieldwork)

# the_text <- "Hello bebi I know you are tired but know you are loved! I am also very proud of my bebi that you presented your first paper at a conference, hurray for the bebi!"

# the_text <- "hello"

concatenate_soundfiles <- possibly(concatenate_soundfiles, otherwise = "error", quiet = F)

tiktok_tts <- function(prmpt, speaker) {
  
  the_text <- prmpt %>% 
    tibble::tibble(text = .) %>% 
    tidytext::unnest_tokens(sentences, text, token = "sentences", to_lower = F) %>% 
    pull(sentences)
  
  
  the_text <- utils::URLencode(the_text)
  
  n_sentences <<- 1
  
  deses <- the_text %>% 
    map_chr(~get_sounds(.x, speaker,  rlang::hash(the_text[1])))
  
  print("combine it")
  
  print(dir("tts"))
  
  concatenate_soundfiles(path = paste0(here::here(), "/tts/", deses[1]), result_file_name = deses[1], annotation = NULL)
  
  print("combined")
  
  # file.remove()
  
  return(paste0("tts/", deses[1], "/", deses[1], ".wav"))
}


get_sounds <- function(t, speaker, hashs) {
  
  the_text <- utils::URLencode(t)
  
  url <- glue::glue("https://api16-normal-useast5.us.tiktokv.com/media/api/text/speech/invoke/?text_speaker={speaker}&req_text={the_text}&speaker_map_type=0")
  
  posted = POST(url = url)
  
  des <- download_sound(content(posted)$data$v_str, paste0("tts/", hashs), hashs)
  
  return(hashs)
  
}




download_sound <- function(x, folder, hashs){
  
  
  rand_num <- round(runif(1, 0, 10000000))
  temp_bin <- paste0(rand_num, ".bin")
  # 
  dir.create(folder)
  
  
  des <- paste0(folder, "/", hashs, "_", ifelse(str_count(n_sentences)==1, paste0(0, n_sentences), n_sentences), ".mp3")
  
  conn <- file(temp_bin,"wb")
  writeBin(x, conn)
  close(conn)
  
  inconn <- file(temp_bin,"rb")
  outconn <- file(des, "wb")
  base64enc::base64decode(what=inconn, output=outconn)
  close(inconn)
  close(outconn)  
  
  file.remove(temp_bin)
  
  n_sentences <<- n_sentences + 1
  
  des <- paste0(folder, "/", hashs, "_", ifelse(str_count(n_sentences)==1, paste0(0, n_sentences), n_sentences), ".mp3")
  
  
  file.copy("tts/1sec.mp3", des)
  
  
  n_sentences <<- n_sentences + 1
  
  return(des)
  
}

# yo <- 'Saul Goodman was having a bad day. First, he was nearly killed by a giant Titan, then he lost his lunch to a fire-breathing dragon. On top of that, he was pretty sure he was going to be fired from his job as a lawyer for the local lord. All in all, it was shaping up to be a really terrible day.  As Saul moped around his office, he heard a commotion outside. He peeked out the window to see a giant Titan shambling down the street, smashing everything in its path. Saul knew that he didnt stand a chance against the Titan, so he quickly grabbed his things and ran out the back door.  He didnt have time to worry about the Titans or dragons or anything else right now. He had to get away from here before he got killed. Saul ran as fast as he could, not looking back until he was safe inside an abandoned building.  Panting and sweating, Saul leaned against a wall and tried to catch his breath. That was when he heard someone crying. He followed the sound until he came to a small girl huddled in a corner.  "Dont worry," Saul said as he picked her up. "Ill keep you safe."  And with that, Saul Goodman became the unlikely hero of the day. He managed to keep the girl safe from the Titans and dragons while everyone else fled in terror. When the lords and ladies returned to their city, they found Saul Goodman waiting for them with a brave smile on his face.'

# final_des <- tiktok_tts(yo, "en_male_narration")




# names(voices[which(voices == "Ghost Face")])

voices <<- c(
  # DISNEY VOICES
  'en_us_ghostface' =          "Ghost Face",
  'en_us_chewbacca'       =  "Chewbacca",
  'en_us_c3po'        =  "C3PO",
  'en_us_stitch'          =  "Stitch",
  'en_us_stormtrooper'     =  "Stormtrooper",
  'en_us_rocket'           =  "Rocket",
  
  # ENGLISH VOICES
  'en_au_001'             =  "English AU - Female",
  'en_au_002'             =  "English AU - Male",
  'en_uk_001'             =  "English UK - Male 1",
  'en_uk_003'             =  "English UK - Male 2",
  'en_us_001'             =  "English US - Female (Int. 1)",
  'en_us_002'             =  "English US - Female (Int. 2)",
  'en_us_006'             =  "English US - Male 1",
  'en_us_007'             =  "English US - Male 2",
  'en_us_009'             =  "English US - Male 3",
  'en_us_010'             =  "English US - Male 4",
  
  # EUROPE VOICES
  'fr_001'                 =  "French - Male 1",
  'fr_002'                 =  "French - Male 2",
  'de_001'                 =  "German - Female",
  'de_002'                 =  "German - Male",
  'es_002'                 =  "Spanish - Male",
  
  # # AMERICA VOICES
  # 'es_mx_002',              =  "Spanish MX - Male"
  # 'br_001',                 =  "Portuguese BR - Female 1"
  # 'br_003',                 =  "Portuguese BR - Female 2"
  # 'br_004',                 =  "Portuguese BR - Female 3"
  # 'br_005',                 =  "Portuguese BR - Male"
  # 
  # # ASIA VOICES
  # 'id_001',                 =  "Indonesian - Female",
  # 'jp_001',                 =  "Japanese - Female 1",
  # 'jp_003',                 =  "Japanese - Female 2",
  # 'jp_005',                 =  "Japanese - Female 3",
  # 'jp_006',                 =  "Japanese - Male",
  # 'kr_002',                 =  "Korean - Male 1",
  # 'kr_003',                 =  "Korean - Female",
  # 'kr_004',                 =  "Korean - Male 2",
  
  # NARRATOR
  'en_male_narration' = "Narrator Voice",
  
  # SINGING VOICES
  'en_female_f08_salut_damour' =  "Singing Female",
  'en_male_m03_lobby' = "Singing Male"
)
