# split the transcript JSON file by participant for handing out 
# the data to participants for checking

library(tidyverse)
library(jsonlite)
library(fs)

# load JSON
df_all <- 
  fromJSON("output/transcript_codes.json") %>% 
  select(Coder, `Participant ID`, Time, Person, `Formatted Transcript`)


# remove formats
remove_format_transcript <- function(transcript_df) {
  df_sub <- tibble(transcript_df)
  if (ncol(df_sub) == 3) {
    df_sub$bold <-  NA
    df_sub$color <-  NA
  }
  df_sub
}

df_unformatted <- 
  df_all %>% 
  mutate(`Formatted Transcript` = map(`Formatted Transcript`, remove_format_transcript)) %>% 
  mutate(Coder = case_match(Coder,
                             "Chat" ~ "Coder 1",
                             "Nacho" ~ "Coder 2"))


# save by participant
participants <- 
  df_all %>% 
  pull(`Participant ID`) %>% 
  unique()

for (pid in participants) {
  df_individual <- 
    df_unformatted %>% 
    mutate(`Formatted Transcript` = if_else(`Participant ID` == pid,
                                            `Formatted Transcript`,
                                            map(`Formatted Transcript`, \(transcript_df) tibble(text = "-"))))
  
  path_output <- dir_create(paste0("output/JSON by participant/", pid))
  
  dir_copy("/Users/chat/local_git/glass-scam/for_participant/Transcript of your interview", path_output)
  
  df_individual %>% 
    toJSON(pretty = TRUE, null = "null") %>%
    str_replace_all(fixed(": null"), fixed(": []")) %>%
    str_c("transcript_codes = ", .) %>% 
    write_file(paste0(path_output, "/Transcript of your interview/code-search_files/transcript_codes.js"))
}

