# extract own code from the merged code file to a separate file

library(tidyverse)

path_agreed <- "../../04 Transcripts/Chat/glass-codebook/codebook-agreed.txt"
path_own <- "../../04 Transcripts/Chat/glass-codebook/codebook-CW.txt"
path_merged <- "../../04 Transcripts/Chat/glass-codebook/CW+Agreed.txt"

# read the code files
agreed_codes <- read_lines(path_agreed)
merged_codes <- read_lines(path_merged)
own_codes <- setdiff(merged_codes, agreed_codes)

# write output
own_codes %>% 
  paste(collapse = "\n") %>% 
  write_file(path_own)
