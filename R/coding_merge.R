# generate a merged code file to use while coding

library(tidyverse)

path_agreed <- "../../04 Transcripts/Chat/glass-codebook/codebook-agreed.txt"
path_own <- "../../04 Transcripts/Chat/glass-codebook/codebook-CW.txt"
path_output <- "../../04 Transcripts/Chat/glass-codebook/CW+Agreed.txt"

# backup the output file if exist
if (file.exists(path_output)) {
  file.copy(path_output, paste0(path_output, format(Sys.time(), " %y%m%d %H%M")))
}



# read the code files
agreed_codes <- read_lines(path_agreed)
own_codes <- read_lines(path_own)

# merge
codes_df <- 
  bind_rows(
    tibble(code = own_codes),
    tibble(code = agreed_codes)) %>% 
  arrange(code)

# write output
codes_df %>% 
  pull(code) %>% 
  paste(collapse = "\n") %>% 
  write_file(path_output)
