# generate a two-dimensional CSV file from agreed code
# each column contains code with the same prefix
# within each column, each row contains a code
#
# Usage: Open this file in a spreadsheet app to copy-and-paste to Miro as sticky notes

library(tidyverse)

path_agreed <- "../../04 Transcripts/Chat/glass-codebook/codebook-agreed.txt"
path_output <- "output/agreed_2d.csv"

#-------------------------------------------------------------------------------
# code from BioString
lcPrefix <- function (x, ignore.case = FALSE) 
{
  x <- as.character(x)
  if (ignore.case) 
    x <- toupper(x)
  nc <- nchar(x, type = "char")
  for (i in 1:min(nc)) {
    ss <- substr(x, 1, i)
    if (any(ss != ss[1])) {
      return(substr(x[1], 1, i - 1))
    }
  }
  substr(x[1], 1, i)
}
#-------------------------------------------------------------------------------

dominate_mode <- function(x) {
  den <- density(x) 
  plot(den)
  den$x[den$y == max(den$y)]
}

#-------------------------------------------------------------------------------

# load files
agreed_codes <- read_lines(path_agreed)
agreed_df <- tibble(code = agreed_codes)

# determine the length of longest common prefix
lcp_df <- 
  agreed_df %>% 
  mutate(code_lag = lag(code, default = "")) %>% 
  rowwise() %>% 
  mutate(lcp = lcPrefix(c(code, code_lag))) %>% 
  ungroup() %>% 
  mutate(lcp_length = str_length(lcp)) 

# determine cut-off of the common prefix length
min_lcp_length <- dominate_mode(lcp_df$lcp_length) / 2

min_code_per_col <- 3

# group the string based on the threshold
groups_df <-
  lcp_df %>% 
  select(code, lcp_length) %>% 
  
  # determine groups (j) based on prefix
  mutate(is_new_group = if_else(lcp_length < min_lcp_length, 1, 0)) %>% 
  mutate(j = cumsum(is_new_group)) %>% 
  
  # for group with a small number of codes, collapse them into one group
  group_by(j) %>% 
  mutate(group_size = n()) %>% 
  ungroup() %>% 
  mutate(j = if_else(group_size < min_code_per_col, max(j), j)) %>% 
  arrange(j, code) %>% 
  
  # generate row number (i) by group
  group_by(j) %>% 
  mutate(i = row_number(code)) %>% 
  ungroup() %>% 
  
  select(code, i, j)

# transform into 2D and save CSV
groups_df %>% 
  pivot_wider(names_from = j, values_from = code, values_fill = "") %>% 
  write_csv(path_output)


system2("open", c("-a", "Numbers", paste0("\"", normalizePath(path_output), "\"")))
