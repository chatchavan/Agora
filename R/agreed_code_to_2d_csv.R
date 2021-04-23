# generate a two-dimensional CSV file from agreed code
# each column contains code with the same prefix
# within each column, each row contains a code
#
# Usage: Open this file in a spreadsheet app to copy-and-paste to Miro as sticky notes

library(tidyverse)

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
# convert codes to 2D tibble
codes_to_2d_df <- function(codes) {
  # load files
  agreed_df <- tibble(code = codes)
  
  # determine the length of longest common prefix
  lcp_df <- 
    agreed_df %>% 
    mutate(code_lag = lag(code, default = "")) %>% 
    rowwise() %>% 
    mutate(lcp = lcPrefix(c(code, code_lag))) %>% 
    ungroup() %>% 
    mutate(lcp_length = str_length(lcp)) 
  
  # determine cut-off of the common prefix length
  # min_lcp_length <- dominate_mode(lcp_df$lcp_length) * 2
  min_lcp_length <- 7 # TODO: hard-coded
  
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
  
  # transform into 2D tibble
  df_2d <- groups_df %>% 
    pivot_wider(names_from = j, values_from = code, values_fill = "")
  
  # return
  df_2d
}


#-------------------------------------------------------------------------------
# open a file in Apple Numbers
open_numbers <- function(file_path) {
  system2("open", c("-a", "Numbers", paste0("\"", normalizePath(file_path), "\"")))
}

#===============================================================================
# entry point (unused; subsumed by "R/agreed_codes_for_miro.R")

# path_agreed <- "../../04 Transcripts/Chat/glass-codebook/codebook-agreed.txt"
# path_output <- "output/agreed_2d.csv"
# read_lines(path_agreed) %>% 
#   codes_to_2d_df() %>% 
#   write_csv(path_output)
# 
# open_numbers(path_output)

