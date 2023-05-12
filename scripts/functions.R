# function to read specific lines from text file

library(readr)
library(glue)
library(here)

#pathrun <- "output/satscan/spatial_bernoulli"
#modrun <- "rb_bd_h12_bern_iso_50p_500k_5min"
#file_in <- here(glue("{pathrun}/{modrun}.txt"))

# find lines to skip based on a pattern
find_skip <- function(file, pattern, n=6) {
  min(grep(pattern, readr::read_lines(file), n))
}

# Give pattern, and how many lines you want to return after pattern
# read_lines(file_in, skip=find_skip(file_in, "SUMMARY OF DATA"), n_max = 4, skip_empty_rows = TRUE)
#
# read_lines(file_in, skip=find_skip(file_in, "^Analysis"), n_max = 4, skip_empty_rows = TRUE)
#
# cat(read_lines(file_in, skip=find_skip(file_in, "Multiple Coordinates Type"), n_max = 30, skip_empty_rows = TRUE), sep = "\n")


source(here("scripts/My.gbm.step.R"))
