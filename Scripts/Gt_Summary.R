library(gtsummary)

# summarize the data with our package
table1 <- 
  trial %>%
  tbl_summary(include = c(age, grade, response))

table2 <-
  tbl_summary(
    trial,
    include = c(age, grade, response),
    by = trt, # split table by group
    missing = "no" # don't list missing data separately
  ) %>%
  add_n() %>% # add column with total number of non-missing observations
  add_p() %>% # test for a difference between groups
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels()

debug(tbl_summary)

?add_p
