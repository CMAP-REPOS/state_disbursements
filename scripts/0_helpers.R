#' function to remove a total row, checking the sum first
#' 
#' @param df a data frame with a total row
#' @param col1 the column that has the word "TOTAL" in it, demarcating the total row
#' @param col2 the column with the total to check. Must be numeric.
#' @param msg_label the value to append on the front end of the confirmation message
#' @param search the label to search for in `col1`
#' 
total_check_extract <- function(df, col1, col2, msg_label = "", search = "TOTAL"){
  
  # identify total row in col1
  total_pos <- str_which(df[[col1]], search)
  
  if (length(total_pos) > 2) {
    warning(paste0(msg_label, ": Multiple total rows? Check! (NULL returned)"))
    return(NULL)
  } else if (length(total_pos) == 0) {
    message(paste0(msg_label, ": No total row found. Check!"))
  } else {
    # first part of message: where is total row located?
    msg1 <- paste("Total row (@", total_pos, "of", nrow(df), "rows")
    
    # extract total row and remove it (and any footnotes) from df
    total_row <- df[total_pos,]
    df <- slice(df, 1:total_pos-1)
    
    # remove any entirely blank rows
    df <- df %>% 
      filter_all(any_vars(complete.cases(.)))  
    
    # compare totals
    df_sum <- sum(df[[col2]])
    totalrow_sum <- total_row[[col2]]
    matches <- isTRUE(all.equal(df_sum, totalrow_sum))
    
    # second part of message: do totals match?
    msg2 <- if_else(matches,
                    "sum OK",
                    paste("total row and sum mismatch. Check!",
                          paste("   df sum:   ", df_sum),
                          paste("   total row:", totalrow_sum),
                          sep = "\n"
                    )
    )
    
    # message and return
    message(paste(msg_label, "|", msg1, "|", msg2))
    
  }
  
  return(df)
}  



#' function to strip headers and footers from an imported PDF list
#'
#' `str_split(pdf_text(.), "\n")` produces a list of character vectors. Each
#' list item is a page, and each vector item is a line on the page. This
#' function goes page by page (list item by list item) and removes header and
#' footer character vectors, returning only the data.
#'
#' @param raw_list the output of `str_split(pdf_text(.), "\n")`.
#' @param header_search text to search for that demarcates the last row to
#'   remove at the top of the page
#' @param footer_search text to search for that demarcates the first row to
#'   remove at the bottom of the page
#'   
rm_header_footer <- function(raw_list, header_search, footer_search){
  processed <- list()
  for (i in seq.int(length(raw_list))) {
    # identify the last row that potentially contains column headers, and remove rows up through it.
    header_row <- last(str_which(raw_list[[i]], header_search))
    processed[[i]] <- raw_list[[i]][-(seq.int(header_row))] 
    
    # identify the first blank row, and remove it and all rows after.
    footer_row <- str_which(processed[[i]], footer_search)[1]
    processed[[i]] <- processed[[i]][-seq(footer_row, length(processed[[i]]))]
  }
  
  return(processed)
}

#' Clean  names from various raw datasets to make them internally and externally consistent
#' 
clean_names <- function(names_vector, corrections_table = NULL){
  
  # get corrections table
  if (is.null(corrections_table)){
    corrections_table <- read.csv(here("resources", "name_corrections.csv")) %>% 
      mutate(ExistingRecord = tolower(ExistingRecord))
  }
  
  df <- as.data.frame(names_vector) %>% 
    set_names("old") %>% 
    # apply general rules
    mutate(
      new1 = str_to_title(old),
      new2 = str_remove(new1, " (Township|Twp|Town|County)$"), # Lots of muni names actually end with "City" or "Village"
      new2 = str_remove(new2, "^(Township|Twp|Town|Village|City) Of "),
      new3 = str_replace(new2, "^St |^Saint |(?<=^East )St ", "St. "),
      new3 = str_replace(new3, "^Ofallon|^O'fallon|^O Fallon", "O'Fallon"),
      new3 = str_replace(new3, "^Gibson$", "Gibson City"),
      new3 = str_replace(new3, "^Rocky Run$|^Wilcox$|^Rocky Run Wilcox$", "Rocky Run-Wilcox"),  # These 2 twps merged mid-2017
      ) %>% 
    # apply custom renaming via corrections table
    mutate(join = str_to_lower(new3)) %>% 
    left_join(corrections_table, by = c("join" = "ExistingRecord")) %>% 
    mutate(new4 = ifelse(is.na(CensusName), new3, CensusName)) %>% 
    # identify changes (besides for capitalization and prefix/suffix removal)
    mutate(changed = ifelse(new4 == new2, FALSE, TRUE))
  
  changes <- filter(df, changed) %>%
    select(BEFORE = new2, AFTER = new4) %>% 
    unique()
  
  row.names(changes) <- seq_len(nrow(changes))
  
  message("Set all names to title case, removed primary LG type suffixes, and changed the following names:")
  print.data.frame(changes)
  
  return(df$new4)
}
