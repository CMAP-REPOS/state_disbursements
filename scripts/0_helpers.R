

#' function to remove a total row, checking the sum first
#' 
#' @param df a data frame with a total row
#' @param col1 the column that has the word "TOTAL" in it, demarcating the total row
#' @param col2 the column with the total to check. Must be numeric.
#' @param msg_label the value to append on the front end of the confirmation message
#' 
total_check_extract <- function(df, col1, col2, msg_label = ""){

  # identify total row in col1
  total_pos <- str_which(df[[col1]], "TOTAL")
  
  if (length(total_pos) > 2) {
    warning(paste0(msglabel, ": Multiple total rows? Check! (NULL returned)"))
    return(NULL)
  } else if (length(total_pos) == 0) {
    message(paste0(msglabel, ": No total row found. Check!"))
  } else {
    # first part of message: where is total row located?
    msg1 <- paste("Total row (@", total_pos, "of", nrow(df), "rows")
    
    # extract total row and remove it (and any footnotes) from df
    total_row <- df[total_pos,]
    df <- slice(df, 1:total_pos-1)
    
    # compare totals
    df_sum <- sum(df[[col2]])
    totalrow_sum <- total_row[[col2]]
    matches <- all.equal(df_sum, totalrow_sum)
    
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


