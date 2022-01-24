#' Get market data which has gone through assertR cleaning
#'
#' @return Return a dataframe which has gone through assertr cleaning
#' @export
#' 
#' @importFrom assertr insist within_n_mads just_warn
#' @importFrom dplyr slice
#'
#' @examples
#' \dontrun{
#' clean_tidy_data()
#' }
clean_tidy_data = function(){
  data = tidy_data()
  market_survey_dqa(data)
}


market_survey_dqa = function(monthly_price_data){
  clean_dat = assertr::insist(monthly_price_data,
      assertr::within_n_mads(3), 
      local_yellow_price,
      error_fun = error_handler
    )
  clean_dat
}

error_handler = function( ... ) {
  args = list(...)
  
  do.call( assertr::just_warn, args )
  
  bad_data = args[[1]][[1]]$error_df
  
  these_failed_rows = dplyr::slice(args$data,bad_data$index )
  
  if(!exists("my_failed_rows", inherits=TRUE)) {
    my_failed_rows = NULL
  }
  
  my_failed_rows = cbind(these_failed_rows,bad_data)
  assign( "my_failed_rows", my_failed_rows, envir= .GlobalEnv )
  good_rows = dplyr::slice(args$data,-bad_data$index)
  
  return(good_rows)
}
