#' Read data from online only if internet available or if data is stale
#'
#' @return a dataframe of agricultural commodity prices in raw form
#' @export
#' @importFrom labelled var_label
#' @importFrom curl has_internet
#'
#' @examples
#' \dontrun{
#' get_data()
#' }
get_data = function(){
  
  if(exists("survey")){
    # Check that data is current
    if(labelled::var_label(survey$deviceid)==as.character(lubridate::today())){
      message("current data, returning local")
      return(survey)
    } else{
      # Check internet access
      if(curl::has_internet()){
        message("data not current, internet accessible, returning hosted")
        agrimarketR::get_raw_data()
      }else{
        message("data not current, internet NOT accessible, returning most current local")
        readRDS(here("data","market_data.rds"))
      }
      
    }
  }else{
    message("market data does not exist locally, going live")
    if(curl::has_internet()){
      get_raw_data()
    }else{
      message("Unfortunately, no internet connection, going local")
      readRDS(here("data","market_data.rds"))
    }
    
  }
}
