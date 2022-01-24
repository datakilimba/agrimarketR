#' Gets raw data from data collected on a form hosted on Kobo server
#' 
#' @description Gets raw data from data collected on a form hosted on a Kobo server.
#'  Requires that your local environment variables for `sakirp_user` 
#'  (a Kobo user account) and `sakirp_pw` (the corresponding password) be set using
#'  `Sys.setenv(sakirp_user = "kobo user name,sakirp_pw = "kobo password`
#'
#' @return returns a dataframe of the survey data and sets it as a global variable
#' named `survey`
#' @export
#'
#' @importFrom here here
#' @importFrom readr read_csv
#' @importFrom labelled var_label
#' @importFrom httr GET content authenticate
#' @examples
#' \dontrun{
#' get_raw_data()
#' }
get_raw_data = function(){
  
  kobo_server_url = "https://kc.humanitarianresponse.info/"
  form_id = "933925"
  url = paste0(kobo_server_url,"api/v1/data/",form_id,".csv")
  rawdata = httr::GET(url,httr::authenticate(Sys.getenv("sakirp_user"),Sys.getenv("sakirp_pw")))
  content = httr::content(rawdata,"raw",encoding="UTF-8")
  
  survey = readr::read_csv(content)
  labelled::var_label(survey) = as.character(lubridate::today())
  saveRDS(survey, here::here("data","market_data.rds"))
  
  survey <<- survey
}