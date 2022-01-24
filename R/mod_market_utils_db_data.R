#' Get a postrgres connection object
#' 
#' @description Get a postrgres connection object. An ODBC connection has to have been 
#' created on the local machine running the application
#'
#' @return A postgres connection object 
#' @export
#'
#' @examples
#' \dontrun{
#' postgres_connection()
#' }
postgres_connection = function(){
  if(exists("con")){
    message("Existing connection object available, using it")
    con
  }else{
    message("No existing connection object available, creating it")
    con <<- DBI::dbConnect(odbc::odbc(), "PostgreSAKiRP")
  }
}

#' Get extension officer details for each participating ward
#'
#' @return Returns a dataframe of ward extension officers
#' @export
#' @importFrom DBI dbReadTable
#'
#' @examples
#' \dontrun{
#' get_extension_officers()
#' }
get_extension_officers = function(){
  waeos = DBI::dbReadTable(postgres_connection(),"waeos")
  active_waeos = filter(waeos,active == 1)
}

#' Get district details for each participating district
#'
#' @return Returns a dataframe of participating districts
#' @export
#'
#' @examples
#' \dontrun{
#' get_districts
#' }
get_districts = function(){
  DBI::dbReadTable(postgres_connection(),"districts")
}

#' Get ward details for each participating ward
#'
#' @return Returns a dataframe of participating wards
#' @export
#'
#' @examples
#' \dontrun{
#' get_wards()
#' }
get_wards = function(){
  DBI::dbReadTable(postgres_connection(),"wards")
}
