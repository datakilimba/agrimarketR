#' Agricultural commodity price data in long format
#'
#' @return Returns a long dataframe of agricultural commodities and their prices
#' @export
#' 
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr group_by summarise
#'
#' @examples
#' price_long
price_long = function(){
  price_data = tidyr::pivot_longer(price_wide(), cols=`Kigoma Njano`:`Palm Oil`) 
  price_data_grouped =  dplyr::group_by(price_data, District,Ward,Year,Month,Week,name) 
  dplyr::summarise(price_data_grouped,price=median(value,na.rm = T))
}
