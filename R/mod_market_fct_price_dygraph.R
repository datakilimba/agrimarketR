#' Create a timeseries graph of commodity prices
#'
#' @param data Price data 
#' @param product Product selected by user
#' @param dstrct District of interest
#' @param wrd Ward of interest
#'
#' @return a dypgraph object
#' @export
#' @importFrom magrittr '%>%'
#' @importFrom dygraphs dySeries
#'
#' @examples
#' \dontrun{
#'  price_dygraph(price_long(),product = "Kigoma Njano",
#'  dstrct = "Kigoma",wrd = "Kalinzi")
#' }
price_dygraph = function(data,product,dstrct,wrd){
  region_dat = data %>% 
    filter(name==product) %>% 
    group_by(Year,Month) %>% 
    summarise(median_price = median(price,na.rm = TRUE)) %>% 
    mutate(date = as.Date(paste0(Year,"-",Month,"-","15")))
  
  district_dat = data %>% 
    filter(District == dstrct,name == product) %>% 
    group_by(District,Year,Month) %>% 
    summarise(median_price = median(price)) %>% 
    mutate(date = as.Date(paste0(Year,"-",Month,"-","15")))
  
  ward_dat = data %>% 
    filter(District==dstrct,Ward==wrd,name==product) %>% 
    group_by(Ward,Year,Month) %>% 
    summarise(median_price = median(price)) %>% 
    mutate(date = as.Date(paste0(Year,"-",Month,"-","15")))
  
  region = xts::xts(region_dat$median_price,region_dat$date)
  district = xts::xts(district_dat$median_price,district_dat$date)
  ward = xts::xts(ward_dat$median_price,ward_dat$date)
  
  series = cbind(region,district,ward)
  
  dygraphs::dygraph(series,main = 
            paste0(wrd," - ",product)) %>% 
    dygraphs::dySeries("region",label = "Region Price",strokeWidth = 3) %>% 
    dygraphs::dySeries("district",label = "District Price",strokeWidth = 3,
             strokePattern = "dashed") %>% 
    dygraphs::dySeries("ward", label = "Ward Price",strokeWidth = 3, 
             strokePattern = "dotted")
}
