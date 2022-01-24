#' Agricultural commodity price data in wide format
#'
#' @return Returns a wide dataframe of agricultural commodities and their prices
#' @export
#'
#' @examples
#' price_wide()
price_wide = function(){
  data = dplyr::left_join(tidy_data(),get_districts(),by=c("district"="id")) 
  data2 = dplyr::left_join(data,get_wards(),by=c("ward"="id"))
  data3 = dplyr::select(data2,
                        District = district.y,
                        Ward = ward.y,
                        Date = endtime,
                        # should leave thise with a `price` suffix, for manipulating
                        # downstream (generalisability)
                        `Kigoma Njano` = local_yellow_price,
                        `Dried Cassava` = dried_cassava_price,
                        `Fresh Cassava` = fresh_cassava_price,
                        `Cassava Flour` = cassava_flour_price,
                        `Sunflower Seed` = sf_seed_price,
                        `Sunflower Oil` = sf_oil_price,
                        `Sunflower Cake` = sf_cake_price,
                        `Palm Oil` = palm_oil_price
  ) 
  
  dplyr::mutate(data3,
                Month = lubridate::month(Date),
                Week = lubridate::week(Date),
                Year = lubridate::year(Date),
  )
}

