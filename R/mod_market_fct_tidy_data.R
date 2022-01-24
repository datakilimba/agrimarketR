#' Get selected columns of market data
#'
#' @return Returns a dataframe of select columns
#' @export
#' @importFrom janitor clean_names
#' @importFrom dplyr ends_with select mutate rename
#'
#' @examples
#' \dontrun{
#' tidy_data()
#' }
tidy_data = function(){
   
    data = janitor::clean_names(get_data())
    data1 = dplyr::rename(data,
      bean_cess = dplyr::ends_with("crop_cess_beans"),
      cassava_cess = dplyr::ends_with("crop_cess_cassava"),
      sunflower_cess_ = dplyr::ends_with("crop_cess_sunflower"),
      red_bean_price_ = dplyr::ends_with("bean_red_price_market"),
      improved_yellow_price = dplyr::ends_with("bean_improvedyellow_price_market"),
      local_yellow_price = dplyr::ends_with("bean_yellow_price_market"),
      dried_cassava_price = dplyr::ends_with("cassava_dried_price_market"),
      fresh_cassava_price = dplyr::ends_with("cassava_fresh_price_market"),
      cassava_flour_price = dplyr::ends_with("cassava_flour_price_market"),
      sf_seed_price = dplyr::ends_with("sunflower_seeds_price_market"),
      sf_cake_price = dplyr::ends_with("sunflower_cake_price_market"),
      sf_oil_price = dplyr::ends_with("sunflower_oil_price_market"),
      palm_oil_price = dplyr::ends_with("palm_oil_price_farm"),
      working_capital = dplyr::ends_with("working_capital"),
      quantity_beans = dplyr::ends_with("quantity_beans"),
      quantity_cassava = dplyr::ends_with("quantity_cassava"),
      
    )
    
    data2 = dplyr::select(data1,
      `uuid`,
      endtime,
      district = survey_info_district,
      ward = survey_info_ward,
      enumname = survey_info_enumname,
      bean_cess,
      cassava_cess,
      sunflower_cess_,
      red_bean_price_,
      improved_yellow_price,
      local_yellow_price,
      dried_cassava_price,
      fresh_cassava_price,
      cassava_flour_price,
      sf_seed_price,
      sf_cake_price,
      sf_oil_price,
      palm_oil_price,
      working_capital,
      quantity_beans,
      quantity_cassava
    ) 
    
    data3 = dplyr::mutate(data2,
      sunflower_cess = dplyr::case_when(
        sunflower_cess_==999 | sunflower_cess_==0 ~ NA_integer_,
        TRUE ~ as.integer(sunflower_cess_)
      ),
      # sunflower_cess = as.integer(sunflower_cess),
      # 
      red_bean_price_ = dplyr::case_when(
        red_bean_price_=="n/a" | red_bean_price_=="999"|
          red_bean_price_=="0"~NA_character_,
        TRUE~red_bean_price_
      ),
      red_bean_price = as.integer(red_bean_price_),
      
      improved_yellow_price = dplyr::case_when(
        improved_yellow_price=="n/a"|improved_yellow_price=="999"|
          improved_yellow_price=="0"~NA_character_,
        TRUE~improved_yellow_price
      ),
      improved_yellow_price = as.integer(improved_yellow_price),
      
      local_yellow_price = dplyr::case_when(
        local_yellow_price=="n/a"|local_yellow_price=="999"|
          local_yellow_price=="0"~NA_character_,
        TRUE~local_yellow_price
      ),
      local_yellow_price = as.integer(local_yellow_price),
      
      dried_cassava_price = dplyr::case_when(
        dried_cassava_price=="n/a"|dried_cassava_price=="999"|
          dried_cassava_price=="0"~NA_character_,
        TRUE~dried_cassava_price
      ),
      dried_cassava_price = as.integer(dried_cassava_price),
      
      fresh_cassava_price = dplyr::case_when(
        fresh_cassava_price=="n/a"|fresh_cassava_price=="999"|
          fresh_cassava_price=="0"~NA_character_,
        TRUE~fresh_cassava_price
      ),
      fresh_cassava_price = as.integer(fresh_cassava_price),
      
      cassava_flour_price = dplyr::case_when(
        cassava_flour_price=="n/a"|cassava_flour_price=="999"|
          cassava_flour_price=="0"~NA_character_,
        TRUE~cassava_flour_price
      ),
      cassava_flour_price = as.integer(cassava_flour_price),
      # 
      sf_seed_price = dplyr::case_when(
        sf_seed_price=="n/a"|sf_seed_price=="999"|
          sf_seed_price=="0" ~ NA_character_,
        TRUE~sf_seed_price
      ),
      sf_seed_price =  as.integer(sf_seed_price),
      
      sf_cake_price = dplyr::case_when(
        sf_cake_price=="n/a"|sf_cake_price=="999"|
          sf_cake_price=="0"  ~ NA_character_,
        TRUE~sf_cake_price
      ),
      sf_cake_price = as.integer(sf_cake_price),
      # 
      sf_oil_price = dplyr::case_when(
        sf_oil_price=="n/a"|sf_oil_price=="999"|
          sf_oil_price=="0" ~ NA_character_,
        TRUE~sf_oil_price
      ),
      sf_oil_price = as.integer(sf_oil_price),
      
      palm_oil_price = dplyr::case_when(
        palm_oil_price=="n/a"|palm_oil_price=="999"|
          palm_oil_price=="0" ~ NA_character_,
        TRUE~palm_oil_price
      ),
      palm_oil_price = as.integer(palm_oil_price),
      
      working_capital = dplyr::case_when(
        working_capital=="n/a"|working_capital=="999"|
          working_capital=="0"~NA_character_,
        TRUE~working_capital
      ),
      working_capital = as.integer(working_capital),
      
      quantity_beans = dplyr::case_when(
        quantity_beans=="n/a"|quantity_beans=="999"|
          quantity_beans=="0"~NA_character_,
        TRUE~quantity_beans
      ),
      quantity_beans = as.integer(quantity_beans),
      
      quantity_cassava = dplyr::case_when(
        quantity_cassava=="n/a"|quantity_cassava=="999"|
          quantity_cassava=="0"~NA_character_,
        TRUE~quantity_cassava
      ),
      quantity_cassava = as.integer(quantity_cassava)
    ) 
    
    dplyr::select(data3,-red_bean_price_)
}
