library(tidyverse)
library(httr)
library(xml2)
library(rvest)

# get list of flowers
url <- "https://www.johnnyseeds.com/flowers/"
page <- xml2::read_html(url)
flowers <- xml2::xml_find_all(page, xpath="//a[@class='c-navigation-block__link refinement-link']") %>%
  xml2::xml_attrs() %>%
  map(~.x[["href"]]) %>%
  map(~paste0(.x, "?sz=25&start=0"))
flowers <- flowers[c(-37, -75)]

#get varieties
df <- map_df(flowers, function(flower){
  page <- xml2::read_html(flower)
  
  flower_name <- xml2::xml_find_all(page, xpath="//div[@class='c-slider__header']") %>%
    xml_text() %>%
    str_remove_all("\n")
  
  vars <- xml2::xml_find_all(page, xpath="//a[@class='c-tile__link name-link']") %>%
    xml2::xml_attrs() %>%
    map(~paste0("https://www.johnnyseeds.com", .x[["href"]]))
  
    map_df(vars, function(var){
    page <- xml2::read_html(var) 
    
    var_name <- xml_find_all(page, xpath="//h1[@class='c-product-header__heading product-name']") %>%
      xml_text() %>%
      str_remove_all("\n")
    
    product_id <- xml_find_all(page, xpath="//span[@itemprop='productID']") %>%
      xml_text()
    
    flower_nodeset <- xml_find_all(page, xpath="//div[@class='c-accordion__body s-lgc-pdp-content js-accordion__body']") %>%
      xml_children()
    
    headers_nodeset <- xml_find_all(flower_nodeset, xpath ="//span[contains(@style, 'bold')]")

    index <- seq_along(flower_nodeset)
    headers_index <- which(flower_nodeset %in% headers_nodeset)
    dummy_index <- c(headers_index[-1], length(flower_nodeset)+1)
    content_index <- map2(headers_index, dummy_index, function(x,y){
      index[index > x & index < y]
    })
    
    headers <- flower_nodeset[headers_index] %>% xml_text()
    content <- map_dfc(content_index, function(x){
      out <- flower_nodeset[x] %>% 
        xml_text() %>%
        map(~trimws(.x)) %>%
        discard(~.x=="") %>%
        unlist()
      if(length(out)>1) {out <- reduce(out,paste)}
      if(is.null(out)){out <- ""}
      return(out)
    })
    
    assertthat::are_equal(length(headers), ncol(content))
    headers_clean <- janitor::make_clean_names(headers)
    
    content %>%
      set_names(headers_clean) %>%
      mutate(flower = flower_name, variety = var_name, product_id = product_id)
  })
  
})

df2 <- df %>%
  select(product_id, flower, variety, everything())

write.csv(df2, "johnnys_flowers.csv")

#TODO - read in my list of flowers, look up codes, generate sheet according to chris's template - assign simple bedfeet and then adjust as needed

