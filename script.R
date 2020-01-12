library(tidyverse)
library(httr)
library(xml2)
library(rvest)

# get list of flowers
url <- "https://www.johnnyseeds.com/flowers/"
page <- xml2::read_html(url)
flowers <- xml2::xml_find_all(page, xpath="//a[@class='c-navigation-block__link refinement-link']") %>%
  xml2::xml_text() %>%
  str_remove_all("\n")

#get varieties
map(flowers, function(flower){
  page <- xml2::read_html(paste0(url, flower))
  vars <- xml2::xml_find_all(page, xpath="//a[@class='c-tile__link name-link']") %>%
    xml2::xml_attrs() %>%
    map(~paste0("https://www.johnnyseeds.com", .x[["href"]]))
  
    
# archive -----------------------------------------------------------------
# can query in intervals of 60
murl <- modify_url(url, query = list(sz = 60, start = 0))
page <- xml2::read_html(murl)
prod <- xml_find_all(page, xpath="//div[@class='c-tile__product-detail']")
