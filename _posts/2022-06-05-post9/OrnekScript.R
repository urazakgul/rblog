library(rvest)
library(DBI); library(RSQLite)
library(openxlsx)
library(tidyverse)
library(lubridate)

url <- "https://www.cimri.com/market/temel-gida?page=1"

lastPage <- read_html(url) %>% 
  html_nodes("div.Pagination_pagination__6kvLO li") %>% 
  html_text()
lastPage <- as.numeric(tail(lastPage[lastPage != ""], 1))

urls <- str_c(
  "https://www.cimri.com/market/temel-gida?page=",
  seq(1,lastPage,1)
)

master <- data.frame()
time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

for(i in 1:2){
  
  thepage <- read_html(urls[i])
  
  item <- thepage %>% 
    html_nodes("div.ProductCard_productName__35zi5") %>% 
    html_text()
  
  price <- thepage %>% 
    html_nodes("div.ProductCard_footer__Fc9OL span.ProductCard_price__10UHp") %>% 
    html_text()
  
  tbl <- data.frame(
    Item = item,
    Price = price
  )
  
  master <- master %>% 
    bind_rows(tbl)
  
  Sys.sleep(time = 3)
  
  if(i == 2){
    
    master$Time <- time
    
  }
  
}

myDB <- dbConnect(SQLite(), "C:/Users/datanerd/Desktop/Github/blog/_posts/2022-06-05-post9/marketDB.sqlite")
dbWriteTable(myDB, "master", master, append = TRUE)
mastertbl <- dbGetQuery(myDB, "SELECT * FROM master")
dbDisconnect(myDB)

mastertbl$Time <- as.POSIXct(mastertbl$Time)
maxTime <- mastertbl[ymd_hms(mastertbl$Time)==max(ymd_hms(mastertbl$Time)),]
maxTime2 <- mastertbl[ymd_hms(mastertbl$Time)!=max(ymd_hms(mastertbl$Time)),]
maxTime2 <- maxTime2[ymd_hms(maxTime2$Time)==max(ymd_hms(maxTime2$Time)),]

if(nrow(maxTime2) > 0){
  
  comparetbl <- maxTime %>% 
    bind_rows(maxTime2) %>% 
    group_by(Time) %>% 
    mutate(ID = cur_group_id(),
           ID  = if_else(ID == 1, "Before", "After")) %>% 
    pivot_wider(!Time, names_from = "ID", values_from = "Price") %>% 
    mutate(
      After = as.numeric(gsub("\\,",".",gsub(" TL","",After))),
      Before = as.numeric(gsub("\\,",".",gsub(" TL","",Before))),
      Diff = After - Before
    ) %>% 
    filter(Diff != 0)
  
}

if(exists("comparetbl")){
  
  if(nrow(comparetbl) > 0){
    
    write.xlsx(comparetbl, "PriceTracker.xlsx")
    
  }
  
}