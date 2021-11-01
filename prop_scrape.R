# https://stackoverflow.com/questions/44357803/webscraping-in-r
# https://github.com/yusuzech/r-web-scraping-cheat-sheet/blob/master/README.md#rvest7.5

library(rvest)
library(stringr)
library(leaflet)
library(ggmap)
library(plyr)
library(dplyr)
library(getProxy)
library(rvest)
library(httr)
library(ggplot2)
library(plotly)
# https://www.rostrum.blog/2019/03/04/polite-webscrape/
library(polite)  # respectful webscraping
# # Make our intentions known to the website
# bow <- bow(
#   url = "https://www.property24.com/robots.txt",  # base URL
#   user_agent = my_user_agent, # "M Dray <https://rostrum.blog>",  # identify ourselves
#   force = TRUE
# )
# print(bow)

# Use user agents
# https://stackoverflow.com/questions/28852057/change-ip-address-dynamically
# user_agent_list = c(
#   "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.1 (KHTML, like Gecko) Chrome/22.0.1207.1 Safari/537.1",
#   "Mozilla/5.0 (X11; CrOS i686 2268.111.0) AppleWebKit/536.11 (KHTML, like Gecko) Chrome/20.0.1132.57 Safari/536.11",
#   "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/536.6 (KHTML, like Gecko) Chrome/20.0.1092.0 Safari/536.6",
#   "Mozilla/5.0 (Windows NT 6.2) AppleWebKit/536.6 (KHTML, like Gecko) Chrome/20.0.1090.0 Safari/536.6",
#   "Mozilla/5.0 (Windows NT 6.2; WOW64) AppleWebKit/537.1 (KHTML, like Gecko) Chrome/19.77.34.5 Safari/537.1",
#   "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/536.5 (KHTML, like Gecko) Chrome/19.0.1084.9 Safari/536.5",
#   "Mozilla/5.0 (Windows NT 6.0) AppleWebKit/536.5 (KHTML, like Gecko) Chrome/19.0.1084.36 Safari/536.5",
#   "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/536.3 (KHTML, like Gecko) Chrome/19.0.1063.0 Safari/536.3",
#   "Mozilla/5.0 (Windows NT 5.1) AppleWebKit/536.3 (KHTML, like Gecko) Chrome/19.0.1063.0 Safari/536.3",
#   "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_0) AppleWebKit/536.3 (KHTML, like Gecko) Chrome/19.0.1063.0 Safari/536.3",
#   "Mozilla/5.0 (Windows NT 6.2) AppleWebKit/536.3 (KHTML, like Gecko) Chrome/19.0.1062.0 Safari/536.3",
#   "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/536.3 (KHTML, like Gecko) Chrome/19.0.1062.0 Safari/536.3",
#   "Mozilla/5.0 (Windows NT 6.2) AppleWebKit/536.3 (KHTML, like Gecko) Chrome/19.0.1061.1 Safari/536.3",
#   "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/536.3 (KHTML, like Gecko) Chrome/19.0.1061.1 Safari/536.3",
#   "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/536.3 (KHTML, like Gecko) Chrome/19.0.1061.1 Safari/536.3",
#   "Mozilla/5.0 (Windows NT 6.2) AppleWebKit/536.3 (KHTML, like Gecko) Chrome/19.0.1061.0 Safari/536.3",
#   "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/535.24 (KHTML, like Gecko) Chrome/19.0.1055.1 Safari/535.24",
#   "Mozilla/5.0 (Windows NT 6.2; WOW64) AppleWebKit/535.24 (KHTML, like Gecko) Chrome/19.0.1055.1 Safari/535.24",
#   # https://gist.github.com/seagatesoft/e7de4e3878035726731d
#   'Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:23.0) Gecko/20100101 Firefox/23.0',
#   'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/29.0.1547.62 Safari/537.36',
#   'Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.2; WOW64; Trident/6.0)',
#   'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/33.0.1750.146 Safari/537.36',
#   'Mozilla/5.0 (Windows NT 6.2; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/33.0.1750.146 Safari/537.36',
#   'Mozilla/5.0 (X11; Linux x86_64; rv:24.0) Gecko/20140205 Firefox/24.0 Iceweasel/24.3.0',
#   'Mozilla/5.0 (Windows NT 6.2; WOW64; rv:28.0) Gecko/20100101 Firefox/28.0',
#   'Mozilla/5.0 (Windows NT 6.2; WOW64; rv:28.0) AppleWebKit/534.57.2 (KHTML, like Gecko) Version/5.1.7 Safari/534.57.2'
#   )
# http://www.useragentstring.com/
my_user_agent <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.61 Safari/537.36"
 
# Get, download (as csv) and use proxies
# https://github.com/constverum/ProxyBroker
# Run this in commad line before
# proxybroker find --types HTTPS --lvl High --countries US ZA NA --strict -l 10 --outfile ./proxies.txt
# sed 's|[<>,]||g' proxies.txt > proxies_fixed.txt
proxies <- read.csv("~/proxies_fixed.txt", header = FALSE, sep="")['V5']
colnames(proxies) <- 'url_port'
proxies$url_port = proxies$url_port %>% as.character()
proxies$working <- TRUE
proxies$id <- 1:nrow(proxies)

# Function to read a page with using random (updated working ) proxies with Sys.sleep between pages
read_page <- function(url) {
  page <- "error"
  for (i in 1:10){
    if (page[1] != "error"){
      break
    }
    print(paste("attempt",i))
    # Sleep between scraping the pages
    print(paste("Start sleeping "))
    min_time <- 2
    max_sleep_time <- 4
    sleep_time <- runif(1, min_time, max_sleep_time)
    print(paste("Sleeping for", round(sleep_time, 2)))
    Sys.sleep(sleep_time)
    print(paste("Done sleeping "))
    # Don't know if this is needed
    closeAllConnections()
    gc()
    proxies_working <- proxies %>% filter(working)
    proxy_to_use <- sample_n(proxies_working,1)$url_port
    print(proxy_to_use)
    page = tryCatch({
      # page <- read_html(httr::GET(url,httr::use_proxy(proxy_to_use), httr::timeout(30)))
      page <- read_html(httr::GET(url,httr::use_proxy(proxy_to_use), httr::user_agent(my_user_agent), httr::timeout(30)))
      page_text <- page %>% html_text() 
      if(page_text=="The service is unavailable."){
        proxies[proxies$url_port==proxy_to_use,"working"] <<- FALSE
        "error"
      }else{
        page
      }
    }, warning = function(w) {
      "warning"
    }, error = function(e) {
      print("error")
      proxies[proxies$url_port==proxy_to_use,"working"] <<- FALSE
      "error"
    }, finally = {
    }
    )
  }
  return(page)
}

# Rearrange desired scraped data (from xml/html) to list and then later a dataframe 
get_property_content_data <- function(page_content) {
  
  page_content <- init_page %>%
    html_nodes('.p24_content')
  
  price_data <- page_content %>% 
    html_nodes('.p24_price') %>% 
    html_attr("content") %>% 
    as.numeric()
  
  address_data <- page_content %>% 
    html_nodes(".p24_address") %>%
    html_text()

  description_data <- page_content %>% 
    html_nodes(".p24_title") %>%
    html_text()
  
  location_data <- page_content %>% 
    html_nodes('span.p24_location') %>% 
    html_text()
  
  floor_size <- page_content %>% 
    html_nodes('.p24_icons') %>% 
    html_nodes('.p24_size') %>% 
    html_text() %>% 
    str_replace_all("[\r\n]", "") %>% 
    str_replace_all("m²", "") %>% 
    str_replace_all("[[:blank:]]", "") %>% 
    as.numeric()
  
  list(price_data=price_data,address_data=address_data, description_data=description_data,
       location_data=location_data, floor_size=floor_size)
}

# paste here your overall searching suburb's url (with subpages)
url <- 'https://www.property24.com/for-sale/die-boord/stellenbosch/western-cape/8530'
init_page <- read_page(url)

sub_pages <- init_page %>% 
  html_nodes('.p24_pager') %>% 
  html_nodes('.text-center') %>% 
  html_node('.pagination') %>% 
  html_nodes("a") %>% 
  html_attr("href")
# https://stackoverflow.com/questions/42943533/r-get-last-element-from-str-split
all_subpages <- NULL
all_subpages[1:length(sub_pages)-1] <- sub_pages[1:length(sub_pages)-1]
more_number_subpages = gsub('^.*/p\\s*|\\s*\\?.*$', "", sub_pages)[(length(sub_pages)-1):length(sub_pages)] %>% as.numeric()
more_subpages <- paste(gsub("\\?.*","",sub_pages[1]), "/p", (more_number_subpages[1]+1):more_number_subpages[2], "?", sub('.*\\?', '', sub_pages[1]), sep="")
all_subpages[(more_number_subpages[1]+1):more_number_subpages[2]] <- more_subpages

all_subpages <- sub_pages

all_data <- NULL
for (i in 1:length(all_subpages)){
  
  print(paste(i, "out of", length(all_subpages)))
  print(paste("scraping", all_subpages[i]))
  page <- read_page(all_subpages[i])
  
  list_id <- page %>% 
    html_nodes('.js_listingResultsContainer') %>% 
    html_nodes('.p24_regularTile') %>% 
    html_attr("data-listing-number")
  
  # list_href <- paste(all_subpages[1],"/",list_id, sep="")
  list_href <- page %>%
    html_nodes('.js_listingResultsContainer') %>%
    # html_nodes('.p24_promotedTile') %>%
    # html_nodes('.js_pseudoLinkHref') %>%
    html_nodes('.p24_regularTile') %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    unique() %>%
    paste("https://www.property24.com",., sep="")
  
  df <- page %>% 
    html_nodes('.p24_content') %>% 
    lapply(get_property_content_data) %>%
    do.call(rbind, .) %>% 
    as.data.frame

  df$location_data <- as.character(df$location_data)
  df$location_data[df$location_data=="character(0)"] <- NA
  df$address_data <- as.character(df$address_data)
  df$address_data[df$address_data=="character(0)"] <- NA
  df$description_data <- as.character(df$description_data)
  df$description_data[df$description_data=="character(0)"] <- NA
  df$price_data <- as.numeric(df$price_data)
  df$floor_size <- as.numeric(df$floor_size)
  df$geo_address <- paste(df$address_data,df$location_data, "South Africa", sep=" ")
  # ToDo: add stellenbosch/western-cape in geo_address
  df <- df[!is.na(df$description_data),]
  
  df$id <- list_id
  df$url <- list_href
  # register with Google maps API key 
  # https://console.developers.google.com/apis/credentials?folder=&organizationId=&project=geo-location-1579987100565
  # register_google(key='')
  geo_location <- geocode(df$geo_address)
  df <- cbind(df, as.data.frame(geo_location))
  all_data[[i]] <- df
}

data <- ldply(all_data, data.frame)

data_address_data <- data %>% filter(is.na(address_data))
# %>% filter(is.na(lon_p24))
data$url_string <- paste0(data$url)

pb = txtProgressBar(min = 0, max = length(data_address_data$id), initial = 0) 
for (i in data_address_data$id){
  print(i)
  setTxtProgressBar(pb,1)
  
  # Get individual lat/lon
  listing_url <- data[data$id==i, 'url'] %>% as.character()

  print(paste("scraping page", listing_url))
  listing_page <- read_page(listing_url)
  if (listing_page != "error"){
    listing_page_scripts  <- listing_page %>% html_nodes('script')
    if (length(listing_page_scripts) >= 18){
      listing_page_adress <- listing_page_scripts[18] %>% html_text()
      # https://stackoverflow.com/questions/31530930/how-to-get-the-text-between-two-words-in-r
      data[data$id==i,'lat_p24'] <- gsub("^.*Latitude=\\s*|\\s*&.*$", "", listing_page_adress) %>% as.numeric()
      data[data$id==i,'lon_p24'] <- gsub("^.*Longitude=\\s*|\\s*').*$", "", listing_page_adress) %>% as.numeric()
    }
  }
}

data$lat_g<- data$lat
data$lon_g<- data$lon
# data$lat_g_fixed <- data$lon_g
# data$lon_g_fixed <- data$lat_g

data$lat <- coalesce(data$lat_p24, data$lat_g)
data$lon <- coalesce(data$lon_p24, data$lon_g)

# Save for use (e.g. Shiny)
write.csv(data, "data.csv",row.names  = TRUE)

# data <- read.csv("~/R_web_scrape/data_western-cape_Strand_norht_7819.csv")

# Visualise the data

pal <- colorNumeric(
  palette = "RdYlGn",
  domain = data$price_data,
  reverse = TRUE
)

leaflet(data) %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addCircles(radius=~floor_size/10,
             color=~pal(price_data),
             label=~paste(floor_size, " m2", "|",
                          "R", format(price_data, big.mark=",")
             ),
             # https://steemit.com/geomatics/@thornux/leaflet-mapping-in-rstudio-custom-popups
             popup = ~paste("<iframe width=1000 height=500 src='", url ,"'frameborder=0 allowfullscreen></iframe>", sep=""),
             #popup = ~paste('<iframe width="300" height="169" src=', url, 'frameborder="0" allowfullscreen></iframe>')
             #popup = '<a href="https://www.property24.com/for-sale/strandvale/strand/western-cape/16514/108328732?plId=516899&plt=3/">P24</a>',
             stroke = FALSE,
             fillOpacity = 0.5
  ) %>%
  #https://rstudio.github.io/leaflet/legends.html
  addLegend("bottomright", pal = pal, values = ~price_data,
            title = "Price",
            labFormat = labelFormat(prefix = "R"),
            opacity = 0.7
  ) %>%
  addLayersControl(
    baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
    options = layersControlOptions(collapsed = FALSE)
  )

ggplot(data %>% filter(floor_size<1000), aes(floor_size, price_data, colour = location_data)) + 
  geom_point() %>% 
ggplotly(ggplot(data %>% filter(floor_size<1000), aes(floor_size, price_data, colour = location_data)) + 
           geom_point())