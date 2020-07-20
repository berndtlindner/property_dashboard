# property_dashboard
R Shiny app scraping property sites for sale/rent and graphing features

At the moment this consists of  

0. Using [ProxyBroker](ProxyBroker) > proxies.txt
1. [prop_scrape.R](prop_scrape.R) scraping the webpages with proxies.txt > data.csv
2. [prop_data_dashboard.R](prop_data_dashboard.R) which reads in this data.csv

![prop_scrape_dashboard.gif](prop_scrape_dashboard.gif)