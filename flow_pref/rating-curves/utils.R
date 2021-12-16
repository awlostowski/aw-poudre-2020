
# scrape a page from  Poudre Rock Report
scrape_page <- function(n) {
  logger::log_info("Scraping page {n}")
  # logger::log_info("Scraping page {page[8]}")
  
  site <- paste0(
    "http://www.poudrerockreport.com/category/flows/page/",
    as.character(n)
    # "http://www.poudrerockreport.com/category/flows/page/",
    # as.character(page[8])
  )
  
  articles <- rvest::read_html(site) %>%
    rvest::html_elements("article")
  
  # extract titles
  titles <- rvest::html_attrs(articles) %>%
    lapply(`[`, "aria-label") %>%
    unlist(use.names = FALSE)
  
  # extract dates
  dates <- rvest::html_elements(articles, "time") %>%
    rvest::html_text2()
  
  categories <- rvest::html_elements(articles, ".entry-categories") %>%
    rvest::html_text2() %>%
    stringr::str_remove_all("Filed Under: ")
  
  # blog entry content text contains stage and flow observations at several sites
  entry_content = rvest::read_html(site) %>%
    html_nodes(".entry-content") %>%
    html_text()
  
  # ---- HEWLETT ----
  # locates the string "Hewlett" and select a bunch of characters just after it. 
  J = str_locate(entry_content, pattern = "Hewlett") # locate
  hewlett_strings = str_sub(entry_content, J[,1], J[,2]+100) # select trailing 100 characters
  
  # break "Hewlett"-containing strings into chunks separated by spaces
  comps <- str_split(hewlett_strings, " ", simplify = T)
  
  # extract stage value from each post, loop over rows
  hewlett_stage = c()
  for (i in 1:nrow(comps)) {
    # find "Feet"
    I = which(str_detect(comps[i,], "Feet|ft.|Ft."))
    
    # select the string following and convert to numeric
    hewlett_stage[i] = as.numeric(comps[i,min(I)-1])
  }
  
  # ---- RUSTIC ----
  # locates the string "Rustic" and select a bunch of characters just after it. 
  J = str_locate(entry_content, pattern = "Rustic") # locate
  rustic_strings = str_sub(entry_content, J[,1], J[,2]+100) # select trailing 100 characters
  
  # break "Rustic"-containing strings into chunks separated by spaces
  comps <- str_split(rustic_strings, " ", simplify = T)
  
  # extract stage value from each post, loop over rows
  rustic_stage = c()
  for (i in 1:nrow(comps)) {
    
    # find "Feet"
    I = which(str_detect(comps[i,], "Feet|ft.|Ft."))
    
    # select the string following and convert to numeric
    rustic_stage[i] = as.numeric(comps[i,min(I)-1])
    
  }
  # remove special characters EXCEPT period
  title_content <- stringi::stri_replace_all_fixed(
    titles, 
    c("{", "}", "~", "[", "]", "(", ")","!", "@", "<", ">"),
    c("", "", "", "", "","", "","", "", "", ""),
    vectorize_all = FALSE
  )
  
  pine_stage <- str_extract(title_content, "\\d+\\.*\\d*") %>% 
    as.numeric()
  
  # convert post date to Date class
  date = as.Date(dates, format = "%B %d %Y")
  
  data.frame(
    title        = titles,
    date         = date,
    Pineview     = pine_stage,
    Hewlett      = hewlett_stage,
    Rustic       = rustic_stage,
    page         = n,    # page         = page[8],
    categories   = categories
  )
}