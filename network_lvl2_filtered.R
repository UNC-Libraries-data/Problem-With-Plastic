library(WikipediR)
library(tidyverse)
library(networkD3)

# First Level Links ----

# api query for first level links
lvl1_raw <- page_links("en","wikipedia", page = "foam food container", limit = 500)

# flatten and clean up results
lvl1 <- unlist(lvl1_raw$query$pages[[1]]$links) %>% 
  str_subset("\\d|:", negate = TRUE)

# add first level links to dataframe and create a column for categories
links <- tibble(from = "Foam Food Container", to = lvl1) %>% 
  mutate(categories = "")

# get total first level links
lvl1len <- length(lvl1)

# First Level Categories ----

# Get categories for first level links
for (n in 1:lvl1len) {
  
  # get title of  page
  title = links$to[n]
  
  # api query for first level categories
  cats_raw  <- categories_in_page("en","wikipedia", pages = title, limit = 500)
  
  # if page has no categories, try to figure out if it's missing or a redirect
  if (is.null(cats_raw$query$pages[[1]]$categories)) {
    
    # get basic info on the page being queried
    info <- page_info("en","wikipedia", page = title)
    
    # check to see if title comes up missing
    if (!is.null(info$query$pages[[1]]$missing)) {
      
      #if it's missing, try using lowercase title
      title <- tolower(title)
      cats_raw  <- categories_in_page("en","wikipedia", pages = title, limit = 500)
      info <- page_info("en","wikipedia", page = title)
      
    }
    
    # check to see if page is a redirect
    if (!is.null(info$query$pages[[1]]$redirect)) {
      
      #if it's a redirect, get the page content which includes redirect title
      rd <- page_content("en","wikipedia", page_name = title, as_wikitext = TRUE)
      
      # pull redirect title from page content
      rdtitle <- rd[["parse"]][["wikitext"]][["*"]]
      rdtitle <- str_split(rdtitle, "\\[\\[", simplify = TRUE)[2]
      rdtitle <- str_split(rdtitle, "\\]\\]", simplify = TRUE)[1]
      
      # query redirect title for categories
      cats_raw <- categories_in_page("en","wikipedia", pages = rdtitle, limit = 500)
      
    }
    
  }
  
  # clean up categories
  cats <- unlist(cats_raw$query$pages[[1]]$categories) %>% 
    str_subset("Category:") %>% 
    str_replace("Category:", "") %>% 
    paste(collapse = "; ")
  
  # add categories to links
  links$categories[n] <- cats
  
}

# Create list of categories
allcats <- as.vector(links$categories) %>% 
  paste(collapse = "; ") %>% 
  str_split("; ", simplify = TRUE) %>% 
  as.vector() %>%
  unique() %>%
  sort()

# choose a category to filter by
catselect = "Containers"

# Filter first level links by selected category
flinks <- filter(links, grepl(catselect, categories)) %>% 
  select(-categories)

# Second Level Links ----

for(p in 1:nrow(flinks)) {

  # get title of linked page
  title = flinks$to[p]

  # api query for second level links
  lvl2_raw  <- page_links("en","wikipedia", page = title, limit = 500)

  # if page isn't found, try using lower case
  if (!is.null(lvl2_raw$query$pages[[1]]$missing)) {
    lvl2_raw  <- page_links("en","wikipedia", page = tolower(title), limit = 500)
  }

  # flatten and clean up results
  lvl2 <- unlist(lvl2_raw$query$pages[[1]]$links) %>% 
    str_subset("\\d|:", negate = TRUE)

  # create dataframe for second level links
  df <- tibble(from = title, to = lvl2)

  # add second level links to main dataframe
  flinks <- rbind(flinks, df)

}
# Preprocess ----

# Create node list
nodes <- flinks$from %>% 
  append(flinks$to) %>%
  unique() %>% 
  as_tibble() %>% 
  rename(name = value) %>%
  rowid_to_column("node_id") %>% 
  mutate(node_id = node_id -1)

# Add ids to links
netlinks <- left_join(flinks, nodes, by = c("from" = "name")) %>% 
  rename(source = node_id ) %>% 
  left_join(nodes, by = c("to" = "name")) %>%
  rename(target = node_id)

# Add group and node size column to nodes
nodes <- mutate(nodes, "group" = "none") %>% 
  mutate(node_size = 10)

# make node for main page stand out
nodes$node_size[1] <- 1000
nodes$group[1] <- "main"

# Network ----

forceNetwork(Links = netlinks, 
             Nodes = nodes, 
             Source = "source", 
             Target = "target", 
             NodeID = "name",
             Nodesize = "node_size",
             Group = "group",
             colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),
             charge = -1000,
             fontSize = 100,
             fontFamily = "Helvetica",
             linkColour = "#cccccc",
             opacity = 0.8,
             zoom = TRUE)



