library(WikipediR)
library(tidyverse)
library(networkD3)

# function to retrieve network data from a wiki category
wikiCatNet <- function(category, levels = 2, limit = 500, project = "wikipedia") {
  
  # query a category
  cats_raw <- pages_in_category(language = "en", 
                                project = project, 
                                categories = category, 
                                limit = limit,
                                properties = c("title", "type"))
  
  # get list of pages and subcats
  cats_raw <- cats_raw$query$categorymembers
  
  # map list onto a df
  cats_df <- tibble(
    to = map_chr(cats_raw, "title"),
    type = map_chr(cats_raw, "type")
  )
  
  # create "from" column
  cats_df$from <- paste("Category:", category, sep = "")
  
  # create network df
  cats_net <- cats_df %>% 
    select(from, to, type)
  
  # keep track of categories queried
  cats_got <- c(paste("Category:", category, sep = ""))
  
  # check to see if there are subcategories
  if (!("subcat" %in% cats_net$type)) {
    msg <- paste("Warning: No subcategories found in ", category, ".", sep ="")
    message(msg)
    return(cats_net)
  }
  
  message("Crawling subcategories...")
  
  for (l in 1:(levels - 1)) {
    
    # select subcats
    subcats_df <- filter(cats_net, type == "subcat")
    
    # remove subcats already queried
    if (l > 1) {
      subcats_df <- filter(subcats_df, to %in% cats_got == FALSE)
    }
    
    # check to see if there are any subcategories left
    if (nrow(subcats_df) == 0) {
      msg <- paste("Warning: No subcategories found in ", qtitle, ".", sep ="")
      message(msg)
      return(cats_net)
    }
    
    # add another level
    for (c in 1:nrow(subcats_df)) {
      
      # get subcat title
      title <- subcats_df$to[c]
      qtitle <- str_split(title, "Category:", simplify = TRUE)[2]
      
      # display subcats being crawled
      message(qtitle)

      # query subcat
      subcats_raw <- pages_in_category(language = "en", 
                                       project = project, 
                                       categories = qtitle, 
                                       limit = limit,
                                       properties = c("title", "type"))
      
      # get list of pages and subcats
      subcats_raw <- subcats_raw$query$categorymembers
      
      # map list onto a df
      newcats_df <- tibble(
        to = map_chr(subcats_raw, "title"),
        type = map_chr(subcats_raw, "type")
      )
      
      # add from column
      newcats_df$from <- title
      
      # add to network
      cats_net <- rbind(cats_net, newcats_df)
      
      # keep track of categories queried
      cats_got <- c(cats_got, title)
      
    }
    
  }
  
  return(cats_net)
}

# get network data for Food storage and single serve categories
storage <- wikiCatNet("Food storage containers", levels = 2)
singleserve <- wikiCatNet("Single-serve containers", levels = 2)

# combine links
links <- rbind(storage, singleserve)

# create nodes
nodes <- links$from %>% 
  append(links$to) %>%
  unique() %>% 
  as_tibble() %>% 
  rename(title = value) %>%
  rowid_to_column("node_id") %>% 
  mutate(node_id = node_id - 1)

# create links for viz
netlinks <- select(links, -type)
netlinks <- left_join(netlinks, nodes, by = c("from" = "title")) %>% 
  rename(source = node_id ) %>% 
  left_join(nodes, by = c("to" = "title")) %>%
  rename(target = node_id)

# Add node size and type to nodes
nodes <- mutate(nodes, node_size = 10) %>% 
  mutate(type = ifelse(str_detect(title, "Category:"), 
                       "Wikipedia category", 
                       "Wikipedia page"))

# make node for main page stand out
nodes <- nodes %>% 
  mutate(node_size = ifelse(title == "Foam food container", 
                                         10, 
                                         node_size)) %>% 
  mutate(type = ifelse(title == "Foam food container", 
                                         "Foam food container", 
                                         type))

# Network ----

forceNetwork(Links = netlinks, 
             Nodes = nodes, 
             Source = "source", 
             Target = "target", 
             NodeID = "title",
             Nodesize = "node_size",
             Group = "type",
             colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),
             #charge = -1000,
             fontSize = 16,
             fontFamily = "Helvetica",
             #linkColour = "#cccccc",
             opacity = 0.8,
             zoom = TRUE,
             legend = TRUE)
