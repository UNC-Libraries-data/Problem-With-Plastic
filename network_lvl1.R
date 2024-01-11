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
links <- tibble(from = "Foam Food Container", to = lvl1)

# Preprocess ----

# Create node list
nodes <- links$from %>% 
  append(links$to) %>%
  unique() %>% 
  as_tibble() %>% 
  rename(name = value) %>%
  rowid_to_column("node_id") %>% 
  mutate(node_id = node_id -1)

# Add ids to links
netlinks <- left_join(links, nodes, by = c("from" = "name")) %>% 
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
             fontSize = 40,
             fontFamily = "Helvetica",
             linkColour = "#cccccc",
             opacity = 0.8,
             zoom = TRUE)



