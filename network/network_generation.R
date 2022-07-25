## StrainHub Network Generation

## Load in Libraries
library(strainhub)
library(visNetwork)
library(dplyr)
library(tidyr)
library(readr)
# library(randomcoloR)

### Galvan 16 - 54 Taxa
## Read in tree, metadata, and geodata
treedata <- ape::read.tree("../tree/RAxML_bestTree.MSA_full.tre")
metadata <- readr::read_tsv("gisaid_pox_2022_07_25_00.tsv", col_names = TRUE) %>% 
  rename(Accession = `Accession ID`) %>% 
  mutate(`Collection date` = as.Date(`Collection date`)) %>% 
  separate(Location, into = c("Continent","Country", "State", "City"), sep = " / ")

contintent_countries <- metadata %>% select(Continent, Country) %>% unique()


## Make the Transmission Network
# selected_meta = "Continent"
# full_graph <- makeTransNet(treedata,
#                            metadata,
#                            columnSelection = selected_meta,
#                            centralityMetric = 6,
#                            treeType = "parsimonious")
# 
# full_graph



## 2022 Graph

### By Continent
metadata_pre2022 <- metadata %>% filter(`Collection date` < '2022-01-01')
# metadata_2022 <- metadata %>% filter(`Collection date` >= '2022-01-01')
treedata_2022 <- treedata %>% ape::drop.tip(., metadata_pre2022$Accession)

sub_graph_2022_continents <- makeTransNet(treedata_2022,
                           metadata,
                           columnSelection = "Continent",
                           centralityMetric = 6,
                           treeType = "parsimonious")
sub_graph_2022_continents

### By Country
sub_graph_2022_countries <- makeTransNet(treedata_2022,
                                          metadata,
                                          columnSelection = "Country",
                                          centralityMetric = 6,
                                          treeType = "parsimonious")
sub_graph_2022_countries


## Convert Graph info to DataFrame
sub_graph_2022_countries_df <- sub_graph_2022_countries$x$edges %>% 
  inner_join(sub_graph_2022_countries$x$nodes, by=c('from'='id'), suffix = c("", "_from")) %>% 
  inner_join(sub_graph_2022_countries$x$nodes, by=c('to'='id'),  suffix = c("", "_to")) %>% 
  mutate(transmission = paste0(label,">", label_to))


continent_colors <- readr::read_csv("Continent_Colors.csv")


sub_graph_2022_nodes <- sub_graph_2022_countries$x$nodes %>%
  mutate(shape = "dot",
         font.size = 20) %>% 
  inner_join(contintent_countries, by = c("label" = "Country")) %>% 
  inner_join(continent_colors, by = c("Continent" = "Continent")) %>% 
  select(!Continent)



sub_graph_2022_edges <- sub_graph_2022_countries_df %>% 
  mutate(arrows = "to",
         smooth = TRUE,
         color = "grey",
         width = value) %>% 
  select(from, to, arrows, smooth, color, width)


sub_graph_2022_countries_modified <- visNetwork(sub_graph_2022_nodes, sub_graph_2022_edges) %>%
  visOptions(nodesIdSelection = list(enabled = TRUE))

sub_graph_2022_countries_modified



### By US State

metadata_2022_us <- metadata %>%
  filter(`Collection date` >= '2022-01-01') %>% 
  filter(Country == "USA")
metadata_pre2022_us <- metadata %>% filter(!Accession %in% metadata_2022_us$Accession)
treedata_2022_us <- treedata %>% ape::drop.tip(., metadata_pre2022_us$Accession)

sub_graph_2022_states <- makeTransNet(treedata_2022_us,
                                      metadata_2022_us,
                                      columnSelection = "State",
                                      centralityMetric = 6,
                                      treeType = "parsimonious")
sub_graph_2022_states
