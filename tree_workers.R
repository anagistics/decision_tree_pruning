library(magrittr)
library(dplyr)


parent <- function(x) ifelse(x %% 2 == 0, x/2, (x-1)/2)

all_children_of_node <- function(tree_frame, node_num) {
  all_nodes <- tree_frame %>% rownames() %>% as.integer()
  lnode <- node_num * 2
  rnode <- lnode + 1
  lchilds <- c()
  rchilds <- c()
  if(lnode %in% all_nodes) {
    lchilds <- c(lnode, all_children_of_node(tree_frame, lnode))
  } 
  if(rnode %in% all_nodes) {
    rchilds <- c(rnode, all_children_of_node(tree_frame, rnode))
  } 
  c(lchilds, rchilds)
}


