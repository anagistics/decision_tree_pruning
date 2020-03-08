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


#' Join confusion information to data.frame with tree node data
#'
#' The following function expects a `data.frame` with at least these three columns:
#' C: the class
#' P: the number of positive instances
#' N: the number of negative instances
#' 
#' @param treeframe 
#'
#' @return the \code{data.frame} with confusion information in columns TP, TN, FP, FN
#' @export
#'
join_confusion_info <- function(treeframe) {
  treeframe %>% mutate(TP = if_else(C == 1, P, 0), 
                       TN = if_else(C == 1, 0, N), 
                       FP = if_else(C == 1, N, 0),
                       FN = if_else(C == 1, 0, P))  
}