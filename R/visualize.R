get_used_rules <- function(rules, weights) {
 return(rules[Matrix::rowSums(weights)>0]) 
}

visualize <- function(model) {
  W <- model$first_weights
  W2 <- model$second_weights
  rules <- get_used_rules(model$original_rules,W)
  firstNodeIds <- 1:dim(W)[1]
  firstNodeLabels <- paste(c(as.character(inspect(rules)$lhs)),"=>",c(as.character(inspect(rules)$rhs)))
  secondNodeIds <- 1:dim(W)[2]
  secondNodeLabels <- paste("Layer 2 Node",secondNodeIds)
  secondNodeIds <- secondNodeIds + dim(W)[1]
  thirdNodeIds <- 1:dim(W2)[2]
  thirdNodeLabels <- paste("Layer 3 Node",thirdNodeIds)
  thirdNodeIds <- thirdNodeIds + dim(W)[1] + dim(W)[2]
  fromEdges <- c(rep(1:dim(W)[1],dim(W)[2]),rep(1:dim(W2)[1],dim(W2)[2])+dim(W)[1])
  toEdges <- c(sort(rep(1:dim(W)[2],dim(W)[1])+dim(W)[1]),sort(rep(1:dim(W2)[2],dim(W2)[1])+dim(W)[1]+dim(W)[2]))
  edgeLabels <- c(c(W),c(W2))
  edgeLabels <- round(edgeLabels,2)
  fromEdges <- fromEdges[as.logical(edgeLabels)]
  toEdges <- toEdges[as.logical(edgeLabels)]
  edgeLabels <- edgeLabels[as.logical(edgeLabels)]
  nodeIds <- c(firstNodeIds,secondNodeIds,thirdNodeIds)
  nodeLabels <- c(firstNodeLabels,secondNodeLabels,thirdNodeLabels)
  nodesUsed <- unique(c(toEdges,fromEdges))
  nodesToKeep <- nodeIds %in% nodesUsed
  nodeIds <- nodeIds[nodesToKeep]
  nodeLabels <- nodeLabels[nodesToKeep]
  nodes <- data.frame(id = nodeIds, title=nodeLabels)
  edges <- data.frame(from = fromEdges, to = toEdges, label = edgeLabels)
  visNetwork(nodes, edges, width = "100%", height = "100%") %>% 
    visEdges(arrows="to", color=list(highlight="red",hover="red")) %>% 
    visHierarchicalLayout(direction="LR", sortMethod = "directed") %>%
    visOptions(highlightNearest = list(enabled =TRUE, degree = 1))
}

add_rule_groups <- function(model) {
  rules <- model$original_rules
  W <- model$first_weights
  for(group in 1:dim(W)[2]) {
    quality(rules)[[paste0('weight_group_',group)]] = W[,group]
  }
  return(get_used_rules(rules,W))
}

select_rules_for_group <- function(rules, group) {
  return(rules[quality(rules)[[paste0('weight_group_',group)]]>0])
}

rule_stats <- function(rules) {
  print(rules)
  print('RHS:')
  sparseRep <- as(rhs(rules),'ngCMatrix')
  print(sort(Matrix::rowSums(sparseRep)[Matrix::rowSums(sparseRep)>0],decreasing=T))
  print('LHS:')
  sparseRep <- as(lhs(rules),'ngCMatrix')
  print(sort(Matrix::rowSums(sparseRep)[Matrix::rowSums(sparseRep)>0],decreasing=T))
}


overlapping_lhs <- function(rules1, rules2) {
  sparseRep <- as(lhs(rules1),'ngCMatrix')
  lhs1 <- sort(Matrix::rowSums(sparseRep)[Matrix::rowSums(sparseRep)>0],decreasing=T)
  sparseRep <- as(lhs(rules2),'ngCMatrix')
  lhs2 <- sort(Matrix::rowSums(sparseRep)[Matrix::rowSums(sparseRep)>0],decreasing=T)
  base::intersect(names(lhs1),names(lhs2))
}

overlapping_rhs <- function(rules1, rules2) {
  sparseRep <- as(rhs(rules1),'ngCMatrix')
  rhs1 <- sort(Matrix::rowSums(sparseRep)[Matrix::rowSums(sparseRep)>0],decreasing=T)
  sparseRep <- as(rhs(rules2),'ngCMatrix')
  rhs2 <- sort(Matrix::rowSums(sparseRep)[Matrix::rowSums(sparseRep)>0],decreasing=T)
  base::intersect(names(rhs1),names(rhs2))
}

comparative_rule_stats <- function(rules1, rules2) {
  print(rules1)
  print(rules2)
  print("Overlapping rules:")
  print(inspect(arules::intersect(rules1,rules2)))
  print('Overlapping RHS:')
  print(overlapping_rhs(rules1,rules2))
  print('Overlapping LHS:')
  print(overlapping_lhs(rules1,rules2))
}


group_itemset_stats <- function(all_rules, group_names, classifier, type = 'group') {
  rules <- get_used_rules(all_rules, quality(all_rules)[group_names])
  if(type == 'group') {
    item_weights <- (t(as(items(rules),'matrix')*1.0))%*%as(quality(rules)[group_names],'matrix')
  } else if(type == 'class') {
    print(as(quality(rules)[group_names],'matrix'))
    print(classifier$second_weights[group_names,])
    class_weights <- (as(quality(rules)[group_names],'matrix') %*% classifier$second_weights[group_names,])
    print(class_weights)
    item_weights <- (t(as(items(rules),'matrix')*1.0))%*% class_weights
  }
  group_total_weights <- apply(quality(rules)[group_names],2,sum)
  group_total_weights <- sweep(item_weights, 2, group_total_weights, '/') #divide row-wise by group total weights
  print(group_total_weights)
  return(data.frame(items=row.names(group_total_weights),group_total_weights))
}


normalized_group_weights <- function(classifier, trans) {
  group_weights <- group_weights.CWAR(classifier,trans)
  weight_cols <- head(colnames(group_weights),-1)
  group_weights[weight_cols] <- apply(group_weights[weight_cols],2,function(x) x/(max(x)+1e-10))
  return(group_weights)
}
