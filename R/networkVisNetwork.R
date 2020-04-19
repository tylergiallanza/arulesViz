visNetwork_arules_network <- function(classifier, rules, groups) {
  
  colors <- list(weight_group_1=0,weight_group_2=50,weight_group_3=100,weight_group_4=200,
                weight_group_5=300,weight_group_6=360,weight_group_7=-50,weight_group_8=-100,
                weight_group_9=-150,weight_group_10=-200)
  colors <- lapply(colors, function(y) sequential_hcl(n=8,h=y,c=c(86,NA,NA),l=c(50,100),
                                                      power=1.3,rev=T,register=))
  
  rule_quality <- quality(rules)
  rule_quality[['max_group']] <- groups[apply(quality(rules)[groups], 1, which.max)] #make this disc max
  rule_quality[['num_groups']] <- apply(quality(rules)[groups], 1, function(x) sum(x>0))
  rule_quality[['color']] <- 0
  
  from_rules <- c()
  to_groups <- c()
  keep_groups <- c()
  for(group in groups) {
    group_rules <- subset(rules, subset=quality(rules)[[group]] > 0 & rule_quality[['max_group']] == group)
    if(length(group_rules)>0) {
      group_indices <- as.numeric(rownames(quality(group_rules)))
      color_indices <- as.integer(cut(quality(group_rules)[[group]],7))+1
      group_colors <- colors[[group]][color_indices]
      rule_quality[as.character(group_indices),'color'] <- group_colors
    }
    
    group_rules <- subset(rules, subset=quality(rules)[[group]] > 0)
    if(length(group_rules)>0) {
      rules_for_group <- which(rule_quality[[group]]>0)
      from_rules <- c(from_rules, rules_for_group)
      to_groups <- c(to_groups,rep(length(keep_groups)+1,length(rules_for_group)))
      keep_groups <- c(keep_groups, substring(group,8))
    }
  }
  rule_colors <- rule_quality$color
  rule_shapes <- c('circle',rep('diamond',9))[rule_quality$num_groups]
  rule_titles <- paste(c(as.character(inspect(rules)$lhs)),"=>",c(as.character(inspect(rules)$rhs)))
  rule_labels <- unlist(lapply(1:length(rules),function(x) paste0('Rule ',x)))
  #need: labels, color, title, shape
  #need: connections
  
  #from_rules <- which(from_rules==rownames(quality(rules)))
  to_groups <- to_groups+length(rules)
  print('from_rules')
  print(from_rules)
  print('to_groups')
  print(to_groups)
  ids <- 1:(length(rules)+length(keep_groups))
  sizes <- rep(1,length(ids))
  titles <- c(rule_titles,keep_groups)
  labels <- c(rule_labels,keep_groups)
  colors <- c(rule_colors,rep(hcl(h=260),length(keep_groups)))
  shapes <- c(rule_shapes,rep('circle',length(keep_groups)))
  
  nodes <- data.frame(
    id = ids,
    label = labels, 
    #group = nodeType,
    value = sizes,
    color = colors,
    title = titles,
    shape = shapes
  )
  
  edges <- data.frame(
    from = from_rules,
    to = to_groups,
    arrows = "to") 
  
  visNetwork(nodes = nodes, edges = edges) %>% 
    visNodes(scaling = list(label = list(enabled = TRUE))) %>%
    visHierarchicalLayout(direction="LR", sortMethod = "directed") %>%
    visOptions(highlightNearest = 
        list(enabled = TRUE, degree = 1, hover = TRUE) 
      )
}