#######################################################################
# arulesViz - Visualizing Association Rules and Frequent Itemsets
# Copyrigth (C) 2011 Michael Hahsler and Sudheer Chelluboina
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


# FIXME: Implement max
# FIXME: size and color...

visNetwork_arules_group <- function(x, groups, measure = "support", shading = "lift", 
  control=NULL, ...) {
  
  if(class(x) != "rules") stop("Only implemented for rules!")
  
  control <- c(control, list(...))  
  
  control <- .get_parameters(control, list(
    #nodeColors = .nodeColors(
    #  if(!is.null(control$alpha)) control$alpha else .5),
    itemCol = hcl(h = 260),
    nodeCol = default_colors(100, alpha = 0),
    #edgeCol = grey_hcl(1, alpha = 0),
    #alpha = .5,
    #cex = 1,
    #itemLabels = TRUE,
    #labelCol = hcl(l=0, alpha = .7),
    #measureLabels = FALSE,
    precision = 3,
    #type = "items",
    #layout = NULL,
    igraphLayout = "layout_nicely",
    #layoutParams = list(),
    #arrowSize = .5,
    interactive = TRUE,
    engine = "visNetwork", 
    max = 100,
    selection_menu = TRUE,
    degree_highlight =1
    #,
    #plot = TRUE
  ))
  
  itemNodes <- which(itemFrequency(items(generatingItemsets(x)), 
    type="absolute") >0)
  
  lhs <- LIST(lhs(x), decode=FALSE)
  rhs <- LIST(rhs(x), decode=FALSE)
  
  itemNodes <- unique(c(unlist(lhs), unlist(rhs)))
  ruleNodes <- paste("r", 1:length(x), sep='')
  
  nodeLabels <- c(itemLabels(x)[itemNodes], paste("rule", 1:length(ruleNodes))) #TODO: change node name
  
  allNodes <- factor(c(itemNodes, ruleNodes), levels = c(itemNodes, ruleNodes))
  nodeType <- c(rep("item", length(itemNodes)), rep("rule", length(ruleNodes)))
  
  from_lhs <- match(unlist(lhs), allNodes)
  to_lhs <- rep(1:length(x), sapply(lhs, length)) + length(itemNodes)
  
  to_rhs <- match(unlist(rhs), allNodes)
  from_rhs <- rep(1:length(x), sapply(rhs, length)) + length(itemNodes)
  
  titleRules <- paste0('<B>[',1:length(x),']</B><BR>',
    labels(x, itemSep= ',<BR>&nbsp;&nbsp;', 
      ruleSep = '<BR>&nbsp;&nbsp; => ', 
      setStart = '<B>{', setEnd = '}</B>'),
    "<BR><BR>", 
    apply(quality(x), MARGIN = 1, 
      FUN = function(x) paste(names(x), "=", signif(x,control$precision), collapse = "<BR>")))
  
  title <- c(itemLabels(x)[itemNodes], titleRules)
  
  s <- quality(x)[[measure]] #TODO: change node size
  size <- rep(1, length(nodeType))
  size[nodeType == "rule"] <- map(s, c(1, 100)) 
  
  if(!is.na(shading)) {
    colors <- list(weight_group_1=0,weight_group_2=50,weight_group_3=100,weight_group_4=200,
                  weight_group_5=300,weight_group_6=360,weight_group_7=-50,weight_group_8=-100,
                  weight_group_9=-150,weight_group_10=-200)
    
    colors <- lapply(colors, function(y) sequential_hcl(n=8,h=y,c=c(86,NA,NA),l=c(50,100),
                                                        power=1.3,rev=T,register=))
    
    rule_quality <- quality(x)
    rule_quality[['max_group']] <- groups[apply(quality(x)[groups], 1, which.max)] #make this disc max
    rule_quality[['num_groups']] <- apply(quality(x)[groups], 1, function(x) sum(x>0))
    rule_quality[['color']] <- 0
    current_rules <- subset(x, subset=quality(x)$confidence > 1)
    lhs_overlap <- c()
    rhs_overlap <- c()
    for(group in groups) {
      group_rules <- subset(x, subset=quality(x)[[group]] > 0 & rule_quality[['max_group']] == group)
      if( length(group_rules) == 0) {
        next
      }
      group_indices <- as.numeric(rownames(quality(group_rules)))
      color_indices <- as.integer(cut(quality(group_rules)[[group]],7))+1
      group_colors <- colors[[group]][color_indices]
      rule_quality[as.character(group_indices),'color'] <- group_colors
      
      lhs_overlap <- c(lhs_overlap,overlapping_lhs(current_rules,group_rules))
      rhs_overlap <- c(rhs_overlap,overlapping_rhs(current_rules,group_rules))
      current_rules <- arules::union(current_rules,group_rules)
      #rule_quality[['color']] <- group_colors
    }
    node_colors <- rule_quality$color
    node_shapes <- c('circle',rep('diamond',9))[rule_quality$num_groups]
    
    
    
    item_node_colors <- rep(hcl(h=260), length(itemNodes))
    names(item_node_colors) <- itemLabels(x)[itemNodes]
    overlapping_nodes <- c(lhs_overlap,rhs_overlap)
    item_node_colors[overlapping_nodes] <- rgb(1,0,0)
    
    
    color <- c(item_node_colors,node_colors)
    node_shapes <- c(rep("box", length(itemNodes)),node_shapes)
      #.col_picker(map(s, c(0.9,0.1)), control$nodeCol)) 
  } else v.color <- c(rep(control$itemCol[1], length(itemNodes)),
    .col_picker(rep(.5, length(x)), control$nodeCol)) 

  nodes <- data.frame(
    id = as.integer(allNodes), 
    label = nodeLabels, 
    group = nodeType,
    value = size,
    color = color,
    title = title,
    shape = node_shapes
  )
  
  edges <- data.frame(
    from = c(from_lhs, from_rhs), 
    to = c(to_lhs, to_rhs), 
    arrows = "to") 
  
  visNetwork(nodes = nodes, edges = edges) %>% 
    visNodes(scaling = list(label = list(enabled = TRUE))) %>%
    visIgraphLayout(layout = control$igraphLayout) %>%
    visOptions(highlightNearest = 
        list(enabled = TRUE, degree = control$degree_highlight, hover = TRUE), 
      nodesIdSelection = control$selection_menu
      )
}