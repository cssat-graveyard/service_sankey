# Author: Brian Waismeyer
# Contact: bwaismeyer@gmail.com

# Date created: 7/15/2015
# Date updated:

###############################################################################
## SCRIPT OVERVIEW

# GOAL: service_sankey.R is a script designed to generate a prototype
#       Sankey diagram of movement through the Washington child welfare system.
#
#       The prototype makes no effort to automate the data collection or
#       Sankey generation - it is simply a rough sketch of a Sankey to assess
#       if a more automated, sustainable Sankey should be built.
#
#       The prototype makes use of rCharts to handle the Sankey generation,
#       following the example given here:
#       http://timelyportfolio.github.io/rCharts_d3_sankey/example_build_network_sankey.html
#
# SCRIPT OUTLINE:
# - Load Supporting Packages/Scripts
# - Hard Code the Sankey Data
# - Generate the Sankey

###############################################################################
## Load Supporting Packages/Scripts

library(rCharts)

###############################################################################
## Hard Code the Sankey Data

service_flow <- list(
    "Referrals" = list(
        "Screened In" = 44813,
        "Screened Out" = 90000 - 44813
    ),
    
    "Screened In" = list(
        "CPS" = 31310,
        "ARS" = 3895,
        "FRS" = 4678,
        "Risk Only" = 3342,
        "CFWS" = 644,
        "Other" = 44813 - (31310 + 3895 + 4678 + 3342 + 644)
    ),
    
    "CPS" = list(
        "Involuntary OOH" = 5898 - 289,
        "Voluntary OOH" = 289,
        "Case Closed or Other Services" = 31310 - 5898
    ),
    
    "ARS" = list(
        "Involuntary OOH" = 79 - 12,
        "Voluntary OOH" = 12,
        "Case Closed or Other Services" = 3895 - 79
    ),
    
    "FRS" = list(
        "Involuntary OOH" = 95 - 15,
        "Voluntary OOH" = 15,
        "Case Closed or Other Services" = 4678 - 95
    ),
    
    "Risk Only" = list(
        "Involuntary OOH" = 1126 - 42,
        "Voluntary OOH" = 42,
        "Case Closed or Other Services" = 3342 - 1126
    ),
    
    "CFWS" = list(
        "Involuntary OOH" = 109 - 25,
        "Voluntary OOH" = 25,
        "Case Closed or Other Services" = 644 - 109
    ),
    
    "Involuntary OOH" = list(
        "Reunification" = 9.39,
        "Adoption" = 0.34,
        "Guardianship" = 0.68,
        "Emancipation" = 0.65,
        "Other" = 0.61,
        "Still in OOH" = 88.33
    ),
    
    "Voluntary OOH" = list(
        "Reunification" = 78.75,
        "Adoption" = 0.48,
        "Guardianship" = 3.74,
        "Emancipation" = 1.01,
        "Other" = 2.02,
        "Still in OOH" = 13.99
    ),
    
    "Case Closed or Other Services" = list(
        "Previously Investigated and/or Placed" = 1
    ),
    
    "Reunification" = list(
        "Previously Investigated and/or Placed" = 1
    ),
    
    "Adoption" = list(
        "Previously Investigated and/or Placed" = 1
    ),
    
    "Guardianship" = list(
        "Previously Investigated and/or Placed" = 1
    ),
    
    "Previously Investigated and/or Placed" = list(
        "Re-Referral" = 1,
        "Re-Entry in OOH" = 1
    )
)

make_df <- function(flow_list) {
    row_set <- c()
    
    for(lin in 1:length(service_flow)) {
        current_source <- names(service_flow)[lin]
        
        for(rin in 1:length(service_flow[[lin]])) {
            current_row <- service_flow[[lin]][rin]
            current_target <- names(current_row)
            current_value <- current_row[[1]]
            
            row_set <- rbind(row_set,
                             c(current_source, 
                               current_target, 
                               current_value)
                             )
        }
    }
    
    df <- data.frame(row_set, stringsAsFactors = FALSE)
    names(df) <- c("source", "target", "value")
    df$value <- as.numeric(df$value)
    
    return(df)
}

service_df <- make_df(service_flow)

flip_to_proportions <- function(flow_df) {
    # get unique sources
    us <- unique(flow_df$'source')
    
    # for each unique source...
    for(index in 1:length(us)) {
        # current source name
        current_source <- us[index]
        
        # get the indices for source rows
        row_in <- which(flow_df$'source' == current_source)
        
        # sum the values of those rows
        source_sum <- sum(flow_df$value[row_in])
        
        # divide to get proportions
        flow_df$value[row_in] <- flow_df$value[row_in]/source_sum
    }
    
    return(flow_df)
}

service_df <- flip_to_proportions(service_df)

# feed_props <- function(flow_df, top_node_num_pairs) {
#     for(index in 1:nrow(flow_df)) {
#         current_source <- flow_df$'source'[index]
#         if(current_source %in% flow_df$target) {
#             source_index <- which(flow_df$target == current_source)
#             source_value <- flow_df$value[source_index]
#             current_value <- flow_df$value[index]
#             
#             flow_df$value[index] <- source_value * current_value
#         } else {
#             source_index <- which(names(top_node_num_pairs) == current_source)
#             source_value <- top_node_num_pairs[[source_index]]
#             current_value <- flow_df$value[index]
#             
#             flow_df$value[index] <- source_value * current_value
#         }
#     }
#     
#     return(flow_df)
# }

feed_props <- function(flow_df, top_node_num_pairs) {
    browser()
    st <- unique(c(flow_df$'source', flow_df$target))
    values <- rep(0, length(st))
    
    for(index in 1:length(top_node_num_pairs)) {
        row_num <- which(st == names(top_node_num_pairs)[index])
        values[row_num] <- top_node_num_pairs[[index]]
    }
    
    for(index in 1:length(st)) {
        current_target <- st[index]
        if(!(current_target %in% names(top_node_num_pairs))) {
            target_indices <- which(flow_df$target == current_target)
            
            for(t in target_indices) {
                current_source <- flow_df$'source'[t]
                source_index <- which(st == current_source)
                source_value <- values[source_index]
                
                flow_df$value[t] <- flow_df$value[t] * source_value
            }
            
            values[index] <- sum(flow_df$value[target_indices])
        }
    }
    
    test <- data.frame(st, values, stringsAsFactors = FALSE)
    return(test)
}

###############################################################################
## Generate the Sankey
sankeyPlot <- rCharts$new()
sankeyPlot$setLib('libraries/widgets/d3_sankey')
sankeyPlot$setTemplate(script = "libraries/widgets/d3_sankey/layouts/chart.html")

sankeyPlot$set(
    data = service_df,
    nodeWidth = 15,
    nodePadding = 10,
    layout = 32,
    width = 960,
    height = 500
)

sankeyPlot
