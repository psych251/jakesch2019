# Rather than encoding the difference between the primed and control ondition using slopes (as in Fig. 1)
# it is more effective to use position along a common scale
# however, I did not have time to finish this before the project deadline

item_level_boot = function (data) {
  result = bootES::bootES(data,
                          data.col = "normalized_score", 
                          group.col = "condition",
                          contrast = c("primed", "control"))
  return(paste(result$t0, paste(result$bounds, collapse = '')))
} 

#TODO make item_level_boot take in a subset of the data (right now it calculates the bootstrap for the whole dataset each time)
processed_data_item_level <- processed_data %>% 
  group_by(profile_number) %>% summarize(boot = item_level_boot(.))

original_data_item_level <- original_processed_data %>% filter(condition %in% c("control","primed") %>% group_by(ResponseId) %>% mutate(meanRating = mean(rating)) %>% ungroup %>% mutate(normalized_score = rating - meanRating)  
                                                               
item_level_plot <- function(data) {
 ggplot(data, aes(x = profile_number, y = normalized_score)) + 
   stat_summary( + 
                   theme_cowplot() + 
                   theme(axis.text.x = element_text(angle = 45,  hjust = 1))
}
                                                               
item_level_plot(processed_data_item_level)
item_level_plot(orgina_data_item_level)