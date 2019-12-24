   

train %<>% 
  mutate(is_assessment = if_else(condition = (event_code == 4110 & 
                                                title == "Bird Measurer (Assessment)"), 
                                 true = "T",
                                 false = if_else(condition = event_code == 4100 &
                                                   type == "Assessment" &
                                                   title != "Bird Measurer (Assessment)",
                                                 true = "T", 
                                                 false = "F"))) 

train %<>% 
  mutate(success = if_else(grepl(pattern = 'correct\":true', x = event_data,
                                 ignore.case = T) & is_assessment == "T",
                           true = "positive", 
                           false = if_else(is_assessment == "T" &
                                             grepl(pattern = 'correct\":false',
                                                   x =  event_data, ignore.case = T),
                                           true = "negative", false = "other"))) 





train_last_assessment <- sample_train %>% 
  filter(type == "Assessment" & event_code == 2000) %>% 
  arrange(installation_id, desc(timestamp)) %>% 
  distinct(installation_id, .keep_all = T)

labels_manual <- train %>% 
  group_by(game_session, success, title, installation_id) %>% 
  summarise(count = n()) %>% 
  filter(success != "other") %>% 
  spread(key = "success", value = "count") %>% 
  replace_na(list(negative = 0, positive = 0)) %>% 
  filter(positive != 0 | negative != 0) %>% 
  mutate(accuracy = positive/(negative+positive),
         accuracy_group = cut(accuracy, breaks = c(-0.1, 0.00000001,0.49, 0.51, 1.01),
                              labels = c(0,1,2,3)))




