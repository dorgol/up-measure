'%ni%' <- Negate('%in%')

# arrange test data to select the last assessment attempted
last_assessment <- test %>% 
  filter(type == "Assessment") %>% 
  arrange(installation_id, desc(timestamp)) %>% 
  distinct(installation_id, .keep_all = T)

a <- full_join(temp, select(assessment_test, installation_id, game_session, event_code),
               by = c("installation_id", "game_session"))

b <- a[is.na(a$event_code.y),]
last_assessment$game_session %in% b$game_session %>% sum()

d <- b[b$game_session %ni% last_assessment$game_session,]  

assessment_test[assessment_test$game_session %in% d$game_session,]

e <- a[a$game_session %ni% d$game_session & a$game_session %ni% b$game_session,]


