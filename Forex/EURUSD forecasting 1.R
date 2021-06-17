pacman::p_load(pacman, dplyr, magrittr, vars, ecm, MuMIn, PerformanceAnalytics,dynlm, dyn, rio)
rm(list = ls())

# Preparing data

df <- import(file="clipboard")
df %<>% mutate(lag1 = lag(Y, differences = 1), 
               differ = lag1 - Y,
               bull = if_else(differ > 0, 1, 0),
               bull1 = lag(bull, 1),
               bull2 = lag(bull1, 1),
               bull3 = lag(bull2, 1),
               bull4 = lag(bull3, 1),
               bull5 = lag(bull4, 1),
               bull6 = lag(bull5, 1),
               bull7 = lag(bull6, 1),
               bull8 = lag(bull7, 1)) 

df2 <- df[-c(1:10),]



#skenario 1

df3 <- df2[,c(6,7,11,13,15)]

df3 %<>% mutate(bullish = case_when(bull==1 & bull4==0 & bull6==0 & bull8==0 ~ 1, 
                                    bull==0 & bull4==0 & bull6==0 & bull8==0 ~ 0,
                                    TRUE ~ NA_real_))
df3 %<>% mutate(bearish = case_when(bull==0 & bull4==1 & bull6==1 & bull8==1 ~ 1, 
                                    bull==1 & bull4==1 & bull6==1 & bull8==1 ~ 0,
                                    TRUE ~ NA_real_))

df3 %<>% mutate(differ = differ*10000)

bull_win <- df3 %>% filter(!is.na(bullish))
summary(bull_win$differ)
sum(bull_win$differ)

bear_win <- df3 %>% filter(!is.na(bearish))
summary(bear_win$differ)
sum(bear_win$differ)

total_win <- sum(bull_win$differ) + abs(sum(bear_win$differ))
total_win


#skenario 2

df2 %<>% mutate(bullish = case_when(bull==1 & bull1==1 ~ 1, 
                                    bull==0 & bull1==0 ~ 0,
                                    TRUE ~ NA_real_))
df2 %<>% mutate(bearish = case_when(bull==0 & bull1==0 ~ 1, 
                                    bull==1 & bull1==1 ~ 0,
                                    TRUE ~ NA_real_))

df2 %<>% mutate(differ = differ*10000)

bull_win <- df2 %>% filter(!is.na(bullish))
summary(bull_win$differ)
sum(bull_win$differ)

bear_win <- df2 %>% filter(!is.na(bearish))
summary(bear_win$differ)
sum(bear_win$differ)

total_win <- sum(bull_win$differ) + abs(sum(bear_win$differ))
total_win