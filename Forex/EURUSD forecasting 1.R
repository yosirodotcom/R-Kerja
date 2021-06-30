pacman::p_load(pacman, dplyr, magrittr, vars, ecm, MuMIn, PerformanceAnalytics,dynlm, dyn, rio, olsrr, broom)
rm(list = ls())

# Preparing data

df <- import(file="eurusd.xlsx")
names(df)
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

df2 <- df %>% mutate(y1 = lag(Y,1),
              y2 = lag(Y,2),
              y3 = lag(Y,3),
              y4 = lag(Y,4),
              y5 = lag(Y,5),
              y6 = lag(Y,6),
              y7 = lag(Y,7)) %>% na.omit()

model <- lm(Y ~ y1+y2+y3+y4+y5+y6+y7, df2)
      summary(model)

ols_step_both_p(model)

df2 <- df[-c(1:10),]
model_fit_combine <- regsubsets(Y ~ y1+y2+y3+y4+y5+y6+y7, data = df2, nbest = 1, method = "seqrep")
df_fit_combine <- with(summary(model_fit_combine), data.frame(rsq, adjr2, cp, bic, rss, outmat)) %>% print()

options(na.action ="na.fail")
dredge(global.model = model)
options(na.action ="na.omit")

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













################### with logit

update.packages()
pacman::p_load(pacman, rio, tidyverse, magrittr, AER, stargazer)


eurusd <- import(file = "clipboard")
df <- eurusd
df %<>% mutate(pip = abs(Price - Open)*10000,
               trend = (Price - Open)*10000,
               nomor = 1:n(),
               candle = ifelse(trend < 0, "Bear", "Bull")) %>% 
        arrange(desc(nomor))
df %<>% mutate(candleL1 = lag(candle,1), pipL1 = lag(pip, 1),
               candleL2 = lag(candle,2), pipL2 = lag(pip, 2),
               PriceL1 = lag(Price, 1),
               OpenL1 = lag(Open, 1),
               HighL1 = lag(High, 1),
               LowL1 = lag(Low, 1),
               PriceL2 = lag(Price, 2),
               OpenL2 = lag(Open, 2),
               HighL2 = lag(High, 2),
               LowL2 = lag(Low, 2)) 
df %<>% mutate(sinyal = ifelse(candleL2 == "Bear" & candleL1 == "Bull" & PriceL1 > HighL2, "Bull", 
                               ifelse(candleL2 == "Bull" & candleL1 == "Bear" & PriceL1 < LowL2, "Bear","No signal")))

df %<>% select(sinyal, candle, pip)

profit.bull <- df %>% filter(sinyal == "Bull", candle == "Bull") %>% summarize(profit = sum(pip)) %>% pull()
loss.bull <- df %>% filter(sinyal == "Bull", candle == "Bear") %>% summarize(loss = sum(pip)) %>% pull()
profit.bear <- df %>% filter(sinyal == "Bear", candle == "Bear") %>% summarize(profit = sum(pip)) %>% pull()
loss.bear <- df %>% filter(sinyal == "Bear", candle == "Bull") %>% summarize(loss = sum(pip)) %>% pull()

profit.bull
loss.bull
profit.bear
loss.bear

total.profit <- profit.bull+profit.bear
total.loss <- loss.bull+loss.bear

df$X2 <- as_factor(df$X2)
df$X3 <- as_factor(df$X3)
df$X4 <- as_factor(df$X4)
df$candle <- as_factor(df$candle)

engulfing <- df %>% mutate(sinyal = ifelse())



























maxpip <- 30

df.logit <- df[-c(1:4),]
df.logit %<>% filter(pipL1 >=maxpip,
                     pipL2 >=maxpip,
                     pipL3 >=maxpip,
                     pipL4 >=maxpip)

logit.fit1 <- glm(candle ~ candleL1, df.logit, family = binomial(link = "logit"))
logit.fit2 <- glm(candle ~ X2, df.logit, family = binomial(link = "logit"))
logit.fit3 <- glm(candle ~ X3, df.logit, family = binomial(link = "logit"))
logit.fit4 <- glm(candle ~ X4, df.logit, family = binomial(link = "logit"))
summary(logit.fit1)
summary(logit.fit2)
summary(logit.fit3)
summary(logit.fit4)


profit <- df.logit %>% filter(X2 == "Bull Bull", candle == "Bear") %>% summarize(profit = abs(sum(pip))) %>% pull()
jumlah.profit <- df.logit %>% filter(X2 == "Bull Bull", candle == "Bear") %>% summarize(jumlah = n()) %>% pull()
loss <- df.logit %>% filter(X2 == "Bull Bull", candle == "Bull") %>% summarize(loss = abs(sum(pip))) %>% pull()
jumlah.loss <- df.logit %>% filter(X2 == "Bull Bull", candle == "Bull") %>% summarize(jumlah = n()) %>% pull()

win.ratio1 <- profit/(profit+loss) 
win.ratio1

win.ratio2 <- jumlah.profit/(jumlah.loss+jumlah.profit)
win.ratio2
jumlah.loss
jumlah.profit
