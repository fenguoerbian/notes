######Step 1: Load the package
library(haven)
library(tidyverse)
library(gestate)
library(rpact)
library(eventTrack)

######Step 2: Load the dataset
adtte <- read_sas("C:\\Users\\sunhaolin\\Desktop\\zijing\\BD0801\\data\\2022_09_21\\adttetor.sas7bdat")
adtte <- data.frame(adtte)

# 0 -- event, 1 -- censor,
adtte1 <- adtte %>%
  mutate(EVENT = ifelse(CNSR == 0,TRUE,FALSE)) %>%
  select(c("AVAL","EVENT"))


######Step 3: Set the recruitment rate
re <- c(1,7,8,15,12,16,27,13,7,11,11,21,28,22,23,25,25,36,30,19)
recruit <- PieceR(matrix(c(rep(1,length(re)),re),ncol = 2), 2)

######Step 4: Set the censoring rate
fit2 <- fit_tte_data(data=adtte
                     ,Time="AVAL"
                     ,Event="cnsr1"
                     ,censoringOne=FALSE
                     ,type="automatic")
fit2

dc <- Weibull(fit2$Parameters[1], fit2$Parameters[2])

######Step 5: Fit the event prediction
predictions <- event_prediction(data = adtte1,
                                Time = "AVAL",
                                Event = "EVENT",
                                censoringOne = FALSE,
                                rcurve = recruit # observed and expected recruitment distribution
                                , dcurve = dc 
                                , max_time = 30 # 不是很敏感
                                , type = "automatic"
                                # 我最终要预测多长时间内的Events还有xxx.
                                , cond_Events = 91 #88 #91    #Current there is 91 events in adtte
                                , cond_NatRisk = 239 #244 #239  # 239 = 357 - 91 - 27(This is censorred forever)
                                , cond_Time = 14
                                , units = "Months"
)

#For the exact date
nevents <- 204
pred_gestage <- exactDatesFromMonths(predictions$Summary[,c("Time", "Cond_Prediction_Upper")],nevents)
pred_gestage #20.21883

#For the lower limit
approx(x =predictions$Summary[,c("Time")],y = predictions$Summary[,c("Cond_Prediction_Lower")], xout = pred_gestage)$y #184.3767

#For the upper limit
approx(x =predictions$Summary[,c("Time")],y = predictions$Summary[,c("Cond_Prediction_Upper")], xout = pred_gestage)$y #223.8144

######Step 6: Fit the graph
a <- adtte %>%
  filter(CNSR == 0) %>%
  mutate(ymonth = format(ADT, "%Y-%m")) %>%
  group_by(ymonth) %>%
  summarise( n = n())
# b <- c(0,0,cumsum(a$n))

b <- c(0,0,1,4,4,9,24,29,35,46,53,60,74,88)

plot_ep(predictions
        ,trajectory="both"
        ,which_PI="both"
        ,max_time=40
        ,observed=b
        ,target=204
        ,max_E=210
        ,legend_position = "bottom_right")


