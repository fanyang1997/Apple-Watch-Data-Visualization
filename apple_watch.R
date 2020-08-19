library(XML)
library(tidyverse)
library(lubridate)
library(scales)
library(ggthemes)



path <- './code/apple_watch/apple_watch/'
list.files(paste0(path, '/apple_health_export'))

xml <- xmlParse(paste0(path, '/apple_health_export/export.xml'))
summary(xml)


df_record <-   XML:::xmlAttrsToDataFrame(xml["//Record"])
df_activity <- XML:::xmlAttrsToDataFrame(xml["//ActivitySummary"])
df_workout <-  XML:::xmlAttrsToDataFrame(xml["//Workout"])
df_clinical <- XML:::xmlAttrsToDataFrame(xml["//ClinicalRecord"])
df_location <- XML:::xmlAttrsToDataFrame(xml["//Location"]) %>% 
  mutate(latitude = as.numeric(as.character(latitude)),
         longitude = as.numeric(as.character(longitude)))



df %>%
  filter(endDate >= '2020/04/01' & 
           date < '2020/07/30') %>%
  filter(type == 'DistanceWalkingRunning') %>% 
  # Had to reduce sourceName to these 2 sources to avoid double-counting
  # by other apps that use BodyMass and then store it back into Health
  filter(value > 0.5 & value < 2) %>%
  ggplot(aes(x= date, y = value * 100)) +
  geom_point(alpha = 0.3) +
  geom_smooth(span = 0.2, col = "grey30", se = FALSE) +
  labs(title = "Apple Health Weight Chart Sample",
       caption = "@taraskaduk | taraskaduk.com") +
  theme(axis.text.y = element_blank()) # you shouldn't see these, lol


df_workout %>%
  filter(date > '2020/04/01' &
         totalDistance > 1) %>%
  # Had to reduce sourceName to these 2 sources to avoid double-counting
  # by other apps that use BodyMass and then store it back into Health
  ggplot(aes(x= date, y = totalDistance)) +
  geom_point(alpha = 0.3) +
  geom_smooth(span = 0.2, col = "grey30", se = FALSE) +
  labs(title = "Apple Health Weight Chart Sample",
       caption = "@taraskaduk | taraskaduk.com") +
  theme(axis.text.y = element_blank()) # you shouldn't see these, lol

df_activity_tall %>% 
  filter(date > '2020/04/01' &
           date < '2020/07/01') %>%
  ggplot(aes(x = wday, y = week, fill = boolean)) +
  geom_tile(col = "grey30", na.rm = FALSE) +
  theme(panel.grid.major = element_blank()) +
  scale_fill_manual(values = c("grey80", "#1a9641")) +
  facet_wrap(~ category) +
  coord_fixed(ratio = 0.15) +
  guides(fill=FALSE) +
  labs(title = "Apple Watch goals completion",
       caption = '@Fan Yang') +
  theme(axis.text.x = element_blank())
