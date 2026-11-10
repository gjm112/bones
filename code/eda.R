# Pb, Ca, Sr, Zn, Fe
# 
# They each have a % and significant difference. I’ll get in contact with the rep at Thermo Fisher who lent us the device for more information on what the percentage is out of to be sure. There was an option for the machine to take readings in PPM as well. If you’d like the documents of the machine output as well I can forward those to you. Just let me know!
#   
#   Sara Brownlee

library(tidyverse)
library(xlsx)
library(energy)
library(proxy)

dat <- read.xlsx("./data/7.20 Scans.xlsx",1)

#Pb, Ca, Sr, Zn, Fe
datclean <- dat %>% filter(Scan.. != "Average") %>%
  select(Bone, Ca.., Fe.., Pb.., Zn..) %>% 
  group_by(Bone) %>% summarize(Ca = mean(Ca..),
                               Fe = mean(Fe..),
                               Pb = mean(Pb..),
                               S = mean(S..)) %>% 
  mutate(leg = str_detect(Bone, "Femur|Tibia") + 0) %>% 
  arrange(leg) %>% filter(S < 4)

datclean %>% ggplot(aes(x = Ca)) + geom_histogram()
datclean %>% ggplot(aes(x = Fe)) + geom_histogram()
datclean %>% ggplot(aes(x = Pb)) + geom_histogram()
datclean %>% ggplot(aes(x = S)) + geom_histogram()

datclean %>% ggplot(aes(x = S, color = leg)) + geom_

sizes <- datclean %>% group_by(leg) %>% summarize(n = n()) %>% pull(n)

#Compoute distance matrix
ddd <- datclean %>% select(Ca:S) %>% proxy::dist(method = "cosine")
#ddd <- datclean %>% select(Ca:S) %>% stats::dist()

res_eqdist <- eqdist.etest(ddd, sizes = sizes, R = 10000, distance = TRUE)
res_eqdist
datclean %>% view()

