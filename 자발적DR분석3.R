library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

hh1 <- read.csv('c:/Users/user/Desktop/자발적DR분석2/name_ymd_hour_kWh_cbl.csv', header=T)
hh2 <- read.csv('c:/Users/user/Desktop/자발적DR분석2/name_ymd_hour_kWh_cbl2.csv', header=T)

holiday <- c('2019-01-01','2019-02-04','2019-02-05','2019-02-06','2019-03-01','2019-06-06','2019-08-15',
             '2019-09-12','2019-09-13','2019-10-03','2019-10-09','2019-12-25','2020-01-01','2020-01-24',
             '2020-01-25','2020-01-26','2020-01-27','2020-03-01','2020-04-15', '2020-04-30','2020-05-01', '2020-05-05')

hh <- rbind(hh1, hh2)

hh <- hh[!hh$ymd %in% holiday,]
hh$ymd <- ymd(hh$ymd)

hh %>% filter(ymd=='2019-01-01')


#
g <- hh %>% filter(is.na(cbl)==F) %>%
  group_by(ymd, hour, ym) %>%
  summarise(kWh=sum(kWh), cbl=sum(cbl)) %>%
  filter(!ymd%in%c('2020-01-23','2020-01-28','2020-03-31', '2020-04-15')) %>% 
  as.data.frame

g %>% filter(is.na(cbl))



# 피크 수요 dR 발생한 시간 감축량
## 8/20 17, 18
## 8/26 17, 18
g %>% 
  mutate(diff = cbl - kWh) %>% 
  filter(ymd=='2020-08-20', hour=='18:00')

g %>% 
  mutate(diff = cbl - kWh) %>% 
  filter(ymd>='2020-08-09', hour=='18:00')

g$ymd <- as.character(g$ymd)

# 피크DR 발령 시간대 line plot
thresholds <- data.frame(xmin=c('2020-08-19', '2020-08-25'),
                         xmax=c('2020-08-20', '2020-08-26'),
                         ymin=rep(-Inf, 2), ymax=rep(Inf, 2))


library(reshape)
g %>% 
  mutate(diff = cbl - kWh) %>% 
  filter(ymd>='2020-08-09', hour=='18:00') %>%
  mutate(min_line = pmin(cbl, kWh)) %>% select(ymd, kWh, cbl, min_line) %>% 
  melt(id.vars=c("ymd","min_line")) %>%
  ggplot(aes(x=ymd))+
  geom_line(aes(y=kWh, group=1), color='#F7941D', size=2)+
  geom_point(aes(y=kWh), shape=21, color='#F7941D', size=2.5, fill='white')+
  geom_line(aes(y=cbl, group=1), color='#5B9BD5', size=2)+
  geom_point(aes(y=cbl), shape=21, color='#5B9BD5', size=2.5, fill='white')+
  #  geom_line(aes(y=med.cbl, group=1), color='#1aaaba', size=1.5)+
  #  geom_point(aes(y=med.cbl), shape=21, color='#1aaaba', size=2, fill='white')+
  # geom_line(aes(y=min.cbl, group=1), color='#F7941D', size=3)+
  # geom_point(aes(y=min.cbl), shape=21, color='#F7941D', size=4.5, fill='white')+
  geom_rect(data=thresholds , aes(x=NULL, y=NULL, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), alpha=.1)+
  theme_bw()+
  theme(panel.border = element_blank(),
        strip.background = element_rect(fill='#7DCDCB', color=NA),
        strip.text = element_text(size=15, color='white', face = 'bold'),
        axis.title=element_blank(),
        axis.text.y=element_text(size=14),
        axis.text.x=element_text(angle=45, size=12, hjust=1))



#

library(reshape)
g %>% 
  mutate(diff = cbl - kWh) %>% 
  filter(ymd>='2020-08-09', hour=='18:00') %>%
  mutate(min_line = pmin(cbl, kWh)) %>% select(ymd, kWh, cbl, min_line) %>%
  gather(., c('kWh', 'cbl'), key='var', value='value') %>%
  ggplot(aes(x=ymd, fill=var))+
  geom_ribbon(aes(ymax=value, ymin=min_line))+
  scale_fill_manual(values=c(Stocks="darkred", Bonds="darkblue"))


  geom_line(aes(y=kWh, group=1), color='#F7941D', size=2)+
  geom_point(aes(y=kWh), shape=21, color='#F7941D', size=2.5, fill='white')+
  geom_line(aes(y=cbl, group=1), color='#5B9BD5', size=2)+
  geom_point(aes(y=cbl), shape=21, color='#5B9BD5', size=2.5, fill='white')+
  #  geom_line(aes(y=med.cbl, group=1), color='#1aaaba', size=1.5)+
  #  geom_point(aes(y=med.cbl), shape=21, color='#1aaaba', size=2, fill='white')+
  # geom_line(aes(y=min.cbl, group=1), color='#F7941D', size=3)+
  # geom_point(aes(y=min.cbl), shape=21, color='#F7941D', size=4.5, fill='white')+
  geom_rect(data=thresholds , aes(x=NULL, y=NULL, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), alpha=.1)+
  theme_bw()+
  theme(panel.border = element_blank(),
        strip.background = element_rect(fill='#7DCDCB', color=NA),
        strip.text = element_text(size=15, color='white', face = 'bold'),
        axis.title=element_blank(),
        axis.text.y=element_text(size=14),
        axis.text.x=element_text(angle=45, size=12, hjust=1))
