require(ggplot2)
require(dplyr)
require(tidyr)
library("knitr")

library(readr)

d0 <- read.csv("strach_i_samoskutecznosc.csv", 
               sep = ',', 
               dec = ",",
               col.names = c("time", "f1", "f2", 'f3', 'f4', 'f5', 'f6', 'f7',
            's1', 's2', "s3", "s4", 's5', 's6', 's7', 's8', 's9', 's10',
            'ex', 'covid', 'sex', 'size'),
                skip=1, header=F, na.string="NA") 

d1 <- d0 %>%
  mutate(across(c("f1", "f2", 'f3', 'f4', 'f5', 'f6', 'f7', 
                  's1', 's2', "s3", "s4", 's5', 's6', 's7', 's8', 's9', 's10', 
                  ), ~ case_when(
    . == "zdecydowanie nie" ~ 1,
    . == "nie"              ~ 2,
    . == "ani tak/ani nie"  ~ 3,
    . == "tak"              ~ 4,
    . == "zdecydowanie tak" ~ 5
  ))) %>%
  mutate (fear = f1 + f2 + f3 + f4 + f5 + f6 + f7,
          se = s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8 +s9 +s10) %>%
  select (fear, se, ex, covid, sex, size)

## sex

sex.f <- d1 %>%
  select (sex) %>%
  group_by(sex)%>%
  summarize(n=n())%>%
  mutate(prop=n/sum(n) * 100 )

sex.t <- kable(sex.f, col.names = c('płeć', 'n', '%'))
sex.t

## chart
p.1 <- ggplot(sex.f, aes(x = reorder(sex, n), y = n )) +
  ggtitle("Badani wg płci") +
  xlab("") + ylab("%") +
  geom_bar(position = 'dodge', stat = 'identity', fill = "steelblue") +
  geom_text(data=sex.f, aes(label=sprintf("%.2f", prop), y= prop), hjust=1.5, color="white" ) +
  #scale_x_discrete (breaks=var.names,  labels=var.labels) +
  coord_flip()
p.1

## size
size.f <- d1 %>%
  select (size) %>%
  group_by(size)%>%
  summarize(n=n())%>%
  mutate(prop=n/sum(n) * 100 )

size.t <- kable(size.f, col.names = c('zatrudnienie', 'n', '%'))
size.t
## recode
## podziel na dwie tylko
size.f.r <- size.f %>%
mutate(size=recode(size, 
                   '1' = '1-49',
                  'do 9 osób'="1-49",
                  '10-49'= '1-49')) %>%
  group_by(size) %>%
    summarise(n=sum(n), prop=sum(prop))

## chart
p.2 <- ggplot(size.f.r, aes(x = reorder(size, n), y = n )) +
  ggtitle("Badani wg wielkości firmy w której są zatrudnieni") +
  xlab("") + ylab("%") +
  geom_bar(position = 'dodge', stat = 'identity', fill = "steelblue") +
  geom_text(data=size.f.r, aes(label=sprintf("%.2f", prop), y= prop), hjust=1.5, color="white" ) +
  #scale_x_discrete (breaks=var.names,  labels=var.labels) +
  coord_flip()
p.2

## ex (staż)
## staż jest liczbą

                             
## W grupach staż wg płci

ex.sex.f <- d1 %>%
  select (ex, sex) %>%
  group_by(sex)%>%
  summarize(m=mean(ex))

ex.sex.t <- kable(ex.sex.f, col.names = c('płeć', 'średni staż'))

ex.sex.t
###

## zależność/korelacja: między stażem/kontaktem/płcią/wielkością firmy
## a strachem (przekodowanym na skalę porządkową) -- test chi kwadrat

fear.sex.f <- d1 %>%
  select (fear, sex) %>% 
  mutate( fear=case_when(fear >= 25 ~ "d",  fear >= 15 ~ "s", 
                         TRUE ~ "m") ) %>%
  table()

fear.sex.t <- kable(fear.sex.f)

fear.sex.t
## test chi kwadrat
chi_test <- chisq.test(fear.sex.f)
chi_test$statistic
chi_test$p.value
## Nie ma podstaw do odrzucenia hipotezy że nie ma związku
## między fear a sex




## regresja
## strach vs samoskuteczność
lm <- lm(data=d1, fear ~ se );
lmc <- coef(lm);
lmr <- summary(lm)$r.squared
lmc

lmr

summary(lm)$coefficients

ggplot(d1, aes(x = fear, y = se)) +
  geom_point(color="steelblue", size=2) +
  geom_smooth(method = "lm") +
  ggtitle ("Strach vs samoskuteczność")

## strach vs ex
##
lm <- lm(data=d1, fear ~ ex );
lmc <- coef(lm);
lmr <- summary(lm)$r.squared
lmc

lmr

summary(lm)$coefficients

ggplot(d1, aes(x = fear, y = ex)) +
  geom_point(color="steelblue", size=2) +
  geom_smooth(method = "lm") +
  ggtitle ("Strach vs staż")

## korelacja
## współczynnik korelacji pearsona
r.f.se <- cor(d1$fear, d1$se, method = "pearson")
r.f.se
