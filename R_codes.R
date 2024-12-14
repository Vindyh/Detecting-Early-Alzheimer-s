MRI <- read.csv("C:/Users/vindy/Google Drive/SHSU/Longitudinal Data Analysis/Project/MRI_data.csv") 
attach(MRI)


library(ggplot2)
library(tidyverse)
x <- MRI %>%
  select(EDUC, CDR, Gender) %>%
  mutate(CDR = as.factor(CDR)) %>%
  ggplot(aes(x = CDR, y = EDUC)) + 
  geom_jitter(aes(col = CDR), alpha = 0.6) +
  theme_light() 
x

y <- MRI %>%
  select(SES, CDR, Gender) %>%
  mutate(CDR = as.factor(CDR)) %>%
  ggplot(aes(x = CDR, y = SES)) + 
  geom_jitter(aes(col = CDR), alpha = 0.6) +
  theme_light()
y




library(cowplot)
p <- plot_grid(x, y) 
title <- ggdraw() + draw_label("Distribution of Education and Social Economic Status", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))



x <- MRI %>%
  select(MMSE, CDR, Gender) %>%
  mutate(CDR = as.factor(CDR)) %>%
  ggplot(aes(x = CDR, y = MMSE)) + 
  geom_jitter(aes(col = CDR), alpha = 0.6) +
  theme_light() ; x

y <- MRI %>%
  select(nWBV, CDR, Gender) %>%
  mutate(CDR = as.factor(CDR)) %>%
  ggplot(aes(x = CDR, y = nWBV)) + 
  geom_jitter(aes(col = CDR), alpha = 0.6) +
  theme_light() ; y

p <- plot_grid(x, y) 
title <- ggdraw() + draw_label("Distribution of MMSE Score and Whole-brain Volume", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

x <- MRI %>%
  select(eTIV, CDR, Gender) %>%
  mutate(CDR = as.factor(CDR)) %>%
  ggplot(aes(x = CDR, y = eTIV)) + 
  geom_jitter(aes(col = CDR), alpha = 0.6) +
  theme_light() ;x

y <- MRI %>%
  select(ASF, CDR, Gender) %>%
  mutate(CDR = as.factor(CDR)) %>%
  ggplot(aes(x = CDR, y = Age)) + 
  geom_jitter(aes(col = CDR), alpha = 0.6) +
  theme_light() ;y

p <- plot_grid(x, y) 
title <- ggdraw() + draw_label("Distribution of Total Intracranial Volume and Age", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))


logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}
logit2prob(0.8335)
logit2prob(-15.6317)
