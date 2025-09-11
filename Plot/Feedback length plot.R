library(tidyverse)
library(hrbrthemes)

p <- content_analysis_outcomes %>%
  ggplot(aes(x=Length)) +
  geom_histogram(binwidth=3, fill="grey30", color="#e9ecef", alpha=0.9) +
  theme_ipsum() +
  scale_x_continuous(breaks = c(0, 25, 50,75,100,125,150,175,200,225)) +
  xlab("Feedback length") + 
  ylab("Frequency") +
  theme(
    axis.title.x = element_text(size=12, face="plain"),
    axis.title.y = element_text(size=12, face="plain")
  )+
  theme(axis.title.x = element_text(vjust = 0, hjust = 0.5),
        axis.title.y = element_text(vjust = 0, hjust = 0.5))

p

