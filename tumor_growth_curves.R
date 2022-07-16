library(tidyverse)
library(patchwork)

# Load a test csv file.
# Mice a, b, and c are from the group A. Mice d, e, and f are from B.
# Tumor size is measured on days 0, 5, 10, 15, 20, 25, and 30.
d <- read.delim("tumor_growth_curves.csv", sep = ",")

# Plot tumor growth curves using a mean and standard errors.
p1 <- 
d %>% pivot_longer(cols = c(a,b,c,d,e,f), names_to = "mouse") %>% 
  mutate(treatment = case_when(mouse == c("a","b","c") ~ "A",
                           TRUE ~ "B")) %>% 
  group_by(treatment, Day) %>% 
  summarise(mean=mean(value),
            sem=sd(value)/sqrt(length(value))) %>% 
  ungroup() %>% 
  ggplot(aes(x=Day, y=mean, group=treatment))+
  geom_line(size=0.5)+
  geom_point(aes(shape=treatment),size=3.5)+
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.5)+
  theme_classic()+
  coord_cartesian(ylim = c(0,27), xlim = c(0,32), clip="off")+
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA),
                     breaks = seq(0,27,by = 5), labels = c(0,5,10,15,20,25))+
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_text(size = 18, color = "black"),
        axis.text.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)),
        axis.ticks.length.x = unit(0.3, "cm"),
        axis.ticks.length.y = unit(0.2, "cm"))

# Plot individual tumor growth curves in a group A.
p2 <- 
d %>% pivot_longer(cols = c(a,b,c,d,e,f), names_to = "mouse") %>%
  filter(mouse == c("a","b","c")) %>% 
  ggplot(aes(x=Day, y=value, group = mouse))+
  geom_line()+
  geom_point(aes(shape=mouse), size=3)+
  theme_classic()+
  coord_cartesian(ylim = c(0,27), xlim = c(0,32), clip = "off")+
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA),
                     breaks = seq(0,27,by = 5), labels = c(0,5,10,15,20,25))+
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_text(size = 18, color = "black"),
        axis.text.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)),
        axis.ticks.length.x = unit(0.3, "cm"),
        axis.ticks.length.y = unit(0.2, "cm"))

# Plot individual tumor growth curves in a group B.
p3 <- 
d %>% pivot_longer(cols = c(a,b,c,d,e,f), names_to = "mouse") %>%
  filter(mouse == c("d","e","f")) %>% 
  ggplot(aes(x=Day, y=value, group = mouse))+
  geom_line()+
  geom_point(aes(shape=mouse), size=3)+
  theme_classic()+
  coord_cartesian(ylim = c(0,27), xlim = c(0,32), clip = "off")+
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA),
                     breaks = seq(0,27,by = 5), labels = c(0,5,10,15,20,25))+
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_text(size = 18, color = "black"),
        axis.text.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)),
        axis.ticks.length.x = unit(0.3, "cm"),
        axis.ticks.length.y = unit(0.2, "cm"))

# Arrange figures (patchwork).
p <- 
p2+p3+p1

# Save the figures.
ggsave("tumor_growth_curves.pdf",plot = p, width = 30, height = 8, units = "cm", dpi = "retina")
