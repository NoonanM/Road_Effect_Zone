
library(ggplot2)
library(ggbrace)

Logit_Link <- function(x) {
  B_0 <- 0
  B_1 <- 0.037026
  mu = exp(B_0 + B_1*x)/(1+exp(B_0 + B_1*x))
  mu
}

logit_pred <- rev(Logit_Link(seq(-130,120, 0.01)))

FIG <- 
ggplot() +
  geom_smooth(aes(y=logit_pred, x=seq(-130,120, 0.01)), size = 0.4, col = "#FF0000", se = F) +
  geom_brace(aes(c(-98,70), c(1, 1.2)), inherit.data=F) +
  annotate('text', 
           x = -15, y = 1.25, 
           label = 'paste(bold(Road~Effect~Zone))',
           parse = TRUE) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm")) +
  scale_y_continuous(limits = c(0, 1.35), expand = c(0,0)) +
  scale_x_continuous(limits = c(-100, 120), expand = c(0,1)) +
  xlab("Distance from road") +
  ylab("Ecological impact")



ggsave(FIG,
       width = 3.23, height = 3, units = "in",
       dpi = 600,
       #bg = "transparent",
       file="Figures/REZ.png")
