library(ggplot2)
library(RColorBrewer)


pgls.results <- read.csv("Reduced pgls results.csv", header=TRUE)

pgls.results$Xvar <- factor(pgls.results$Xvar, levels=unique(as.character(pgls.results$Xvar)) )

colorscheme <- colorRampPalette(brewer.pal(9, "Set1"))
darkplot.colorscheme <- colorRampPalette(brewer.pal(12, "Paired"))


# Create a faceted plot, organized by endodermal vs stele tissue. Legend will be 
# placed at the bottom due to the size. 

full.plot <- ggplot(pgls.results, aes(x = pgls.results$Xvar, y = pgls.results$Rsq.Pct,
              fill = pgls.results$Fillvar)) + 
  geom_bar(position = "fill",stat = "identity") +
  theme_classic(base_size=12) + theme(legend.position = "bottom") + 
  scale_fill_manual(values = colorscheme(40)) + scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Percent of Variation") + 
  facet_grid(~Group, space="free", scales="free") +
  scale_x_discrete(labels=function(x) gsub("[[:punct:]]", " ", x)) + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) + theme(legend.title=element_blank())

dark.plot <- ggplot(pgls.results, aes(x = pgls.results$Xvar, y = pgls.results$Rsq.Pct,
                                      fill = pgls.results$Fillvar)) + 
  geom_bar(position = "fill",stat = "identity") +
  theme_classic(base_size=12) + 
  theme(panel.background = element_rect(fill = "grey18", colour = "grey99")) +
  theme(legend.position = "bottom") + 
  theme(legend.background = element_rect(fill = "grey18", colour = "grey99")) +
  theme(legend.text = element_text(colour = "grey99")) +
  theme(plot.background = element_rect(fill = "grey18", colour = "grey99")) +
  theme(text= element_text(colour="grey99")) +
  scale_fill_manual(values = darkplot.colorscheme(40)) + 
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Percent of Variation", colour = "grey99") + 
  facet_grid(~Group, space="free", scales="free") +
  scale_x_discrete(labels=function(x) gsub("[[:punct:]]", " ", x)) + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0, colour="grey99")) + 
  theme(axis.text.y=element_text(colour="grey99")) +
  theme(legend.title=element_blank())

viridis.plot1 <- ggplot(pgls.results, aes(x = pgls.results$Xvar, y = pgls.results$Rsq.Pct,
              fill = pgls.results$Fillvar)) + 
  geom_bar(position = "fill",stat = "identity") +
  theme_classic(base_size=12) + theme(legend.position = "bottom") + 
  scale_fill_viridis(discrete = TRUE, option="magma") + scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Percent of Variation") + 
  facet_grid(~Group, space="free", scales="free") +
  scale_x_discrete(labels=function(x) gsub("[[:punct:]]", " ", x)) + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) + theme(legend.title=element_blank())

manual.colorscheme <- c("coral3", "chartreuse3", "cadetblue3", "brown3", "blue3",
                        "azure3", "antiquewhite3", "dodgerblue3", "deeppink3", "darkslategray",
                        "darkorange3", "darkgreen", "cyan3", "forestgreen",
                        "firebrick3", "indianred3", "honeydew4", "maroon3", "lightsteelblue4",
                        "lightsalmon3", "lightgoldenrod3", "palevioletred3", "orangered3",
                        "olivedrab4", "navyblue", "springgreen3", "skyblue3", "salmon4",
                        "red4", "hotpink4", "midnightblue", "turquoise4",
                        "wheat4", "lightcoral", "mediumpurple3", "grey63")

manual.plot <- ggplot(pgls.results, aes(x = pgls.results$Xvar, y = pgls.results$Rsq.Pct,
              fill = pgls.results$Fillvar)) + 
  geom_bar(position = "fill",stat = "identity") +
  theme_classic(base_size=12) + theme(legend.position = "bottom") + 
  scale_fill_manual(values = manual.colorscheme) + 
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Percent of Variation", size=16, face="bold") + 
  facet_grid(~Group, space="free", scales="free") +
  theme(strip.text.x = element_text(size = 16, face="bold")) +
  scale_x_discrete(labels=function(x) gsub("[[:punct:]]", " ", x)) + 
  theme(axis.text.x=element_text(angle = -90, vjust=0.5, hjust=0, size=14)) + 
  theme(legend.title=element_blank())

ggsave("manual_plot_light_hi-res v2.eps", dpi=1200, scale=2.5)
manual.plot.dark <-  ggplot(pgls.results, aes(x = pgls.results$Xvar, y = pgls.results$Rsq.Pct,
                                      fill = pgls.results$Fillvar)) + 
  geom_bar(position = "fill",stat = "identity") +
  theme_classic(base_size=12) + 
  theme(panel.background = element_rect(fill = "grey18", colour = "grey99")) +
  theme(legend.position = "bottom") + 
  theme(legend.background = element_rect(fill = "grey18", colour = "grey99")) +
  theme(legend.text = element_text(colour = "grey99")) +
  theme(plot.background = element_rect(fill = "grey18", colour = "grey99")) +
  theme(text= element_text(colour="grey99")) +
  scale_fill_manual(values = manual.colorscheme) + 
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Percent of Variation", colour = "grey99") + 
  facet_grid(~Group, space="free", scales="free") +
  scale_x_discrete(labels=function(x) gsub("[[:punct:]]", " ", x)) + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0, colour="grey99")) + 
  theme(axis.text.y=element_text(colour="grey99")) +
  theme(legend.title=element_blank())


ggsave(filename = "darkplot_Paired-12-40.pdf", dpi=900)
