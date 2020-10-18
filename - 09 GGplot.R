library(ggplot2)
library(ggpubr)


# just normals

cn <- fulldata[grep("CN", fulldata$DX) , ]

cnttau <- ggplot() + geom_point(data = cn, mapping = aes(x = av1451entorhinal, y = TAU)) + 
  geom_smooth(method = lm, se = FALSE, data = cn, aes(x = av1451entorhinal, y = TAU), color = "red") + theme_bw() +
  xlab("Level of T-tau in CN Subjects") +
  ylab("Entorhinal Region Uptake")
cnptau <- ggplot() + geom_point(data = cn, mapping = aes(x = av1451entorhinal, y = PTAU)) + 
  geom_smooth(method = lm, se = FALSE, data = cn, mapping = aes(x = av1451entorhinal, y = PTAU), color = "red") + theme_bw()+
  xlab("Level of P-tau in CN Subjects") +
  ylab("Entorhinal Region Uptake")

# Scatter plots (sp)
ci <- fulldata[!grepl("CN", fulldata$DX) , ]

cittau <- ggplot() + geom_point(data = ci, mapping = aes(x = av1451entorhinal, y = TAU)) + 
  geom_smooth(method = lm, se = FALSE, data = ci, aes(x = av1451entorhinal, y = TAU), color = "red") + theme_bw() +
  xlab("Level of T-tau in CI Subjects") +
  ylab("Entorhinal Region Uptake")
ciptau <- ggplot() + geom_point(data = ci, mapping = aes(x = av1451entorhinal, y = PTAU)) + 
  geom_smooth(method = lm, se = FALSE, data = ci, mapping = aes(x = av1451entorhinal, y = PTAU), color = "red") + theme_bw() +
  xlab("Level of P-tau in CI Subjects") +
  ylab("Entorhinal Region Uptake")


ggarrange(cnttau, cnptau, cittau, ciptau, 
          labels = c(" ", " ", " ", " "),
          ncol = 2, nrow = 2)
