
library(readxl)
library(ggplot2)
library(reshape2)

################Please replacedata with your own paths

data <- read_excel("C:/.../.../.../Correlation Coefficients/Raw Data_Factors Only/Guangdong.xlsx")


cor_matrix <- cor(data, use = "pairwise.complete.obs")  
rownames(cor_matrix) <- colnames(cor_matrix) <- colnames(data)


cor_df <- melt(cor_matrix)


cor_df$abs_value <- abs(cor_df$value)


cor_df$stars <- cut(cor_df$abs_value, 
                    breaks = c(-Inf, 0.25, 0.5, 0.75, Inf), 
                    labels = c("", "*", "**", "***"),
                    include.lowest = TRUE)


cor_df$stars <- as.character(cor_df$stars)


cor_df$Var1 <- as.character(cor_df$Var1)
cor_df$Var2 <- as.character(cor_df$Var2)


cor_df$label <- ifelse(cor_df$Var1 < cor_df$Var2, 
                       sprintf("%.2f", cor_df$value), 
                       cor_df$stars)


matrix_height <- ncol(cor_matrix)


print(cor_df)


p <- ggplot(cor_df, aes(x = Var1, y = Var2)) +
  geom_tile(color = "black", fill = NA) +  
  geom_point(aes(size = abs_value, color = value), shape = 16) +  
  geom_text(aes(label = label), vjust = 0.5, color = "black", size = 5, na.rm = TRUE) + 
  scale_size_area(max_size = 25, guide = "none") +  
  scale_color_gradientn(
    colors = c("#3278b5", "#408ac1", "#57a0ca", "#599fc9", "#77b5d6", "#97c8dc", "#b5d5e7", "#cde2f1", "#e2edf5", "#f4f9fa", "#fdf7f3", "#fde7da", "#ffd5c3", "#f9c0a4", "#f0ab87", "#e79072", "#DB725B", "#D05245", "#CF5447", "#BF3739"),
    values = scales::rescale(c(-1, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)),
    limits = c(-1, 1)  
  ) +
  theme_minimal() +
  theme(
    legend.position = "right", 
    legend.box.just = "center", 
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.text.y = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.key.height = unit(matrix_height * 5, "cm")  
  ) +
  coord_fixed() +
  guides(color = guide_colorbar(barheight = unit(matrix_height * 1.8, "cm"))) +  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )


print(p)

ggsave(filename = "C:/Users/tuan/Desktop/Correlation Coefficients/Correlation Coefficients /Correlation Coefficients of Factors in Guangdong Province.png",
       plot = p,
       width = 10,  
       height = 10,
       dpi = 300)  







data <- read_excel("C:/Users/tuan/Desktop/Correlation Coefficients/Raw Data_Factors Only/Yunnan.xlsx")


cor_matrix <- cor(data, use = "pairwise.complete.obs")  
rownames(cor_matrix) <- colnames(cor_matrix) <- colnames(data)


cor_df <- melt(cor_matrix)


cor_df$abs_value <- abs(cor_df$value)


cor_df$stars <- cut(cor_df$abs_value, 
                    breaks = c(-Inf, 0.25, 0.5, 0.75, Inf), 
                    labels = c("", "*", "**", "***"),
                    include.lowest = TRUE)


cor_df$stars <- as.character(cor_df$stars)


cor_df$Var1 <- as.character(cor_df$Var1)
cor_df$Var2 <- as.character(cor_df$Var2)


cor_df$label <- ifelse(cor_df$Var1 < cor_df$Var2, 
                       sprintf("%.2f", cor_df$value), 
                       cor_df$stars)


matrix_height <- ncol(cor_matrix)


print(cor_df)


p <- ggplot(cor_df, aes(x = Var1, y = Var2)) +
  geom_tile(color = "black", fill = NA) +  
  geom_point(aes(size = abs_value, color = value), shape = 16) +  
  geom_text(aes(label = label), vjust = 0.5, color = "black", size = 5, na.rm = TRUE) + 
  scale_size_area(max_size = 25, guide = "none") +  
  scale_color_gradientn(
    colors = c("#3278b5", "#408ac1", "#57a0ca", "#599fc9", "#77b5d6", "#97c8dc", "#b5d5e7", "#cde2f1", "#e2edf5", "#f4f9fa", "#fdf7f3", "#fde7da", "#ffd5c3", "#f9c0a4", "#f0ab87", "#e79072", "#DB725B", "#D05245", "#CF5447", "#BF3739"),
    values = scales::rescale(c(-1, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)),
    limits = c(-1, 1)  
  ) +
  theme_minimal() +
  theme(
    legend.position = "right", 
    legend.box.just = "center", 
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.text.y = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.key.height = unit(matrix_height * 5, "cm")  
  ) +
  coord_fixed() +
  guides(color = guide_colorbar(barheight = unit(matrix_height * 1.8, "cm"))) +  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )


print(p)

ggsave(filename = "C:/Users/tuan/Desktop/Correlation Coefficients/Correlation Coefficients /Correlation Coefficients of Factors in Yunnan Province.png",
       plot = p,
       width = 10,  
       height = 10,
       dpi = 300)  







data <- read_excel("C:/Users/tuan/Desktop/Correlation Coefficients/Raw Data_Factors Only/Fujian.xlsx")


cor_matrix <- cor(data, use = "pairwise.complete.obs")  
rownames(cor_matrix) <- colnames(cor_matrix) <- colnames(data)


cor_df <- melt(cor_matrix)


cor_df$abs_value <- abs(cor_df$value)


cor_df$stars <- cut(cor_df$abs_value, 
                    breaks = c(-Inf, 0.25, 0.5, 0.75, Inf), 
                    labels = c("", "*", "**", "***"),
                    include.lowest = TRUE)


cor_df$stars <- as.character(cor_df$stars)


cor_df$Var1 <- as.character(cor_df$Var1)
cor_df$Var2 <- as.character(cor_df$Var2)


cor_df$label <- ifelse(cor_df$Var1 < cor_df$Var2, 
                       sprintf("%.2f", cor_df$value), 
                       cor_df$stars)


matrix_height <- ncol(cor_matrix)


print(cor_df)


p <- ggplot(cor_df, aes(x = Var1, y = Var2)) +
  geom_tile(color = "black", fill = NA) +  
  geom_point(aes(size = abs_value, color = value), shape = 16) +  
  geom_text(aes(label = label), vjust = 0.5, color = "black", size = 5, na.rm = TRUE) + 
  scale_size_area(max_size = 25, guide = "none") +  
  scale_color_gradientn(
    colors = c("#3278b5", "#408ac1", "#57a0ca", "#599fc9", "#77b5d6", "#97c8dc", "#b5d5e7", "#cde2f1", "#e2edf5", "#f4f9fa", "#fdf7f3", "#fde7da", "#ffd5c3", "#f9c0a4", "#f0ab87", "#e79072", "#DB725B", "#D05245", "#CF5447", "#BF3739"),
    values = scales::rescale(c(-1, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)),
    limits = c(-1, 1)  
  ) +
  theme_minimal() +
  theme(
    legend.position = "right", 
    legend.box.just = "center", 
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.text.y = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.key.height = unit(matrix_height * 5, "cm")  
  ) +
  coord_fixed() +
  guides(color = guide_colorbar(barheight = unit(matrix_height * 1.8, "cm"))) +  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )


print(p)

ggsave(filename = "C:/Users/tuan/Desktop/Correlation Coefficients/Correlation Coefficients /Correlation Coefficients of Factors in Fujian Province.png",
       plot = p,
       width = 10,  
       height = 10,
       dpi = 300)  







data <- read_excel("C:/Users/tuan/Desktop/Correlation Coefficients/Raw Data_Factors Only/Zhejiang.xlsx")


cor_matrix <- cor(data, use = "pairwise.complete.obs")  
rownames(cor_matrix) <- colnames(cor_matrix) <- colnames(data)


cor_df <- melt(cor_matrix)


cor_df$abs_value <- abs(cor_df$value)


cor_df$stars <- cut(cor_df$abs_value, 
                    breaks = c(-Inf, 0.25, 0.5, 0.75, Inf), 
                    labels = c("", "*", "**", "***"),
                    include.lowest = TRUE)


cor_df$stars <- as.character(cor_df$stars)


cor_df$Var1 <- as.character(cor_df$Var1)
cor_df$Var2 <- as.character(cor_df$Var2)


cor_df$label <- ifelse(cor_df$Var1 < cor_df$Var2, 
                       sprintf("%.2f", cor_df$value), 
                       cor_df$stars)


matrix_height <- ncol(cor_matrix)


print(cor_df)


p <- ggplot(cor_df, aes(x = Var1, y = Var2)) +
  geom_tile(color = "black", fill = NA) +  
  geom_point(aes(size = abs_value, color = value), shape = 16) +  
  geom_text(aes(label = label), vjust = 0.5, color = "black", size = 5, na.rm = TRUE) + 
  scale_size_area(max_size = 25, guide = "none") +  
  scale_color_gradientn(
    colors = c("#3278b5", "#408ac1", "#57a0ca", "#599fc9", "#77b5d6", "#97c8dc", "#b5d5e7", "#cde2f1", "#e2edf5", "#f4f9fa", "#fdf7f3", "#fde7da", "#ffd5c3", "#f9c0a4", "#f0ab87", "#e79072", "#DB725B", "#D05245", "#CF5447", "#BF3739"),
    values = scales::rescale(c(-1, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)),
    limits = c(-1, 1)  
  ) +
  theme_minimal() +
  theme(
    legend.position = "right", 
    legend.box.just = "center", 
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.text.y = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.key.height = unit(matrix_height * 5, "cm")  
  ) +
  coord_fixed() +
  guides(color = guide_colorbar(barheight = unit(matrix_height * 1.8, "cm"))) +  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )


print(p)

ggsave(filename = "C:/Users/tuan/Desktop/Correlation Coefficients/Correlation Coefficients /Correlation Coefficients of Factors in Zhejiang Province.png",
       plot = p,
       width = 10,  
       height = 10,
       dpi = 300)  











data <- read_excel("C:/Users/tuan/Desktop/Correlation Coefficients/Raw Data_Factors Only/Guangxi.xlsx")


cor_matrix <- cor(data, use = "pairwise.complete.obs")  
rownames(cor_matrix) <- colnames(cor_matrix) <- colnames(data)


cor_df <- melt(cor_matrix)


cor_df$abs_value <- abs(cor_df$value)


cor_df$stars <- cut(cor_df$abs_value, 
                    breaks = c(-Inf, 0.25, 0.5, 0.75, Inf), 
                    labels = c("", "*", "**", "***"),
                    include.lowest = TRUE)


cor_df$stars <- as.character(cor_df$stars)


cor_df$Var1 <- as.character(cor_df$Var1)
cor_df$Var2 <- as.character(cor_df$Var2)


cor_df$label <- ifelse(cor_df$Var1 < cor_df$Var2, 
                       sprintf("%.2f", cor_df$value), 
                       cor_df$stars)


matrix_height <- ncol(cor_matrix)


print(cor_df)


p <- ggplot(cor_df, aes(x = Var1, y = Var2)) +
  geom_tile(color = "black", fill = NA) +  
  geom_point(aes(size = abs_value, color = value), shape = 16) +  
  geom_text(aes(label = label), vjust = 0.5, color = "black", size = 5, na.rm = TRUE) + 
  scale_size_area(max_size = 25, guide = "none") +  
  scale_color_gradientn(
    colors = c("#3278b5", "#408ac1", "#57a0ca", "#599fc9", "#77b5d6", "#97c8dc", "#b5d5e7", "#cde2f1", "#e2edf5", "#f4f9fa", "#fdf7f3", "#fde7da", "#ffd5c3", "#f9c0a4", "#f0ab87", "#e79072", "#DB725B", "#D05245", "#CF5447", "#BF3739"),
    values = scales::rescale(c(-1, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)),
    limits = c(-1, 1)  
  ) +
  theme_minimal() +
  theme(
    legend.position = "right", 
    legend.box.just = "center", 
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.text.y = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.key.height = unit(matrix_height * 5, "cm")  
  ) +
  coord_fixed() +
  guides(color = guide_colorbar(barheight = unit(matrix_height * 1.8, "cm"))) +  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )


print(p)

ggsave(filename = "C:/Users/tuan/Desktop/Correlation Coefficients/Correlation Coefficients /Correlation Coefficients of Factors in Guangxi Province.png",
       plot = p,
       width = 10,  
       height = 10,
       dpi = 300)  











data <- read_excel("C:/Users/tuan/Desktop/Correlation Coefficients/Raw Data_Factors Only/Chongqing.xlsx")


cor_matrix <- cor(data, use = "pairwise.complete.obs")  
rownames(cor_matrix) <- colnames(cor_matrix) <- colnames(data)


cor_df <- melt(cor_matrix)


cor_df$abs_value <- abs(cor_df$value)


cor_df$stars <- cut(cor_df$abs_value, 
                    breaks = c(-Inf, 0.25, 0.5, 0.75, Inf), 
                    labels = c("", "*", "**", "***"),
                    include.lowest = TRUE)


cor_df$stars <- as.character(cor_df$stars)


cor_df$Var1 <- as.character(cor_df$Var1)
cor_df$Var2 <- as.character(cor_df$Var2)


cor_df$label <- ifelse(cor_df$Var1 < cor_df$Var2, 
                       sprintf("%.2f", cor_df$value), 
                       cor_df$stars)


matrix_height <- ncol(cor_matrix)


print(cor_df)


p <- ggplot(cor_df, aes(x = Var1, y = Var2)) +
  geom_tile(color = "black", fill = NA) +  
  geom_point(aes(size = abs_value, color = value), shape = 16) +  
  geom_text(aes(label = label), vjust = 0.5, color = "black", size = 5, na.rm = TRUE) + 
  scale_size_area(max_size = 25, guide = "none") +  
  scale_color_gradientn(
    colors = c("#3278b5", "#408ac1", "#57a0ca", "#599fc9", "#77b5d6", "#97c8dc", "#b5d5e7", "#cde2f1", "#e2edf5", "#f4f9fa", "#fdf7f3", "#fde7da", "#ffd5c3", "#f9c0a4", "#f0ab87", "#e79072", "#DB725B", "#D05245", "#CF5447", "#BF3739"),
    values = scales::rescale(c(-1, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)),
    limits = c(-1, 1)  
  ) +
  theme_minimal() +
  theme(
    legend.position = "right", 
    legend.box.just = "center", 
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.text.y = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.key.height = unit(matrix_height * 5, "cm")  
  ) +
  coord_fixed() +
  guides(color = guide_colorbar(barheight = unit(matrix_height * 1.8, "cm"))) +  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )


print(p)

ggsave(filename = "C:/Users/tuan/Desktop/Correlation Coefficients/Correlation Coefficients /Correlation Coefficients of Factors in Chongqing Province.png",
       plot = p,
       width = 10,  
       height = 10,
       dpi = 300)  










data <- read_excel("C:/Users/tuan/Desktop/Correlation Coefficients/Raw Data_Factors Only/Jiangxi.xlsx")


cor_matrix <- cor(data, use = "pairwise.complete.obs")  
rownames(cor_matrix) <- colnames(cor_matrix) <- colnames(data)


cor_df <- melt(cor_matrix)


cor_df$abs_value <- abs(cor_df$value)


cor_df$stars <- cut(cor_df$abs_value, 
                    breaks = c(-Inf, 0.25, 0.5, 0.75, Inf), 
                    labels = c("", "*", "**", "***"),
                    include.lowest = TRUE)


cor_df$stars <- as.character(cor_df$stars)


cor_df$Var1 <- as.character(cor_df$Var1)
cor_df$Var2 <- as.character(cor_df$Var2)


cor_df$label <- ifelse(cor_df$Var1 < cor_df$Var2, 
                       sprintf("%.2f", cor_df$value), 
                       cor_df$stars)


matrix_height <- ncol(cor_matrix)


print(cor_df)


p <- ggplot(cor_df, aes(x = Var1, y = Var2)) +
  geom_tile(color = "black", fill = NA) +  
  geom_point(aes(size = abs_value, color = value), shape = 16) +  
  geom_text(aes(label = label), vjust = 0.5, color = "black", size = 5, na.rm = TRUE) + 
  scale_size_area(max_size = 25, guide = "none") +  
  scale_color_gradientn(
    colors = c("#3278b5", "#408ac1", "#57a0ca", "#599fc9", "#77b5d6", "#97c8dc", "#b5d5e7", "#cde2f1", "#e2edf5", "#f4f9fa", "#fdf7f3", "#fde7da", "#ffd5c3", "#f9c0a4", "#f0ab87", "#e79072", "#DB725B", "#D05245", "#CF5447", "#BF3739"),
    values = scales::rescale(c(-1, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)),
    limits = c(-1, 1)  
  ) +
  theme_minimal() +
  theme(
    legend.position = "right", 
    legend.box.just = "center", 
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.text.y = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.key.height = unit(matrix_height * 5, "cm")  
  ) +
  coord_fixed() +
  guides(color = guide_colorbar(barheight = unit(matrix_height * 1.8, "cm"))) +  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )


print(p)

ggsave(filename = "C:/Users/tuan/Desktop/Correlation Coefficients/Correlation Coefficients /Correlation Coefficients of Factors in Jiangxi Province.png",
       plot = p,
       width = 10,  
       height = 10,
       dpi = 300)  







data <- read_excel("C:/Users/tuan/Desktop/Correlation Coefficients/Raw Data_Factors Only/Hunan.xlsx")


cor_matrix <- cor(data, use = "pairwise.complete.obs")  
rownames(cor_matrix) <- colnames(cor_matrix) <- colnames(data)


cor_df <- melt(cor_matrix)


cor_df$abs_value <- abs(cor_df$value)


cor_df$stars <- cut(cor_df$abs_value, 
                    breaks = c(-Inf, 0.25, 0.5, 0.75, Inf), 
                    labels = c("", "*", "**", "***"),
                    include.lowest = TRUE)


cor_df$stars <- as.character(cor_df$stars)


cor_df$Var1 <- as.character(cor_df$Var1)
cor_df$Var2 <- as.character(cor_df$Var2)


cor_df$label <- ifelse(cor_df$Var1 < cor_df$Var2, 
                       sprintf("%.2f", cor_df$value), 
                       cor_df$stars)


matrix_height <- ncol(cor_matrix)


print(cor_df)


p <- ggplot(cor_df, aes(x = Var1, y = Var2)) +
  geom_tile(color = "black", fill = NA) +  
  geom_point(aes(size = abs_value, color = value), shape = 16) +  
  geom_text(aes(label = label), vjust = 0.5, color = "black", size = 5, na.rm = TRUE) + 
  scale_size_area(max_size = 25, guide = "none") +  
  scale_color_gradientn(
    colors = c("#3278b5", "#408ac1", "#57a0ca", "#599fc9", "#77b5d6", "#97c8dc", "#b5d5e7", "#cde2f1", "#e2edf5", "#f4f9fa", "#fdf7f3", "#fde7da", "#ffd5c3", "#f9c0a4", "#f0ab87", "#e79072", "#DB725B", "#D05245", "#CF5447", "#BF3739"),
    values = scales::rescale(c(-1, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)),
    limits = c(-1, 1)  
  ) +
  theme_minimal() +
  theme(
    legend.position = "right", 
    legend.box.just = "center", 
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.text.y = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.key.height = unit(matrix_height * 5, "cm")  
  ) +
  coord_fixed() +
  guides(color = guide_colorbar(barheight = unit(matrix_height * 1.8, "cm"))) +  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )


print(p)

ggsave(filename = "C:/Users/tuan/Desktop/Correlation Coefficients/Correlation Coefficients /Correlation Coefficients of Factors in Hunan Province.png",
       plot = p,
       width = 10,  
       height = 10,
       dpi = 300)  















data <- read_excel("C:/Users/tuan/Desktop/Correlation Coefficients/Raw Data_Factors Only/Sichuan.xlsx")


cor_matrix <- cor(data, use = "pairwise.complete.obs")  
rownames(cor_matrix) <- colnames(cor_matrix) <- colnames(data)


cor_df <- melt(cor_matrix)


cor_df$abs_value <- abs(cor_df$value)


cor_df$stars <- cut(cor_df$abs_value, 
                    breaks = c(-Inf, 0.25, 0.5, 0.75, Inf), 
                    labels = c("", "*", "**", "***"),
                    include.lowest = TRUE)


cor_df$stars <- as.character(cor_df$stars)


cor_df$Var1 <- as.character(cor_df$Var1)
cor_df$Var2 <- as.character(cor_df$Var2)


cor_df$label <- ifelse(cor_df$Var1 < cor_df$Var2, 
                       sprintf("%.2f", cor_df$value), 
                       cor_df$stars)


matrix_height <- ncol(cor_matrix)


print(cor_df)


p <- ggplot(cor_df, aes(x = Var1, y = Var2)) +
  geom_tile(color = "black", fill = NA) +  
  geom_point(aes(size = abs_value, color = value), shape = 16) +  
  geom_text(aes(label = label), vjust = 0.5, color = "black", size = 5, na.rm = TRUE) + 
  scale_size_area(max_size = 25, guide = "none") +  
  scale_color_gradientn(
    colors = c("#3278b5", "#408ac1", "#57a0ca", "#599fc9", "#77b5d6", "#97c8dc", "#b5d5e7", "#cde2f1", "#e2edf5", "#f4f9fa", "#fdf7f3", "#fde7da", "#ffd5c3", "#f9c0a4", "#f0ab87", "#e79072", "#DB725B", "#D05245", "#CF5447", "#BF3739"),
    values = scales::rescale(c(-1, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)),
    limits = c(-1, 1)  
  ) +
  theme_minimal() +
  theme(
    legend.position = "right", 
    legend.box.just = "center", 
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.text.y = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.key.height = unit(matrix_height * 5, "cm")  
  ) +
  coord_fixed() +
  guides(color = guide_colorbar(barheight = unit(matrix_height * 1.8, "cm"))) +  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )


print(p)

ggsave(filename = "C:/Users/tuan/Desktop/Correlation Coefficients/Correlation Coefficients /Correlation Coefficients of Factors in Sichuan Province.png",
       plot = p,
       width = 10,  
       height = 10,
       dpi = 300)  











data <- read_excel("C:/Users/tuan/Desktop/Correlation Coefficients/Raw Data_Factors Only/Henan.xlsx")


cor_matrix <- cor(data, use = "pairwise.complete.obs")  
rownames(cor_matrix) <- colnames(cor_matrix) <- colnames(data)


cor_df <- melt(cor_matrix)


cor_df$abs_value <- abs(cor_df$value)


cor_df$stars <- cut(cor_df$abs_value, 
                    breaks = c(-Inf, 0.25, 0.5, 0.75, Inf), 
                    labels = c("", "*", "**", "***"),
                    include.lowest = TRUE)


cor_df$stars <- as.character(cor_df$stars)


cor_df$Var1 <- as.character(cor_df$Var1)
cor_df$Var2 <- as.character(cor_df$Var2)


cor_df$label <- ifelse(cor_df$Var1 < cor_df$Var2, 
                       sprintf("%.2f", cor_df$value), 
                       cor_df$stars)


matrix_height <- ncol(cor_matrix)


print(cor_df)


p <- ggplot(cor_df, aes(x = Var1, y = Var2)) +
  geom_tile(color = "black", fill = NA) +  
  geom_point(aes(size = abs_value, color = value), shape = 16) +  
  geom_text(aes(label = label), vjust = 0.5, color = "black", size = 5, na.rm = TRUE) + 
  scale_size_area(max_size = 25, guide = "none") +  
  scale_color_gradientn(
    colors = c("#3278b5", "#408ac1", "#57a0ca", "#599fc9", "#77b5d6", "#97c8dc", "#b5d5e7", "#cde2f1", "#e2edf5", "#f4f9fa", "#fdf7f3", "#fde7da", "#ffd5c3", "#f9c0a4", "#f0ab87", "#e79072", "#DB725B", "#D05245", "#CF5447", "#BF3739"),
    values = scales::rescale(c(-1, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)),
    limits = c(-1, 1)  
  ) +
  theme_minimal() +
  theme(
    legend.position = "right", 
    legend.box.just = "center", 
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.text.y = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.key.height = unit(matrix_height * 5, "cm")  
  ) +
  coord_fixed() +
  guides(color = guide_colorbar(barheight = unit(matrix_height * 1.8, "cm"))) +  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )


print(p)

ggsave(filename = "C:/Users/tuan/Desktop/Correlation Coefficients/Correlation Coefficients /Correlation Coefficients of Factors in Henan Province.png",
       plot = p,
       width = 10,  
       height = 10,
       dpi = 300)  









data <- read_excel("C:/Users/tuan/Desktop/Correlation Coefficients/Raw Data_Factors Only/Hainan.xlsx")


cor_matrix <- cor(data, use = "pairwise.complete.obs")  
rownames(cor_matrix) <- colnames(cor_matrix) <- colnames(data)


cor_df <- melt(cor_matrix)


cor_df$abs_value <- abs(cor_df$value)


cor_df$stars <- cut(cor_df$abs_value, 
                    breaks = c(-Inf, 0.25, 0.5, 0.75, Inf), 
                    labels = c("", "*", "**", "***"),
                    include.lowest = TRUE)


cor_df$stars <- as.character(cor_df$stars)


cor_df$Var1 <- as.character(cor_df$Var1)
cor_df$Var2 <- as.character(cor_df$Var2)


cor_df$label <- ifelse(cor_df$Var1 < cor_df$Var2, 
                       sprintf("%.2f", cor_df$value), 
                       cor_df$stars)


matrix_height <- ncol(cor_matrix)


print(cor_df)


p <- ggplot(cor_df, aes(x = Var1, y = Var2)) +
  geom_tile(color = "black", fill = NA) +  
  geom_point(aes(size = abs_value, color = value), shape = 16) +  
  geom_text(aes(label = label), vjust = 0.5, color = "black", size = 5, na.rm = TRUE) + 
  scale_size_area(max_size = 25, guide = "none") +  
  scale_color_gradientn(
    colors = c("#3278b5", "#408ac1", "#57a0ca", "#599fc9", "#77b5d6", "#97c8dc", "#b5d5e7", "#cde2f1", "#e2edf5", "#f4f9fa", "#fdf7f3", "#fde7da", "#ffd5c3", "#f9c0a4", "#f0ab87", "#e79072", "#DB725B", "#D05245", "#CF5447", "#BF3739"),
    values = scales::rescale(c(-1, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)),
    limits = c(-1, 1)  
  ) +
  theme_minimal() +
  theme(
    legend.position = "right", 
    legend.box.just = "center", 
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.text.y = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.key.height = unit(matrix_height * 5, "cm")  
  ) +
  coord_fixed() +
  guides(color = guide_colorbar(barheight = unit(matrix_height * 1.8, "cm"))) +  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )


print(p)

ggsave(filename = "C:/Users/tuan/Desktop/Correlation Coefficients/Correlation Coefficients /Correlation Coefficients of Factors in Hainan Province.png",
       plot = p,
       width = 10,  
       height = 10,
       dpi = 300)  









data <- read_excel("C:/Users/tuan/Desktop/Correlation Coefficients/Raw Data_Factors Only/Jiangsu.xlsx")


cor_matrix <- cor(data, use = "pairwise.complete.obs")  
rownames(cor_matrix) <- colnames(cor_matrix) <- colnames(data)


cor_df <- melt(cor_matrix)


cor_df$abs_value <- abs(cor_df$value)


cor_df$stars <- cut(cor_df$abs_value, 
                    breaks = c(-Inf, 0.25, 0.5, 0.75, Inf), 
                    labels = c("", "*", "**", "***"),
                    include.lowest = TRUE)


cor_df$stars <- as.character(cor_df$stars)


cor_df$Var1 <- as.character(cor_df$Var1)
cor_df$Var2 <- as.character(cor_df$Var2)


cor_df$label <- ifelse(cor_df$Var1 < cor_df$Var2, 
                       sprintf("%.2f", cor_df$value), 
                       cor_df$stars)


matrix_height <- ncol(cor_matrix)


print(cor_df)


p <- ggplot(cor_df, aes(x = Var1, y = Var2)) +
  geom_tile(color = "black", fill = NA) +  
  geom_point(aes(size = abs_value, color = value), shape = 16) +  
  geom_text(aes(label = label), vjust = 0.5, color = "black", size = 5, na.rm = TRUE) + 
  scale_size_area(max_size = 25, guide = "none") +  
  scale_color_gradientn(
    colors = c("#3278b5", "#408ac1", "#57a0ca", "#599fc9", "#77b5d6", "#97c8dc", "#b5d5e7", "#cde2f1", "#e2edf5", "#f4f9fa", "#fdf7f3", "#fde7da", "#ffd5c3", "#f9c0a4", "#f0ab87", "#e79072", "#DB725B", "#D05245", "#CF5447", "#BF3739"),
    values = scales::rescale(c(-1, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)),
    limits = c(-1, 1)  
  ) +
  theme_minimal() +
  theme(
    legend.position = "right", 
    legend.box.just = "center", 
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.text.y = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.key.height = unit(matrix_height * 5, "cm")  
  ) +
  coord_fixed() +
  guides(color = guide_colorbar(barheight = unit(matrix_height * 1.8, "cm"))) +  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )


print(p)

ggsave(filename = "C:/Users/tuan/Desktop/Correlation Coefficients/Correlation Coefficients /Correlation Coefficients of Factors in Jiangsu Province.png",
       plot = p,
       width = 10,  
       height = 10,
       dpi = 300)  









data <- read_excel("C:/Users/tuan/Desktop/Correlation Coefficients/Raw Data_Factors Only/Hubei.xlsx")


cor_matrix <- cor(data, use = "pairwise.complete.obs")  
rownames(cor_matrix) <- colnames(cor_matrix) <- colnames(data)


cor_df <- melt(cor_matrix)


cor_df$abs_value <- abs(cor_df$value)


cor_df$stars <- cut(cor_df$abs_value, 
                    breaks = c(-Inf, 0.25, 0.5, 0.75, Inf), 
                    labels = c("", "*", "**", "***"),
                    include.lowest = TRUE)


cor_df$stars <- as.character(cor_df$stars)


cor_df$Var1 <- as.character(cor_df$Var1)
cor_df$Var2 <- as.character(cor_df$Var2)


cor_df$label <- ifelse(cor_df$Var1 < cor_df$Var2, 
                       sprintf("%.2f", cor_df$value), 
                       cor_df$stars)


matrix_height <- ncol(cor_matrix)


print(cor_df)


p <- ggplot(cor_df, aes(x = Var1, y = Var2)) +
  geom_tile(color = "black", fill = NA) +  
  geom_point(aes(size = abs_value, color = value), shape = 16) +  
  geom_text(aes(label = label), vjust = 0.5, color = "black", size = 5, na.rm = TRUE) + 
  scale_size_area(max_size = 25, guide = "none") +  
  scale_color_gradientn(
    colors = c("#3278b5", "#408ac1", "#57a0ca", "#599fc9", "#77b5d6", "#97c8dc", "#b5d5e7", "#cde2f1", "#e2edf5", "#f4f9fa", "#fdf7f3", "#fde7da", "#ffd5c3", "#f9c0a4", "#f0ab87", "#e79072", "#DB725B", "#D05245", "#CF5447", "#BF3739"),
    values = scales::rescale(c(-1, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)),
    limits = c(-1, 1)  
  ) +
  theme_minimal() +
  theme(
    legend.position = "right", 
    legend.box.just = "center", 
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.text.y = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.key.height = unit(matrix_height * 5, "cm")  
  ) +
  coord_fixed() +
  guides(color = guide_colorbar(barheight = unit(matrix_height * 1.8, "cm"))) +  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )


print(p)

ggsave(filename = "C:/Users/tuan/Desktop/Correlation Coefficients/Correlation Coefficients /Correlation Coefficients of Factors in Hubei Province.png",
       plot = p,
       width = 10,  
       height = 10,
       dpi = 300)  









data <- read_excel("C:/Users/tuan/Desktop/Correlation Coefficients/Raw Data_Factors Only/Shandong.xlsx")


cor_matrix <- cor(data, use = "pairwise.complete.obs")  
rownames(cor_matrix) <- colnames(cor_matrix) <- colnames(data)


cor_df <- melt(cor_matrix)


cor_df$abs_value <- abs(cor_df$value)


cor_df$stars <- cut(cor_df$abs_value, 
                    breaks = c(-Inf, 0.25, 0.5, 0.75, Inf), 
                    labels = c("", "*", "**", "***"),
                    include.lowest = TRUE)


cor_df$stars <- as.character(cor_df$stars)


cor_df$Var1 <- as.character(cor_df$Var1)
cor_df$Var2 <- as.character(cor_df$Var2)


cor_df$label <- ifelse(cor_df$Var1 < cor_df$Var2, 
                       sprintf("%.2f", cor_df$value), 
                       cor_df$stars)


matrix_height <- ncol(cor_matrix)


print(cor_df)


p <- ggplot(cor_df, aes(x = Var1, y = Var2)) +
  geom_tile(color = "black", fill = NA) +  
  geom_point(aes(size = abs_value, color = value), shape = 16) +  
  geom_text(aes(label = label), vjust = 0.5, color = "black", size = 5, na.rm = TRUE) + 
  scale_size_area(max_size = 25, guide = "none") +  
  scale_color_gradientn(
    colors = c("#3278b5", "#408ac1", "#57a0ca", "#599fc9", "#77b5d6", "#97c8dc", "#b5d5e7", "#cde2f1", "#e2edf5", "#f4f9fa", "#fdf7f3", "#fde7da", "#ffd5c3", "#f9c0a4", "#f0ab87", "#e79072", "#DB725B", "#D05245", "#CF5447", "#BF3739"),
    values = scales::rescale(c(-1, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)),
    limits = c(-1, 1)  
  ) +
  theme_minimal() +
  theme(
    legend.position = "right", 
    legend.box.just = "center", 
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.text.y = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.key.height = unit(matrix_height * 5, "cm")  
  ) +
  coord_fixed() +
  guides(color = guide_colorbar(barheight = unit(matrix_height * 1.8, "cm"))) +  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )


print(p)

ggsave(filename = "C:/Users/tuan/Desktop/Correlation Coefficients/Correlation Coefficients /Correlation Coefficients of Factors in Shandong Province.png",
       plot = p,
       width = 10,  
       height = 10,
       dpi = 300)  










































