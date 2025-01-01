

library(readxl)
library(plotly)
library(htmlwidgets)
library(webshot)
############
#########Please replace input_folder and output_folder with your own paths for reading data and saving results, respectively.
#############################

input_folder <- "C:/.../.../.../original data/"
output_folder <- "C:/.../.../.../PCresult/"
provinces <- c(
  "Guangdong","Yunnan", "Fujian",  "Zhejiang",  "Guangxi","Chongqing", "Jiangxi", "Hunan", "Sichuan","Henan",  "Hainan", 
   "Jiangsu", "Hubei","Shandong","Beijing","Shanghai",  "Anhui","Hebei","Liaoning", "Shaanxi", "Guizhou","Heilongjiang", 
    "Gansu", "Jilin",  "Tianjin", "Shanxi", "Ningxia","InnerMongolia", "Xinjiang", "Qinghai"
)

theta_ordered <- c("Tx", "T", "Tn", "Td", "P", "RRR", "VV", "U", "Ff", "N", "GDP")


if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}



letter_index <- 1  
get_label <- function(index) {
  
  letters_combined <- c(letters, sapply(letters, function(x) paste0(x, letters)))
  return(letters_combined[index])
}

for (province in provinces) {
  
  file_path <- paste0(input_folder, "data_", province, ".xlsx")
  data <- read_excel(file_path)
  
  
  data_for_pca <- data %>% select(-matches("cases|DATE|count"))
  if (any(is.na(data_for_pca))) {
    data_for_pca <- na.omit(data_for_pca)
  }
  
  
  data_scaled <- scale(data_for_pca)
  
  
  pca_result <- prcomp(data_scaled, center = TRUE, scale. = TRUE)
  pca_loadings <- abs(pca_result$rotation[, 1:4])
  
  
  theta <- gsub("^[A-Za-z]+_", "", rownames(pca_loadings))
  
  
  r_data <- as.data.frame(t(pca_loadings))
  colnames(r_data) <- theta
  
  
  theta_valid <- theta_ordered[theta_ordered %in% theta]
  r_data_ordered <- r_data[, theta_valid]
  
  
  max_value <- 0.9
  tick_step <- 0.3
  vwidth <- 1100
  vheight <- 1100
  
  
  p <- plot_ly(
    type = 'scatterpolar',
    fill = 'toself'
  ) %>%
    add_trace(
      r = as.numeric(r_data_ordered[1, ]),
      theta = theta_ordered,
      name = 'PC1'
    ) %>%
    add_trace(
      r = as.numeric(r_data_ordered[2, ]),
      theta = theta_ordered,
      name = 'PC2'
    ) %>%
    add_trace(
      r = as.numeric(r_data_ordered[3, ]),
      theta = theta_ordered,
      name = 'PC3'
    ) %>%
    add_trace(
      r = as.numeric(r_data_ordered[4, ]),
      theta = theta_ordered,
      name = 'PC4'
    ) %>%
    layout(
      title = list(
        text = get_label(letter_index),  
        x = 0.5,
        yanchor = "bottom",
        font = list(size = 120)
      ),
      polar = list(
        radialaxis = list(
          visible = TRUE,
          range = c(0, max_value),
          dtick = tick_step,
          tickfont = list(size = 60)
        ),
        angularaxis = list(
          tickfont = list(size = 60)
        )
      ),
      margin = list(t = 190, b = 120, l = 120, r = 120),
      showlegend = FALSE
    )
  
 
  output_html <- paste0(output_folder, "radar_", province, ".html")
  output_png <- paste0(output_folder, "radar_", province, ".jpg")
  
  
  saveWidget(p, output_html, selfcontained = TRUE)
  
  
  webshot(output_html, output_png, vwidth = vwidth, vheight = vheight, zoom = 3)
  
  
  letter_index <- letter_index + 1
}


cat("finish", output_folder, "\n")








###############
# the cumulative contribution rate of the first four principal components


cumulative_variance <- data.frame(Province = character(), CumulativeVariance = numeric())

for (province in provinces) {
  
  file_path <- paste0(input_folder, "data_", province, ".xlsx")
  data <- read_excel(file_path)
  
  data_for_pca <- data %>% select(-matches("cases|DATE|count"))
  if (any(is.na(data_for_pca))) {
    data_for_pca <- na.omit(data_for_pca)
  }
  
  data_scaled <- scale(data_for_pca)
  
  pca_result <- prcomp(data_scaled, center = TRUE, scale. = TRUE)
  explained_variance <- summary(pca_result)$importance[2, ]  
  cumulative_variance_province <- sum(explained_variance[1:4])  
  
  
  cumulative_variance <- rbind(
    cumulative_variance,
    data.frame(Province = province, CumulativeVariance = cumulative_variance_province)
  )
}

# 输出结果
print(cumulative_variance)


output_csv <- paste0(output_folder, "cumulative_variance.csv")
write.csv(cumulative_variance, file = output_csv, row.names = FALSE)

cat("The cumulative contribution rate of the first four principal components has been calculated and saved to ：", output_csv, "\n")







################
#############the factor loadings of each variable in the first four principal components for each province

library(readxl)
library(openxlsx) 
library(dplyr)


wb <- createWorkbook()


for (province in provinces) {
  
  file_path <- paste0(input_folder, "data_", province, ".xlsx")
  data <- read_excel(file_path)
  
  data_for_pca <- data %>% select(-matches("cases|DATE|count"))
  if (any(is.na(data_for_pca))) {
    data_for_pca <- na.omit(data_for_pca)
  }
  
  data_scaled <- scale(data_for_pca)
  
 
  pca_result <- prcomp(data_scaled, center = TRUE, scale. = TRUE)
  pca_loadings <- abs(pca_result$rotation[, 1:4]) 
  

  loadings_df <- as.data.frame(pca_loadings)
  loadings_df$Variable <- rownames(pca_loadings)
  
  
  loadings_df <- loadings_df %>%
    relocate(Variable, .before = PC1)
  
  
  addWorksheet(wb, sheetName = province)
  
  
  writeData(wb, sheet = province, x = loadings_df)
}


output_excel <- paste0(output_folder, "PCA_Loadings_ByProvince.xlsx")
saveWorkbook(wb, output_excel, overwrite = TRUE)

cat("principal component factor loadings (sorted by absolute value) have been saved：", output_excel, "\n")



















