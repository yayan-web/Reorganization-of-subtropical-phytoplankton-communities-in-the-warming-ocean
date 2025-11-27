setwd("D:/论文/大亚湾/18S扩增子/Daya Bay 18S/edge_inf/net_data")

edge_files = list.files()
edge_files = edge_files[grep(".csv", edge_files)]
len = length(edge_files)

# Initialize an empty data frame with 5 columns (for each threshold: 0.1, 0.3, 0.5, 0.7, 0.9)
df = as.data.frame(matrix(nrow = len, ncol = 6))
colnames(df) = c("0.1", "0.3", "0.5", "0.7", "0.9", "Temperature")

k = 0
library(igraph)
library(NetSwan)

for (i in edge_files) {
  k = k + 1
  Temperature = gsub(".csv", "", i)
  # Read the CSV file
  temp_file = read.csv(i)
  
  # Define nodes from the Source and Target columns
  nodes = unique(c(temp_file$Source, temp_file$Target))
  
  # Generate the graph
  gra = graph.edgelist(as.matrix(temp_file[, c("Source", "Target")]), directed = FALSE)
  
  # Apply swan_combinatory function (check output format of swan_combinatory)
  f4 <- swan_combinatory(gra, 10)
  f4 = as.data.frame(f4)
  colnames(f4) = c("remove", "betweeness", "degree", "cascading", "random")
  
  # For each threshold in the list, calculate max removal
  for (rand in c(0.1, 0.3, 0.5, 0.7, 0.9)) {
    f5 = subset(f4, random < rand)
    max_remove = max(f5$remove) * length(nodes)
    
    # Assign max_remove to the corresponding column in df
    df[k, as.character(rand)] = max_remove
  }
  df$Temperature[k] = Temperature
  
}

head(df)

colnames(df) = c("10%", "30%", "50%", "70%", "90%", "Temperature")


df_long = melt(df, id.vars = c("Temperature"))

df_long$Temperature = as.numeric(df_long$Temperature)

head(df_long)

p = ggplot(df_long, aes(x = Temperature, y = value)) +
  labs(x = "Temperature [°C]",
       y = "Max remore nodes") +
  theme_bw() +
  #  geom_vline(xintercept = 26.6, linewidth = 2) +
  geom_point(alpha = 0.5) +
  facet_wrap(~variable, scales = "free_y") +
  #  geom_smooth(method = "glm", linewidth = 2, se = F, formula = y ~ x+I(x^2)) +
  geom_smooth() +
  theme(legend.position = "none",
        text = element_text(family = "serif", face = "bold"),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 10),
        axis.title = element_text(size = 15)) 
# x11()
p

















