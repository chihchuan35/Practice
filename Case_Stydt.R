Superstore_Original <- Superstore
Season_sale <- Season_sale

library(tidyverse)
library(ggplot2)
library(knitr)
library(readxl)
library(gridExtra)
library(cowplot)
--------------------------------------------------------
Season_sale_total <- Season_sale %>%
  group_by(Season) %>%
  summarise(
    total_sales = sum(total_sales, na.rm = TRUE),
    total_Quantity = sum(total_Quantity, na.rm = TRUE),
    total_Profit = sum(totla_Profit, na.rm = TRUE)
  )

head(Season_sale_total)

plot1 <- ggplot(data = Season_sale_total) +
  geom_bar(mapping = aes(x = Season, y = total_sales, fill = Season), stat = "identity") +
  scale_fill_manual(values = c("Spring" = "red", "Summer" = "green", "Autumn" = "orange", "Winter" = "blue")) +
  labs(title = "Total Sales", subtitle = "Based on Season", x = "Season", y = "Sales($)") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.text.x = element_blank(),
    axis.title.y = element_text(face = "bold", size = 12))

plot2 <- ggplot(data = Season_sale_total) +
  geom_bar(mapping = aes(x = Season, y = total_Quantity, fill = Season), stat = "identity") +
  scale_fill_manual(values = c("Spring" = "red", "Summer" = "green", "Autumn" = "orange", "Winter" = "blue")) +
  labs(title = "Total Quantity", subtitle = "Based on Season", x = "Season", y = "Quantity") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.text.x = element_blank(),
    axis.title.y = element_text(face = "bold", size = 12))

plot3 <- ggplot(data = Season_sale_total) +
  geom_bar(mapping = aes(x = Season, y = total_Profit, fill = Season), stat = "identity") +
  scale_fill_manual(values = c("Spring" = "red", "Summer" = "green", "Autumn" = "orange", "Winter" = "blue")) +
  labs(title = "Total Profit", subtitle = "Based on Season", x = "Season", y = "Profit($)") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.text.x = element_blank(),
    axis.title.y = element_text(face = "bold", size = 12))

grid.arrange(plot1,plot2,plot3, ncol = 2)

---------------------------------------------------------
season_category_summary <- Season_sale %>%
  group_by(Season, Category, sub.Category) %>%
  summarise(
    total_Quantity_sold = sum(total_Quantity, na.rm = TRUE),  
    total_sales = sum(total_sales, na.rm = TRUE),  
    total_Profit = sum(totla_Profit, na.rm = TRUE)  
  ) %>%
  arrange(Season, Category, sub.Category)

head(Season_sale_category)

Autumn_data <- season_category_summary %>%
  filter(Season == "Autumn")

Except_Autumn_data <- season_category_summary %>%
  filter(Season != "Autumn")

category_totla_profit <- Autumn_data %>%
  group_by(Category) %>%
  summarise(Profit = sum(total_Profit, na.rm = TRUE))

Except_Autumn_category_totla_profit <- Except_Autum_data %>%
  group_by(Category) %>%
  summarise(Profit = sum(total_Profit, na.rm = TRUE))


plot4 <- ggplot(data = Autumn_data) +
  geom_bar(mapping = aes(x = Category, y = total_Quantity_sold, fill = Category),stat = "identity", position = "stack") +
  labs(title = "Autumn Sales Quantity", x = "Category", y = "Quantity") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.text.x = element_blank(),
    axis.title.y = element_text(face = "bold", size = 12))

plot5 <- ggplot(data = Autumn_data) +
  geom_bar(mapping = aes(x = Category, y = total_sales, fill = Category),stat = "identity", position = "stack") +
  labs(title = "Autumn Sales ", x = "Category", y = "Sales($)") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.text.x = element_blank(),
    axis.title.y = element_text(face = "bold", size = 12))

plot6 <- ggplot(data = category_totla_profit) +
  geom_bar(mapping = aes(x = Category, y = Profit, fill = Category),stat = "identity", position = "stack") +
  labs(title = "Autumn Profit ", x = "Category", y = "Profit($)") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.text.x = element_blank(),
    axis.title.y = element_text(face = "bold", size = 12))

plot7 <- ggplot(data = Except_Autumn_data) +
  geom_bar(mapping = aes(x = Category, y = total_Quantity_sold, fill = Category),stat = "identity", position = "dodge") +
  labs(title = "Autumn Sales Quantity", x = "Category", y = "Quantity") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.text.x = element_blank(),
    axis.title.y = element_text(face = "bold", size = 12))


grid.arrange(plot4,plot5,plot6, ncol = 2)



--------------------------------------------------------
  


Furniture_total <- Autumn_data %>%
  filter(Category == "Furniture") %>%
  summarise(total_Furniture_Quantity = sum(total_Quantity_sold)) %>%
  pull(total_Furniture_Quantity)

Furnishings_Percentage_Quantity <- Autumn_data %>%
  filter(Category == "Furniture", sub.Category == "Furnishings") %>%
  summarise(Furnishings_Quantity = sum(total_Quantity_sold)) %>%
  mutate(Furnishings_Percentage = (Furnishings_Quantity / Furniture_total) * 100)

print(Furnishings_Percentage_Quantity)



ggplot(data = Autumn_data, aes(x = Category, y = total_Quantity_sold, fill = sub.Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#FF6347", "#4682B4", "#32CD32", "#FFD700", "#8A2BE2", 
                               "#FF4500", "#2E8B57", "#FF69B4", "#40E0D0", "#6A5ACD", 
                               "#D2691E", "#DC143C", "#228B22", "#8B0000", "#00008B", 
                               "#20B2AA", "#DAA520")) +
  labs(title = "Sales Quantity in Autumn by Category and Sub-Category",
     x = "Category",
     y = "Total Quantity Sold") +
  theme_minimal()

ggplot(data = Autumn_data, aes(x = Category, y = total_sales, fill = sub.Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#FF6347", "#4682B4", "#32CD32", "#FFD700", "#8A2BE2", 
                               "#FF4500", "#2E8B57", "#FF69B4", "#40E0D0", "#6A5ACD", 
                               "#D2691E", "#DC143C", "#228B22", "#8B0000", "#00008B", 
                               "#20B2AA", "#DAA520")) +
  labs(title = "Sales Quantity in Autumn by Category and Sub-Category",
       x = "Category",
       y = "Total Quantity Sold") +
  theme_minimal()

ggplot(data = Autumn_data, aes(x = Category, y = total_Profit, fill = sub.Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#FF6347", "#4682B4", "#32CD32", "#FFD700", "#8A2BE2", 
                               "#FF4500", "#2E8B57", "#FF69B4", "#40E0D0", "#6A5ACD", 
                               "#D2691E", "#DC143C", "#228B22", "#8B0000", "#00008B", 
                               "#20B2AA", "#DAA520")) +
  labs(title = "Sales Quantity in Autumn by Category and Sub-Category",
       x = "Category",
       y = "Total Quantity Sold") +
  theme_minimal()


ggplot(data = Autumn_data, aes(x = Category, y = total_Quantity_sold, fill = sub.Category)) +
  geom_bar(stat = "identity", position = "stack") +  
  scale_fill_manual(values = c("#FF6347", "#4682B4", "#32CD32", "#FFD700", "#8A2BE2", 
                               "#FF4500", "#2E8B57", "#FF69B4", "#40E0D0", "#6A5ACD", 
                               "#D2691E", "#DC143C", "#228B22", "#8B0000", "#00008B", 
                               "#20B2AA", "#DAA520")) +
  labs(title = "Sales Quantity in Autumn by Category and Sub-Category",
       x = "Category",
       y = "Total Quantity Sold") +
  theme_minimal() +
  theme(legend.position = "right")
  
-----------
  
furniture_data <- filter(Autumn_data, Category == "Furniture")

ggplot(data = furniture_data, aes(x = Category, y = total_Quantity_sold, fill = sub.Category)) +
  geom_bar(stat = "identity", position = "stack",width = 0.25) +  
  scale_fill_manual(values = c("#8A2BE2", "#FF4500","#6A5ACD", "#DAA520")) +
  labs(title = "Sales Quantity in Autumn" , subtitle = "by Category and Sub-Category",
       x = "Category",
       y = "Total Quantity Sold") +
  theme_minimal() +
  theme(legend.position = "right")

ggplot(data = furniture_data, aes(x = Category, y = total_sales, fill = sub.Category)) +
  geom_bar(stat = "identity", position = "stack",width = 0.25) +  
  scale_fill_manual(values = c("#8A2BE2", "#FF4500","#6A5ACD", "#DAA520")) +
  labs(title = "Sales  in Autumn" , subtitle = "by Category and Sub-Category",
       x = "Category",
       y = "Total Quantity Sold") +
  theme_minimal() +
  theme(legend.position = "right")

ggplot(data = furniture_data, aes(x = Category, y = total_Profit, fill = sub.Category)) +
  geom_bar(stat = "identity", position = "dodge",width = 0.25) +  
  scale_fill_manual(values = c("#8A2BE2", "#FF4500","#6A5ACD", "#DAA520")) +
  labs(title = "Sales  in Autumn" , subtitle = "by Category and Sub-Category",
       x = "Category",
       y = "Total Quantity Sold") +
  theme_minimal() +
  theme(legend.position = "right")


furniture_long <- furniture_data %>%
  pivot_longer(cols = c(total_sales, total_Profit, total_Quantity_sold), 
               names_to = "Metric", 
               values_to = "Value")

facet_labels <- c(
  total_Profit = "Profit",
  total_sales = "Sales",
  total_Quantity_sold = "Quantity Sold"
)

ggplot(data = furniture_long, aes(x = sub.Category, y = Value, fill = sub.Category)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  
  scale_fill_manual(values = c("#8A2BE2", "#FF4500", "#6A5ACD", "#DAA520")) +
  facet_wrap(~ Metric, scales = "free_y", labeller = as_labeller(facet_labels)) +  
  labs(title = "Sales and Profit in Autumn by Sub-Category",
       x = "furniture", 
       y = "Value") +
  theme_minimal() +
  theme(legend.position = "right", 
        axis.text.x = element_text(angle = 45, hjust = 1))


Office_Supplies_data <- filter(Autumn_data, Category == "Office Supplies")

Office_Supplies_long <- Office_Supplies_data %>%
  pivot_longer(cols = c(total_sales, total_Profit, total_Quantity_sold), 
               names_to = "Metric", 
               values_to = "Value")

ggplot(data = Office_Supplies_long, aes(x = sub.Category, y = Value, fill = sub.Category)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  
  scale_fill_manual(values = c("#4682B4", "#32CD32", "#FFD700", "#FF69B4", "#40E0D0", "#D2691E", "#228B22", "#00008B", "#20B2AA")) +
  facet_wrap(~ Metric, scales = "free_y", labeller = as_labeller(facet_labels)) +  
  labs(title = "Sales and Profit in Autumn by Sub-Category",
       x = "Office_Supplies", 
       y = "Value") +
  theme_minimal() +
  theme(legend.position = "right", 
        axis.text.x = element_text(size = 6,angle = 45, hjust = 1))

Technology_data <- filter(Autumn_data, Category == "Technology")

Technology_long <- Technology_data %>%
  pivot_longer(cols = c(total_sales, total_Profit, total_Quantity_sold), 
               names_to = "Metric", 
               values_to = "Value")

ggplot(data = Technology_long, aes(x = sub.Category, y = Value, fill = sub.Category)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  
  scale_fill_manual(values = c("#FF6347", "#2E8B57" , "#DC143C","#8B0000" )) +
  facet_wrap(~ Metric, scales = "free_y", labeller = as_labeller(facet_labels)) +  
  labs(title = "Sales and Profit in Autumn by Sub-Category",
       x = "Technology", 
       y = "Value") +
  theme_minimal() +
  theme(legend.position = "right", 
        axis.text.x = element_text(size = 6,angle = 45, hjust = 1))
--------------------
  
Customer_data <- Customer

Region_Buy <- Customer_data %>%
  group_by(Region, Category, `sub.Category`, Segment,Season) %>%
  summarise(Quantity = sum(total_Quantity))

Region_Buy_Autumn <- Region_Buy %>%
  filter(Season == 'Autumn')

plot8 <- ggplot(data = Region_Buy) +
  geom_bar(mapping = aes(x = Region, y = Quantity, fill = Segment),stat = "identity", position = "stack",width = 0.65) +
  labs(title = 'Total_Quantity_by_Region', subtitle = 'Segment Proportion')+
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45),
    axis.title.y = element_text(face = "bold", size = 12))

plot9 <- ggplot(data = Region_Buy_Autumn) +
  geom_bar(mapping = aes(x = Region, y = Quantity, fill = Segment),stat = "identity", position = "stack",width = 0.65) +
  labs(title = 'Total_Quantity_by_Region(Autunm)', subtitle = 'Segment Proportion')+
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45),
    axis.title.y = element_text(face = "bold", size = 12))


plot10 <- ggplot(data = Region_Buy) +
  geom_bar(mapping = aes(x = Region, y = Quantity, fill = Category),stat = "identity", position = "stack",width = 0.65) +
  labs(title = 'Total_Quantity_by_Region', subtitle = 'Category Proportion')+
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45),
    axis.title.y = element_text(face = "bold", size = 12))

plot11 <- ggplot(data = Region_Buy_Autunm) +
  geom_bar(mapping = aes(x = Region, y = Quantity, fill = Category),stat = "identity", position = "stack",width = 0.65) +
  labs(title = 'Total_Quantity_by_Region(Autunm)', subtitle = 'Category Proportion')+
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45),
    axis.title.y = element_text(face = "bold", size = 12))

grid.arrange(plot8,plot9,plot10,plot11,ncol = 2, nrow = 2)


ggplot(data = Region_Buy) +
  geom_bar(mapping = aes(x = Region, y = Quantity, fill = sub.Category),stat = "identity", position = "stack",width = 0.65) +
  labs(title = 'Total_Quantity_by_Region', subtitle = 'Category Proportion')+
  scale_fill_manual(values = c("#FF6347", "#4682B4", "#32CD32", "#FFD700", "#8A2BE2", 
                               "#FF4500", "#2E8B57", "#FF69B4", "#40E0D0", "#6A5ACD", 
                               "#D2691E", "#DC143C", "#228B22", "#8B0000", "#00008B", 
                               "#20B2AA", "#DAA520")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45),
    axis.title.y = element_text(face = "bold", size = 12))

ggplot(data = Region_Buy_Autunm) +
  geom_bar(mapping = aes(x = Region, y = Quantity, fill = sub.Category),stat = "identity", position = "stack",width = 0.65) +
  labs(title = 'Total_Quantity_by_Region(Autunm)', subtitle = 'Category Proportion')+
  scale_fill_manual(values = c("#FF6347", "#4682B4", "#32CD32", "#FFD700", "#8A2BE2", 
                               "#FF4500", "#2E8B57", "#FF69B4", "#40E0D0", "#6A5ACD", 
                               "#D2691E", "#DC143C", "#228B22", "#8B0000", "#00008B", 
                               "#20B2AA", "#DAA520")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45),
    axis.title.y = element_text(face = "bold", size = 12))

Season_Buy <- Customer %>%
  group_by(Region,Season,Category,Segment,sub.Category) %>%
  summarise(Quantity = sum(total_Quantity))

Season_Buy_summary <- Season_Buy %>%
  group_by(Region, Season,Category) %>%
  summarise(total_Quantity = sum(Quantity))

Season_Buy$Season <- factor(Season_Buy$Season, levels = c("Spring", "Summer", "Autumn", "Winter"))


ggplot(data = Season_Buy_summary, aes(x = Season, y = total_Quantity, group = Region, color = Region)) +
  geom_line(size = 1.2) +  
  geom_point(size = 3) +   
  labs(title = "Seasonal Quantity Sold by Region",
       x = "Season",
       y = "Total Quantity Sold") +
  theme_minimal() +
  theme(legend.position = "right")

Discount_Average <- Discount

Region_Profit <- Customer_data %>%
  group_by(Region, Category, Season,sub.Category) %>%
  summarise(Profit = sum(total_Profit))

Region_Profit$Season <- factor(Region_Profit$Season, levels = c("Spring", "Summer", "Autumn", "Winter"))

Region_Profit_East <- Region_Profit %>%
  filter(Region == 'East')

Region_Profit_South <- Region_Profit %>%
  filter(Region == 'South')



plot12 <- ggplot(data = Region_Profit_East, aes(x = Season, y = Profit, fill = Category)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(title = "Total Profit in East by Season and Category",
         x = "Season",
         y = "Total Profit") +
    theme_minimal() +
    theme(legend.position = "right")
  
plot13 <- ggplot(data = Region_Profit_South, aes(x = Season, y = Profit, fill = Category)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(title = "Total Profit in South by Season and Category",
         x = "Season",
         y = "Total Profit") +
    theme_minimal() +
    theme(legend.position = "right")

grid.arrange(plot12,plot13)

Season_Buy_summary_East <- Season_Buy_summary %>%
  filter(Region == 'East')

Season_Buy_summary_South <- Season_Buy_summary %>%
  filter(Region == 'South')

plot14 <- ggplot(data = Season_Buy_summary_East, aes(x = Season, y = total_Quantity, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Total Quantity in East by Season and Category",
       x = "Season",
       y = "Total Quantity ") +
  theme_minimal() +
  theme(legend.position = "right")


plot15 <- ggplot(data = Season_Buy_summary_South, aes(x = Season, y = total_Quantity, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Total Quantity in South by Season and Category",
       x = "Season",
       y = "Total Quantity ") +
  theme_minimal() +
  theme(legend.position = "right")

grid.arrange(plot14,plot15)

Season_Buy_summary_sub <- Season_Buy %>%
  group_by(Region, Season,Category, `sub.Category`) %>%
  summarise(total_Quantity = sum(Quantity))


Season_Buy_summary_sub_South_Autunm <- Season_Buy_summary_sub %>%
  filter(Region == 'South' & Season == 'Autumn')

Season_Buy_summary_sub_East_Autunm <- Season_Buy_summary_sub %>%
  filter(Region == 'East' & Season == 'Autumn')

Season_Buy_summary_sub_East_Autunm_filter <- Season_Buy_summary_sub %>%
  filter(total_Quantity > 0)

plot17 <- ggplot(data = Season_Buy_summary_sub_South_Autunm, aes(x = Category, y = total_Quantity, fill = sub.Category)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("#FF6347", "#4682B4", "#32CD32", "#FFD700", "#8A2BE2", 
                               "#FF4500", "#2E8B57", "#FF69B4", "#40E0D0", "#6A5ACD", 
                               "#D2691E", "#DC143C", "#228B22", "#8B0000", "#00008B", 
                               "#20B2AA", "#DAA520")) +
  labs(title = "Total Quantity in South in Autunm",
       x = "Category",
       y = "Total Quantity ") +
  theme_minimal() +
  theme(legend.position = "right")+
  ylim(0, 2500)

plot16 <- ggplot(data = Season_Buy_summary_sub_East_Autunm, aes(x = Category, y = total_Quantity, fill = sub.Category)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("#FF6347", "#4682B4", "#32CD32", "#FFD700", "#8A2BE2", 
                               "#FF4500", "#2E8B57", "#FF69B4", "#40E0D0", "#6A5ACD", 
                               "#D2691E", "#DC143C", "#228B22", "#8B0000", "#00008B", 
                               "#20B2AA", "#DAA520")) +
  labs(title = "Total Quantity in East in Autunm",
       x = "Category",
       y = "Total Quantity ") +
  theme_minimal() +
  theme(legend.position = 'none')+
  ylim(0, 2500)

grid.arrange(plot16,plot17, ncol=2)


total_quantity_south <- sum(Season_Buy_summary_sub_South_Autunm$total_Quantity)
total_quantity_east <- sum(Season_Buy_summary_sub_East_Autunm$total_Quantity)

Season_Buy_summary_sub_South_Autunm <- Season_Buy_summary_sub_South_Autunm %>%
  mutate(Percentage = (total_Quantity / total_quantity_south) * 100)

Season_Buy_summary_sub_East_Autunm <- Season_Buy_summary_sub_East_Autunm %>%
  mutate(Percentage = (total_Quantity / total_quantity_east) * 100)

plot18 <- ggplot(data = Season_Buy_summary_sub_South_Autunm, aes(x = Category, y = Percentage, fill = sub.Category)) +
  geom_bar(stat = "identity", position = "stack",width = 0.65) +
  scale_fill_manual(values = c("#FF6347", "#4682B4", "#32CD32", "#FFD700", "#8A2BE2", 
                               "#FF4500", "#2E8B57", "#FF69B4", "#40E0D0", "#6A5ACD", 
                               "#D2691E", "#DC143C", "#228B22", "#8B0000", "#00008B", 
                               "#20B2AA", "#DAA520")) +
  labs(title = "Percentage of Total Quantity in South in Autumn",
       x = "Category",
       y = "Percentage (%)") +
  theme_minimal() +
  theme(legend.position = "right")

plot19 <- ggplot(data = Season_Buy_summary_sub_East_Autunm, aes(x = Category, y = Percentage, fill = sub.Category)) +
  geom_bar(stat = "identity", position = "stack",width = 0.65) +
  scale_fill_manual(values = c("#FF6347", "#4682B4", "#32CD32", "#FFD700", "#8A2BE2", 
                               "#FF4500", "#2E8B57", "#FF69B4", "#40E0D0", "#6A5ACD", 
                               "#D2691E", "#DC143C", "#228B22", "#8B0000", "#00008B", 
                               "#20B2AA", "#DAA520")) +
  labs(title = "Percentage of Total Quantity in East in Autumn",
       x = "Category",
       y = "Percentage (%)") +
  theme_minimal() +
  theme(legend.position = 'right')

grid.arrange(plot18, plot19, nrow = 2)


Region_Profit_East_Autumn <- Region_Profit %>%
  filter(Region == 'East' & Season == 'Autumn')

Region_Profit_South_Autumn <- Region_Profit %>%
  filter(Region == 'South'& Season =='Autumn')

ggplot(data = Region_Profit_East_Autumn) +
  geom_bar(mapping= aes(x = sub.Category, y = Profit, fill = sub.Category),stat = "identity", position = "stack")+
  scale_fill_manual(values = c("#FF6347", "#4682B4", "#32CD32", "#FFD700", "#8A2BE2", 
                               "#FF4500", "#2E8B57", "#FF69B4", "#40E0D0", "#6A5ACD", 
                               "#D2691E", "#DC143C", "#228B22", "#8B0000", "#00008B", 
                               "#20B2AA", "#DAA520")) +
  labs(title = "SubCategory Profit in East in Autumn")+
  theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
      axis.title.x = element_text(face = "bold", size = 12),
      axis.text.x = element_text(angle = 60),
      axis.title.y = element_text(face = "bold", size = 12))

ggplot(data = Region_Profit_East_Autumn) +
  geom_bar(mapping= aes(x = sub.Category, y = Profit, fill = Category),stat = "identity", position = "stack")+
  scale_fill_manual(values = c("#FF6347", "#4682B4", "#32CD32", "#FFD700", "#8A2BE2", 
                               "#FF4500", "#2E8B57", "#FF69B4", "#40E0D0", "#6A5ACD", 
                               "#D2691E", "#DC143C", "#228B22", "#8B0000", "#00008B", 
                               "#20B2AA", "#DAA520")) +
  labs(title = "SubCategory Profit in East in Autumn", subtitle = "Fill by category")+
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 60),
    axis.title.y = element_text(face = "bold", size = 12))



ggplot(data = Region_Profit_South_Autumn) +
  geom_bar(mapping= aes(x = sub.Category, y = Profit, fill = sub.Category),stat = "identity", position = "stack")+
  scale_fill_manual(values = c("#FF6347", "#4682B4", "#32CD32", "#FFD700", "#8A2BE2", 
                               "#FF4500", "#2E8B57", "#FF69B4", "#40E0D0", "#6A5ACD", 
                               "#D2691E", "#DC143C", "#228B22", "#8B0000", "#00008B", 
                               "#20B2AA", "#DAA520")) +
  labs(title = "SubCategory Profit in South in Autumn")+
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 60),
    axis.title.y = element_text(face = "bold", size = 12))

ggplot(data = Region_Profit_South_Autumn) +
  geom_bar(mapping= aes(x = sub.Category, y = Profit, fill = Category),stat = "identity", position = "stack")+
  scale_fill_manual(values = c("#FF6347", "#4682B4", "#32CD32", "#FFD700", "#8A2BE2", 
                               "#FF4500", "#2E8B57", "#FF69B4", "#40E0D0", "#6A5ACD", 
                               "#D2691E", "#DC143C", "#228B22", "#8B0000", "#00008B", 
                               "#20B2AA", "#DAA520")) +
  labs(title = "SubCategory Profit in South in Autumn", subtitle = "Fill by Category")+
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 60),
    axis.title.y = element_text(face = "bold", size = 12))

Region_Machine_Profit <- Region_Profit %>%
  group_by(Region,Season) %>%
  filter(sub.Category == "Machines") %>%
  summarise(Machine_Profit = sum(Profit))

Region_Machine_Profit$Season <- factor(Region_Machine_Profit$Season, levels = c("Spring", "Summer", "Autumn", "Winter"))

ggplot(data = Region_Machine_Profit) + 
  geom_bar(mapping = aes(x = Season, y = Machine_Profit, fill = Region),stat = 'identity', position = 'dodge')

ggplot(data = Region_Machine_Profit, aes(x = Season, y = Machine_Profit, color = Region, group = Region)) + 
  geom_line(linewidth = 1.5) +  
  geom_point(size = 3) +   
  labs(title = "Machine Profit by Season and Region", 
       x = "Season", 
       y = "Machine Profit") + 
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )

Gross_Profit_Margin <- Gross_Profit_Margin

#Region_Gross_Profit_Margin <- Gross_Profit_Margin %>%
  #group_by(Region)

Gross_Profit_Margin$Season <- factor(Gross_Profit_Margin$Season,levels = c("Spring", "Summer", "Autumn", "Winter"))

Gross_Profit_Margin_sum <- Gross_Profit_Margin %>%
  group_by(Region,Season) %>%
  summarise(GPM = mean(Gross_Profit_Margin))

ggplot(data = Gross_Profit_Margin_sum, aes(x = Season, y = GPM, color = Region, group = Region)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3)


Region_Buy_Machine <- Region_Buy %>%
  filter(sub.Category == "Machines")

ggplot(data = Region_Buy_Machine) +
  geom_bar(mapping = aes(x = Region, y = Quantity), stat = 'identity', position = 'dodge' )

region_sale <- Customer %>%
  filter(sub.Category == 'Machines') %>%
  group_by(Season, Region,sub.Category) %>%
  summarise(Sales = sum(total_sales),
            Profit = sum(total_Profit),
            Quantity = sum(total_Quantity))

region_sale_Autumn <- region_sale %>%
  filter(Season == 'Autumn')

ggplot(data = region_sale, aes(x = Region, y = Sales)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "Machines Sales Distribution by Region", x = "Region", y = "Sales") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12)
  )

ggplot(data = region_sale, aes(x = Region, y = Profit)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "Machines Profit Distribution by Region", x = "Region", y = "Profit") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12)
  )

ggplot(data = region_sale, aes(x = Region, y = Quantity)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "Machines Quantity Distribution by Region", x = "Region", y = "Quantity") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12)
  )

region_sale_Autumn <- region_sale %>%
  filter(Season == 'Autumn')

autumn_region_sale_GrossProfitMargin  <- region_sale_Autumn %>%
  mutate(Gross_Profit_Margin = (Profit / Sales) * 100)

average_GPM_Autumn <- mean(autumn_region_sale_GrossProfitMargin$Gross_Profit_Margin, na.rm = TRUE)

print(paste("The average Gross Profit Margin for Autumn is:", round(average_GPM_Autumn, 2), "%"))

average_GPM_by_region_autumn <- autumn_region_sale_GrossProfitMargin %>%
  group_by(Region) %>%
  summarise(Average_GPM = mean(Gross_Profit_Margin, na.rm = TRUE))

print(average_GPM_by_region_autumn)

ggplot(data = average_GPM_by_region_autumn, aes(x = Region, y = Average_GPM, fill = Region)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "Average Gross Profit Margin in Autumn by Region", 
       x = "Region", 
       y = "Average Gross Profit Margin (%)") +
  scale_fill_manual(values = c("#FF6347", "#4682B4", "#32CD32", "#FFD700")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    legend.position = "none"
  )


total_profit <- sum(region_sale$Profit, na.rm = TRUE)
total_sales <- sum(region_sale$Sales, na.rm = TRUE)
overall_GPM <- (total_profit / total_sales) * 100
print(paste("The overall Gross Profit Margin for Autumn is:", round(overall_GPM, 2), "%"))


region_sale_GrossProfitMargin  <- region_sale %>%
  mutate(Gross_Profit_Margin = (Profit / Sales) * 100)

average_GPM <- mean(region_sale_GrossProfitMargin$Gross_Profit_Margin, na.rm = TRUE)

print(paste("The average Gross Profit Margin for Autumn is:", round(average_GPM, 2), "%"))