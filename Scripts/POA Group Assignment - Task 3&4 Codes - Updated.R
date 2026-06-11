# To update the column names which has a space and - with it to '_'
colnames(Transactions) <- gsub(" ", "_", colnames(Transactions))
colnames(Transactions) <- gsub("-", "_", colnames(Transactions))

# Task 2 - ETL Process for Warranty Sales
library(dplyr)
library(ggplot2)

# Summarise total warranty sales by month and product
monthly_product_warranty <- Transactions %>%
  group_by(Month, `Product_type`) %>%
  summarise(Total_Warranty_Sales = sum(`Warranty_Sales`, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Month = factor(Month, 
                        levels = c("January", "February", "March", "April", "May", "June",
                                   "July", "August", "September", "October", "November", "December")))

# GGPlot Warranty Sales by Product Type per Month
ggplot(monthly_product_warranty, aes(x = Month, y = Total_Warranty_Sales, color = `Product_type`, group = `Product_type`)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 3) +
  labs(title = "Warranty Sales by Product Type per Month",
       x = "Month",
       y = "Total Warranty Sales",
       color = "Product Type") +
  theme_minimal()

# Summarise total warranty sales by month and sector
monthly_sector_warranty <- Transactions %>%
  group_by(Month, Sector) %>%
  summarise(Total_Warranty_Sales_Sector = sum(`Warranty_Sales`, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Month = factor(Month, 
                        levels = c("January", "February", "March", "April", "May", "June",
                                   "July", "August", "September", "October", "November", "December")))

# GGPlot Warranty Sales by Sector per Month
ggplot(monthly_sector_warranty, aes(x = Month, y = Total_Warranty_Sales_Sector, color = Sector, group = Sector)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 3) +
  labs(title = "Warranty Sales by Sector per Month",
       x = "Month",
       y = "Total Warranty Sales",
       color = "Sector") +
  theme_minimal()

# Summarise total warranty sales by product type and region
product_type_country_warranty <- Transactions %>%
  group_by(`Product_type`, Country) %>%
  summarise(Total_Warranty_Sales_Country = sum(`Warranty_Sales`, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(`Product type` = factor(`Product_type`, 
                                 levels = c("Base", "Enhanced", "Workstation")))

# GGPlot for Warranty Sales by Product Type for Country
ggplot(product_type_country_warranty, aes(x = `Product_type`, y = Total_Warranty_Sales_Country, fill = Country)) +
  geom_col(position = position_dodge(width = 0.9), color = "black") +
  geom_text(aes(label = Country), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, 
            size = 3.5) +
  labs(title = "Warranty Sales by Product Type for Country",
       x = "Product Type",
       y = "Total Warranty Sales",
       fill = "Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Summarise total warranty sales by product type and sector
product_type_sector_warranty <- Transactions %>%
  group_by(`Product_type`, Sector) %>%
  summarise(Total_Warranty_Sales_Sector = sum(`Warranty_Sales`, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(`Product type` = factor(`Product_type`, 
                                 levels = c("Base", "Enhanced", "Workstation")))


# GGPlot for Warranty Sales by Product Type for Sector
ggplot(product_type_sector_warranty, aes(x = `Product_type`, y = Total_Warranty_Sales_Sector, fill = Sector)) +
  geom_col(position = position_dodge(width = 0.9), color = "black") +
  geom_text(aes(label = Sector), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, 
            size = 3.5) +
  labs(title = "Warranty Sales by Product Type for Sector",
       x = "Product Type",
       y = "Total Warranty Sales",
       fill = "Sector") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Task 3 - Exploratory Data Analysis

Transactions$Month <- factor(Transactions$Month,
                             levels = c("January", "February", "March", "April", "May", "June",
                                        "July", "August", "September", "October", "November", "December"))

# Multilinear reg: Sector, Product Type, & Promotion
lm_Sales_multi <- lm(Sales~Sector + Product_type + Promotion, data=Transactions)
summary(lm_Sales_multi)

# Multilinear reg: Sector & Product Type
lm_Sales_multi <- lm(Sales~Sector + Product_type, data=Transactions)
summary(lm_Sales_multi)

# Linear reg: Quantity
lm_Sales_Quantity <- lm(Sales~Quantity, data=Transactions)
summary(lm_Sales_Quantity)

# Multilinear reg: Quantity & Product Type
lm_multi <- lm(Sales ~ Quantity + Product_type, data = Transactions)
summary(lm_multi)

# Multilinear reg: Quantity, Sector, & Product Type (Base)
lm_multi <- lm(Sales ~ Quantity + Sector, data = subset(Transactions, Product_type == "Base"))
summary(lm_multi)

# GGPlot for Sales vs Quantity by Sector for Base Model
ggplot(data = subset(Transactions, Product_type == "Base")) + 
  geom_point(aes(x = Quantity, y = Sales, shape = as.factor(Product_type), color = as.factor(Sector)),
             size = 2,
             alpha = 0.8) +
  geom_smooth(aes(x = Quantity, y = Sales),
              method="lm",
              se=FALSE,
              color="brown",
              linetype="dashed") +
  labs(title = "Sales vs Quantity by Sector for Base Model",
       x = "Quantity Sold",
       y = "Sales (Revenue)",
       colour = "Sector") +
  theme_minimal()

# Multilinear reg: Quantity, Sector, & Product Type (Enhanced)
lm_multi <- lm(Sales ~ Quantity + Sector, data = subset(Transactions, Product_type == "Enhanced"))
summary(lm_multi)

# GGplot Sales vs Quantity by Sector for Enhanced Model
ggplot(data = subset(Transactions, Product_type == "Enhanced")) + 
  geom_point(aes(x = Quantity, y = Sales, shape = as.factor(Product_type), color = as.factor(Sector)),
             size = 2,
             alpha = 0.8) +
  geom_smooth(aes(x = Quantity, y = Sales),
              method="lm",
              se=FALSE,
              color="brown",
              linetype="dashed") +
  labs(title = "Sales vs Quantity by Sector for Enhanced Model",
       x = "Quantity Sold",
       y = "Sales (Revenue)",
       colour = "Sector") +
  theme_minimal()

# Multilinear reg: Quantity, Sector, & Product Type (Workstation)
lm_multi <- lm(Sales ~ Quantity + Sector, data = subset(Transactions, Product_type == "Workstation"))
summary(lm_multi)

# GGPlot for Sales vs Quantity by Sector for Workstation Model
ggplot(data = subset(Transactions, Product_type == "Workstation")) + 
  geom_point(aes(x = Quantity, y = Sales, shape = as.factor(Product_type), color = as.factor(Sector)),
             size = 2,
             alpha = 0.8) +
  geom_smooth(aes(x = Quantity, y = Sales),
              method="lm",
              se=FALSE,
              color="brown",
              linetype="dashed") +
  labs(title = "Sales vs Quantity by Sector for Workstation Model",
       x = "Quantity Sold",
       y = "Sales (Revenue)",
       colour = "Sector") +
  theme_minimal()

# GGPlot for Sales vs Quantity by Product Type & Sector
ggplot(data = Transactions) + 
  geom_point(aes(x = Quantity, y = Sales, colour = as.factor(Product_type), shape=as.factor(Sector)),
             size = 2,
             alpha = 0.8) +
  geom_smooth(aes(x = Quantity, y = Sales),
              method="lm",
              se=FALSE,
              color="black",
              linetype="dashed") +
  labs(title = "Sales vs Quantity by Product Type & Sector",
       x = "Quantity Sold",
       y = "Sales (Revenue)",
       colour = "Product Type") +
  theme_minimal()

# Multilinear reg: Sticker Price, Household Income, & Sector
lm_Sales_multi <- lm(Sales~Sticker_price + Household_Income + Sector, data=Transactions)
summary(lm_Sales_multi)

# Multilinear reg: Sticker Price, Quantity, Household Income, Sector, & Month
lm_Sales_multi <- lm(Sales~Sticker_price + Quantity + Household_Income + Sector + Month, data=Transactions)
summary(lm_Sales_multi)

# Multilinear reg: Sticker Price, Quantity, & Month
lm_Sales_multi <- lm(Sales~Sticker_price + Quantity + Month, data=Transactions)
summary(lm_Sales_multi)

# Multilinear reg (Log-Linear): Sticker Price, Quantity, & Month
lm_Sales_multi <- lm(log(Sales)~Sticker_price + Quantity + Month, data=Transactions)
summary(lm_Sales_multi)

# Multilinear reg (Log-Linear): Sticker Price, Country, Quantity, & Month
lm_Sales_multi <- lm(log(Sales)~Sticker_price + Country + Quantity + Month, data=Transactions)
summary(lm_Sales_multi)

#Box plots for Sector and Product Type
boxplot(Sales ~ interaction(Sector, Product_type), data=Transactions,
        main="Sales across Sector and Product type",
        xlab = "Sector and Product type", ylab = "Sales",
        col = c("lightblue", "lightgreen", "lightcoral", "yellow"),
        cex.axis = 0.75)



#Task 4 - Part A - Elasticity model

Transactions$log_Quantity <- log(Transactions$Quantity)
Transactions$log_Sticker_price <- log(Transactions$Sticker_price)

model_elasticity <- lm(log_Quantity ~ log_Sticker_price, data = Transactions)
summary(model_elasticity)

# GGPlot for Sticker Price Vs Quantity
ggplot(Transactions, aes(x = log_Quantity, y = log_Sticker_price)) +
  geom_point(alpha=0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Price Elasticity of Demand",
       x = "Quantity", y = "Price")

#Task 4 - Part B - With other independent variables
model_elasticity_full <- lm(log_Quantity ~ log_Sticker_price + Month + Sector + Household_Income + Promotion_Discount + Sales_Discount, data = Transactions)
summary(model_elasticity_full)


#Task 4 - Part C - Controlling other factors
model_elasticity_sec <- lm(log_Quantity ~ log_Sticker_price + Sector, data = Transactions)
summary(model_elasticity_sec)


#Task 4 - Final Part(Sales Adjusted)

# Default
Transactions$Seasonal_Adjustment <- 1  

# Retail
Transactions$Seasonal_Adjustment[Transactions$Sector == "Retail" & Transactions$Month %in% c("September", "December")] <- 2
Transactions$Seasonal_Adjustment[Transactions$Sector == "Retail" & Transactions$Month == "January"] <- 0.5

# Education
Transactions$Seasonal_Adjustment[Transactions$Sector == "Education" & Transactions$Month %in% c("July", "August")] <- 2

# Adjust quantity
Transactions$Adjusted_Quantity <- Transactions$Quantity / Transactions$Seasonal_Adjustment

# Elasticity model
model_elasticity <- lm(log(Adjusted_Quantity) ~ log(Sticker_price), data = Transactions)
summary(model_elasticity)

# Elasticity model with other independent variables for Adjusted quantity
model_elasticity_full <- lm(log(Adjusted_Quantity) ~ log(Sticker_price) + Month + Sector + Household_Income + Promotion_Discount + Sales_Discount, data = Transactions)
summary(model_elasticity_full)

# Log of Adjusted Quantity
Transactions$log_Adjusted_Quantity <- log(Transactions$Adjusted_Quantity)
Transactions$log_Sticker_price <- log(Transactions$Sticker_price)

# GGPlot for Sticker Price Vs Adjusted quantity
ggplot(Transactions, aes(x = log_Adjusted_Quantity, y = log_Sticker_price)) +
  geom_point(alpha=0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Price Elasticity of Adjusted Demand",
       x = "Adjusted Quantity", y = "Price")

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Appendix Codes

lm_Sales_multi <- lm(Sales~Country + Household_Income, data=Transactions)
summary(lm_Sales_multi)

lm_Sales_multi <- lm(Sales~Household_Income + Sentiment, data=Transactions)
summary(lm_Sales_multi)

lm_Sales_multi <- lm(Sales~Criticality + Refresh_Cycle + Product_type, data=Transactions)
summary(lm_Sales_multi)

lm_Sales_multi <- lm(Sales~Sector+ Household_Income, data=Transactions)
summary(lm_Sales_multi)

lm_Sales_Product_type <- lm(Sales~Product_type, data=Transactions)
summary(lm_Sales_Product_type)

lm_Sales_Sector <- lm(Sales~Sector, data=Transactions)
summary(lm_Sales_Sector)

lm_Sales_Sector <- lm(Sales~factor(Sector, exclude = "Retail"), data=Transactions)
summary(lm_Sales_Sector)

lm_Sales_Country <- lm(Sales~Country, data=Transactions)
summary(lm_Sales_Country)

lm_Sales_multi <- lm(Sales~Sector + Product_type + Promotion_Discount, data=Transactions)
summary(lm_Sales_multi)

lm_Sales_multi <- lm(Sales~Month + Product_type + Promotion, data=Transactions)
summary(lm_Sales_multi)

lm_Sales_multi <- lm(Sales~Month + Product_type + Promotion - 1, data=Transactions)
summary(lm_Sales_multi)

lm_Sales_Product_type <- lm(Sales~Product_type -1, data=Transactions)
summary(lm_Sales_Product_type)

lm_Sales_Sector <- lm(Sales~Sector -1, data=Transactions)
summary(lm_Sales_Sector)

lm_Sales_Month <- lm(Sales~Month - 1, data=Transactions)
summary(lm_Sales_Month)

lm_Sales_Warranty_Sales <- lm(Sales~Warranty_Sales, data=Transactions)
summary(lm_Sales_Warranty_Sales)

lm_Sticker_price_Quantity <- lm(Sticker_price~Quantity + Product_type, data=Transactions)
summary(lm_Sticker_price_Quantity)

lm_warranty_sales <- lm(Warranty_Sales~BYO_Workstation, data=Transactions)
summary(lm_warranty_sales)

lm_sales <- lm(Sales~BYO_Workstation + BYO_Base, data=Transactions)
summary(lm_sales)

lm_Sales_Sector_Household <- lm(Sales~factor(Sector, exclude = "Retail") + Household_Income, data=Transactions)
summary(lm_Sales_Sector_Household)

model_revenue <- lm(Sales ~ `Sticker_price` + Sales_Discount + Product_type + `Sector` +
                      `Promotion` + `Warranty_discount` + `MSP_Usage` +
                      `Refresh_Cycle` + `Criticality` +
                      `Household_Income` + `Sentiment` ,
                    data = Transactions)
summary(model_revenue)

lm_multi <- lm(formula = log(Sales) ~ factor(Country, exclude = c("Italy", "Nigeria")) + Month, data = Transactions)
summary(lm_multi)

lm_multi <- lm(Sales ~ Month, data = Transactions)
summary(lm_multi)





