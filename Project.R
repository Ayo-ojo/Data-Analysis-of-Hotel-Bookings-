bookings <- read.csv("C:\\Users\\13373\\Desktop\\Kaggle Data sets\\Exxpedia Dataset\\Dataset\\Dataset (Data Science- Analytics)-raw_project.csv")

# Define mapping for Booking Window Group
booking_window_mapping <- c(
  "0-1 days" = 1,
  "2-3 days" = 2.5,
  "4-7 days" = 5.5,
  "8-14 days" = 11,
  "15-30 days" = 22.5,
  "31-45 days" = 38,
  "46-60 days" = 53,
  "61-90 days" = 75.5,
  "+90 days" = 90,
  "0 days" = 0.5  # Assign 0 for postbook
)

# Apply the mapping
bookings$Booking.Window.Group <- as.numeric(booking_window_mapping[bookings$Booking.Window.Group])



is.data.frame(bookings)
head(bookings)
str(bookings)
bookings$Net.Orders <- as.numeric(bookings$Net.Orders)
bookings$Net.Gross.Booking.Value.USD <- as.numeric(bookings$Net.Gross.Booking.Value.USD)
bookings$Booking.Window.Group <- as.numeric(bookings$Booking.Window.Group)
str(bookings)
any(is.na(bookings))
head(bookings)
platform_region_aggregate <- aggregate(Net.Gross.Booking.Value.USD ~ Platform.Type.Name + Super.Region, 
                                       data = bookings, 
                                       FUN = sum, 
                                       

# View the result
print(platform_region_aggregate)
# Install and load ggplot2 
install.packages("ggplot2")
library(ggplot2)
platform_region_aggregate <- aggregate(Net.Gross.Booking.Value.USD ~ Platform.Type.Name + Super.Region, 
                                       data = bookings, 
                                       FUN = sum)
# Create a bar plot
ggplot(data = platform_region_aggregate, 
       aes(x = Super.Region, y = Net.Gross.Booking.Value.USD, fill = Platform.Type.Name)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Net Gross Booking Value USD Across Super Regions by Platform",
       x = "Super Region",
       y = "Net Gross Booking Value (USD)") +
  theme_minimal()
# Add a Year column by extracting the first 4 characters of the Week column
bookings$Year <- substr(bookings$Week, 1, 4)
# Aggregate by Year, Platform Type Name, and Super Region
platform_region_year_aggregate <- aggregate(Net.Gross.Booking.Value.USD ~ Year + Platform.Type.Name + Super.Region, 
                                            data = bookings, 
                                            FUN = sum)

# View the aggregated data
head(platform_region_year_aggregate)
library(ggplot2)

# Create a bar plot to compare 2016 and 2017
ggplot(data = platform_region_year_aggregate, 
       aes(x = Super.Region, y = Net.Gross.Booking.Value.USD, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Platform.Type.Name) +  # Create separate plots for each Platform Type
  labs(title = "Net Gross Booking Value USD Comparison (2016 vs 2017)",
       x = "Super Region",
       y = "Net Gross Booking Value (USD)") +
  theme_minimal() 

# Basic density plot
ggplot(data = bookings, aes(x = Net.Gross.Booking.Value.USD, fill = Platform.Type.Name)) +
  geom_density(alpha = 200) +
  labs(title = "Density Plot of Net Gross Booking Value USD by Platform",
       x = "Net Gross Booking Value (USD)",
       y = "Density") +
  theme_minimal()

# Adjust plot aspect ratio
ggplot(data = bookings, aes(x = Net.Gross.Booking.Value.USD, fill = Platform.Type.Name)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Net Gross Booking Value USD by Platform",
       x = "Net Gross Booking Value (USD)",
       y = "Density") +
  xlim(-1000, 10000) +  # Adjust x-axis range
  theme_minimal()

ggplot(data = bookings, aes(x = Net.Gross.Booking.Value.USD, fill = Platform.Type.Name)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Net Gross Booking Value USD by Platform (Log Scale)",
       x = "Net Gross Booking Value (USD)",
       y = "Density") +
  scale_x_log10() +  # Logarithmic scale for x-axis
  theme_minimal()

# Density plot grouped by Super Region
ggplot(data = bookings, aes(x = Net.Gross.Booking.Value.USD, fill = Super.Region)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Net Gross Booking Value USD by Super Region",
       x = "Net Gross Booking Value (USD)",
       y = "Density") +
  theme_minimal()

library(ggplot2)

ggplot(data = bookings, aes(x = Net.Gross.Booking.Value.USD, fill = Super.Region)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Net Gross Booking Value USD by Super Region (Log Scale)",
       x = "Net Gross Booking Value (USD, Log Scale)",
       y = "Density") +
  scale_x_log10() +  # Apply log scale to the x-axis
  theme_minimal()


# Ensure the Year column is already added
bookings$Year <- substr(bookings$Week, 1, 4)

# Density plot grouped by Year with log scale
ggplot(data = bookings, aes(x = Net.Gross.Booking.Value.USD, fill = Year)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Net Gross Booking Value USD by Year (Log Scale)",
       x = "Net Gross Booking Value (USD, Log Scale)",
       y = "Density") +
  scale_x_log10() +  # Apply log scale to the x-axis
  theme_minimal()

library(ggplot2)

library(ggplot2)

ggplot(data = bookings, aes(x = Platform.Type.Name, y = Net.Gross.Booking.Value.USD, fill = Platform.Type.Name)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, alpha = 0.7) +
  labs(title = "Box Plot of Net Gross Booking Value USD by Platform",
       x = "Platform Type",
       y = "Net Gross Booking Value (USD)") +
  scale_y_log10() +  # Apply log scale for better visualization
  theme_minimal()
ggplot(data = bookings, aes(x = Super.Region, y = Net.Gross.Booking.Value.USD, fill = Super.Region)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, alpha = 0.7) +
  labs(title = "Box Plot of Net Gross Booking Value USD by Super Region",
       x = "Super Region",
       y = "Net Gross Booking Value (USD)") +
  scale_y_log10() +  # Use log scale for wide value ranges
  theme_minimal()



ggplot(data = bookings, aes(x = Year, y = Net.Gross.Booking.Value.USD, fill = Year)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, alpha = 0.7) +
  labs(title = "Box Plot of Net Gross Booking Value USD by Year",
       x = "Year",
       y = "Net Gross Booking Value (USD)") +
  scale_y_log10() +
  theme_minimal()

install.packages("GGally")

library(GGally)

ggpairs(data=bookings, columns=c("Net.Gross.Booking.Value.USD", "Super.Region","Net.Orders")) 
`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

ggpairs(data = bookings, 
        columns = c("Booking.Window.Group", "Super.Region", "Platform.Type.Name"),
        diag = list(continuous = wrap("barDiag", binwidth = 10000)))  # Adjust binwidth as needed

library(GGally)

library(GGally)

# Create a ggpairs plot for a mix of numerical and categorical variables
ggpairs(data = bookings, 
        columns = c("Net.Gross.Booking.Value.USD", "Super.Region", "Net.Orders"),  # Mix of numerical and categorical
        diag = list(continuous = wrap("densityDiag"),   # Density plots for numerical variables
                    discrete = wrap("barDiag")),        # Bar plots for categorical variables
        lower = list(combo = "box_no_facet",            # Box plots for numerical-categorical relationships
                     continuous = "smooth"),            # Smoothed scatter plots for numerical-numerical
        upper = list(combo = "facetdensity",            # Faceted density plots for categorical-numerical relationships
                     continuous = "cor"))               # Correlation for numerical-numerical

install.packages("dplyr")  # Install dplyr
library(dplyr)             # Load dplyr

library(countrycode)
revenue_by_country = aggregate(bookings$Net.Gross.Booking.Value.USD, by = list(bookings$Property.Country), FUN = mean) 
revenue_by_country = revenue_by_country%>% rename("country" = "Group.1", "Avg_Revenue" = "x") 
View(revenue_by_country) 
salary_by_country$region = countrycode(revenue_by_country$country,  "country.name")






