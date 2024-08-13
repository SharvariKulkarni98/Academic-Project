
data.dir <- "C:/Users/Home/OneDrive - Syracuse University/Documents/IST 719 InfoViz/Data/"
horror_movies <- read.csv(paste0(data.dir,"horror_movies.csv"))

head(horror_movies)

horror_movies$release_date <- as.Date(horror_movies$release_date, format='%m/%d/%Y')

horror_movies$release_year <- format(horror_movies$release_date, "%Y")

horror_movies <- horror_movies[!is.na(horror_movies$release_year), ]

library(ggplot2)

ggplot(horror_movies, aes(x = as.numeric(format(release_date, "%Y")))) +
  geom_bar(stat = "count", fill = "red") +
  scale_y_reverse() +
  scale_x_continuous(position = "top") +
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())


# The circular plot for genre VS movie ratings 

plots <- list()

for (genre_group in unique(horror_movies$Genre_group)) {
  subset_data <- subset(horror_movies, Genre_group == genre_group)
  
  if (nrow(subset_data) > 0) {
    clean_genre_group <- gsub("[^[:alnum:]]", "_", genre_group)
    
    p <- ggplot(subset_data, aes(x = 1, y = vote_average, fill = clean_genre_group)) +
      geom_bar(stat = "identity", width = 0.95) +
      coord_polar(theta = "y") +
      ylim(c(0, 10)) +
      xlab(NULL) + ylab(NULL) +
      theme_minimal() +
      theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
            plot.title = element_blank(), panel.grid = element_blank()) + 
      ggtitle(NULL) +
      scale_fill_manual(values = "#8B0000", name = clean_genre_group) 
    plots[[length(plots) + 1]] <- p
  }
}

if (length(plots) > 0) {
  grid.arrange(grobs = plots, ncol = 2)  # You can adjust 'ncol' as needed
} else {
  print("No data to plot.")
}

ggplot(horror_movies, aes(x = budget, y = revenue)) +
  geom_hex(bins = 30) +
  labs(title = "Hexbin Plot: Budget vs Revenue",
       x = "Budget",
       y = "Revenue") +
  theme_minimal() +
  scale_fill_viridis_c()


library(dplyr)

genre_counts <- horror_movies %>%
  group_by(Genre_group) %>%
  summarize(movie_count = n())

horror_movies <- left_join(horror_movies, genre_counts, by = "Genre_group")


ggplot(horror_movies, aes(x = Genre_group, y = revenue, size = movie_count)) +
  geom_point(alpha = 0.7, color = "#8B0000") + 
  scale_size_continuous(range = c(3, 15)) +  
  labs(title = "Bubble Plot: Budget vs Revenue by Genre",
       x = "Genre",
       y = "Revenue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "bottom",
        panel.grid = element_blank()) +  
  scale_y_continuous(labels = scales::comma)

genre_budget <- horror_movies %>%
  group_by(Genre_group) %>%
  summarize(avg_budget = mean(budget, na.rm = TRUE))


horror_movies <- left_join(horror_movies, genre_budget, by = "Genre_group")


ggplot(horror_movies, aes(x = Genre_group, y = budget, size = avg_budget)) +
  geom_point(alpha = 0.7, color = "#8B0000") +  
  scale_size_continuous(range = c(3, 15)) + 
  labs(title = "Bubble Plot: Budget vs Genre",
       x = "Genre",
       y = "Budget") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        legend.position = "bottom",
        panel.grid = element_blank()) +  
  scale_y_continuous(labels = scales::comma)

genre_revenue <- horror_movies %>%
  group_by(Genre_group) %>%
  summarize(avg_revenue = mean(revenue, na.rm = TRUE))

horror_movies <- left_join(horror_movies, genre_revenue, by = "Genre_group")


ggplot(horror_movies, aes(x = Genre_group, y = revenue, size = avg_revenue)) +
  geom_point(alpha = 0.7, color = "red1") +  
  scale_size_continuous(range = c(3, 15)) + 
  labs(title = "Bubble Plot: Revenue vs Genre",
       x = "Genre",
       y = "Revenue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better visibility
        legend.position = "bottom",
        panel.grid = element_blank()) +  # Remove grid
  scale_y_continuous(labels = scales::comma)





#COmbing both the plots
genre_budget <- horror_movies %>%
  group_by(Genre_group) %>%
  summarize(avg_budget = mean(budget, na.rm = TRUE))

horror_movies <- left_join(horror_movies, genre_budget, by = "Genre_group")

genre_revenue <- horror_movies %>%
  group_by(Genre_group) %>%
  summarize(avg_revenue = mean(revenue, na.rm = TRUE))

horror_movies <- left_join(horror_movies, genre_revenue, by = "Genre_group")

plot_budget <- ggplot(horror_movies, aes(x = Genre_group, y = budget, size = avg_budget)) +
  geom_point(alpha = 0.7, color = "#8B0000") + 
  scale_size_continuous(range = c(3, 15)) +  
  labs(title = "Bubble Plot: Budget vs Genre",
       x = "Genre",
       y = "Budget") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        legend.position = "none",  
        panel.grid = element_blank()) +  
  scale_y_continuous(labels = scales::comma)

plot_revenue <- ggplot(horror_movies, aes(x = Genre_group, y = revenue, size = avg_revenue)) +
  geom_point(alpha = 0.7, color = "red1") +  
  scale_size_continuous(range = c(3, 15)) +  
  labs(title = "Bubble Plot: Revenue vs Genre",
       x = "Genre",
       y = "Revenue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        legend.position = "none", 
        panel.grid = element_blank()) + 
  scale_y_continuous(labels = scales::comma)
library(gridExtra)
grid.arrange(plot_budget, plot_revenue, ncol = 2)
