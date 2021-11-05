library(ggmap)
library(ggrepel)
library(tidyr)
library(dplyr)
library(plotly)
library(plyr)
library(ggplot2)
library(ggthemes)
library(gridExtra)

#if(!requireNamespace("devtools")) install.packages("devtools")
#devtools::install_github("dkahle/ggmap", ref = "tidyup", force=TRUE)

#################################################################################
#SEPTA STATIONS PLOTS (plotting will only work if you have a valid Google API key)
#################################################################################

# GOOGLE API KEY
#register_google(key = "XXXXXXXXXXXXXXXXXXXXXXXXXX")

# Colors

color <- c("#ff7f00", "#1f78b4", "#984ea3", "#1b9e77")

#Load data
accessible <- read.csv("septa_accessible.csv")
end_station <- read.csv("end_station.csv")

# Poster plot (non-interactive)

phi_accessible <- c(left = -75.542032,
            bottom = 39.846199,
            right = -75.063441,
            top = 40.168601)

phi_accessible_stamen <- get_stamenmap(bbox = phi_accessible,
                            zoom = 12,
                            maptype = 'toner-background')

fig1 <- ggmap(phi_accessible_stamen, extent = "device") +
  geom_point(data = accessible,
             mapping = aes(x = longitude,
                           y = latitude,
                           color = service_line,
                           shape = wheelchair_accessible),
             size = 1.8) +
  scale_shape_manual(values = c(1, 19),
                     name = "Accessible?") +
  scale_color_manual(values = color,
                     name = "Line",
                     labels = c("BSL", "MFL", "Norristown HSL", "Regional Rail")) +
  geom_label_repel(data = end_station, aes(x = longitude, 
                                          y = latitude,
                                          label = name), 
                  size = 2.5, 
                  min.segment.length = 0) +
  labs(title = "Figure 1: Wheelchair accessible SEPTA stations",
       subtitle = "Greater Philadelphia area") +
  theme(legend.key = element_rect(fill = NA),
        legend.position = c(0.15, 0.25),
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))
# Save
width <- 10 
ggsave("septa_accessible", fig1,
       device = "png",
       width=width, 
       height=width/1.618)

# Interactive plot for github

phi_interactive <- c(left = -75.798411,
                    bottom = 39.705074,
                    right = -74.690552,
                    top = 40.361196)

phi_interactive_stamen <- get_stamenmap(bbox = phi_interactive,
                                       zoom = 12,
                                       maptype = 'toner-background')
p <- ggmap(phi_interactive_stamen, extent = "device") +
  geom_point(data = accessible,
             mapping = aes(x = longitude,
                           y = latitude,
                           color = service_line,
                           shape = wheelchair_accessible,
                           text = paste("Name: ", name, "\n",
                                        "Line: ", service_lines_detailed, "\n",
                                        "Wheelchair accessible: ", wheelchair_accessible, "\n",
                                        sep = "")),
             size = 2) +
  scale_shape_manual(values = c(1, 19),
                     name = "Accessible?") +
  scale_color_manual(values = color,
                     name = "Line",
                     labels = c("BSL", "MFL", "Norristown HSL", "Regional Rail")) +
  labs(title = "Wheelchair accessible SEPTA stations\n Greater Philadelphia area") +
  theme(legend.position = "none")

ggplotly(p, tooltip = "text")

#####################################
# ELEVATOR OUTAGES PLOTS
####################################
#Load data
outages <- read.csv("outages.csv")

#Transform to one station per row, with total outage day column
#Summarize by station and line name

outages2 <- outages %>% group_by(stopName, Line, outageYear) %>%
  dplyr::summarize(num_outage = n())

outages3 <- outages %>% group_by(stopName, Line) %>%
  dplyr::summarize(num_outage = n())

#Plot outages2

plot1 <- ggplot(data = outages2, aes(num_outage,
                            reorder(stopName, num_outage, sum), 
                            fill = Line)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values = color) +
  facet_wrap(~outageYear) +
  labs(title = "b) Elevator outages at SEPTA stations",
       subtitle = "Greater Phildephia area, 2017-2019",
       caption = "   ",
       x = "Number of days",
       y = "Station") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.caption = element_text(hjust = 0),
        panel.grid.major.y = element_blank(),
        axis.title.y=element_blank())
  
#Save outages2
width <- 10 
ggsave("outages_by_station",
       device = "png",
       width=width, 
       height=width/1.618)

# Plot outages3
plot2 <- ggplot(data = outages3, aes(num_outage,
                            reorder(stopName, num_outage), 
                            fill = Line)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values = color) +
  scale_x_continuous(breaks = seq(0, 140, 20)) +
  geom_text(aes(label = num_outage), hjust = -0.3, size = 2.5) +
  labs(title = "Figure 3: Elevator outages at SEPTA stations",
       subtitle = "Greater Phildephia area, 24-month total (March 2017-March 2019)",
       caption = "(Stations sorted by total days of outage in the 2-year period)",
       x = "Number of days",
       y = "Station") +
  theme_minimal() +
  theme(legend.position = c(0.85, 0.15),
        plot.caption = element_text(hjust = 0),
        panel.grid = element_blank())

#Save outages3 plots
width <- 9 
ggsave("outages_by_station2", plot2,
       device = "png",
       width=width, 
       height=width/1.618)

plot <- grid.arrange(plot2, plot1, nrow = 1)
width <- 12 
ggsave("outages_by_station3", plot,
       device = "png",
       width=width, 
       height=width/1.618)

###################

#Formatting data

accessible2 <- accessible %>% 
  select(name, service_line, service_lines_detailed, wheelchair_accessible) %>%
  group_by(service_line, service_lines_detailed, wheelchair_accessible) %>%
  dplyr::summarize(n_accessible = n()) %>%
  pivot_wider(names_from = wheelchair_accessible, values_from = n_accessible) %>%
  replace_na(list(`TRUE` = 0, `FALSE` = 0)) %>%
  mutate(n_station = `TRUE` + `FALSE`,
         raw_accessible = `TRUE`/n_station,
         pc_accessible = round(`TRUE`/n_station * 100, 1))

#Plot
ggplot(accessible2, 
       aes(x=raw_accessible, y = reorder(service_lines_detailed, raw_accessible)))+
  geom_col(data = accessible2, aes(fill = service_line)) +
  scale_fill_manual(values = color,
                    labels = c("BSL", "MFL", "NHSL", "Regional Rail")) +
  scale_x_continuous(labels = scales::percent) +
  geom_text(aes(label = pc_accessible), hjust = -0.3, size = 2.5) +
  labs(title = "Figure 2: Accessible SEPTA stations by service line",
       subtitle = "(as of 2019)",
       x = "Accessible stations (percentage)",
       y = "Service Line",
       fill = "Line") +
  theme_tufte() +
  theme(legend.position = "bottom",
        axis.ticks.y = element_blank())


width <- 7 
ggsave("accessible2",
       device = "png",
       width=width, 
       height=width/1.618)
