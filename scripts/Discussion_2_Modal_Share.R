# Description: This script generates the graphs for anlyzing 
# reported mode of transportantion based on and the absence / presence matrix
#
# Comments: set your working directory to 
# Author: Diego Pajarito 


library(ggplot2)

# Setup 
source("scripts/setup.R")
mode_values <- c(1,2,3,4)
mode_labels <- c("Bicycle", "Walking", "Pub. transport", "Private Car")
mode_factors <- factor(mode_values)

# Participants reporting mode of transportation 

transport_city <- data.frame(table_answers$participant, table_answers$City, table_answers$dem_transport_bicycle, table_answers$dem_transport_walk, 
                       table_answers$dem_transport_public, table_answers$dem_transport_car)

names(transport_city) <- c("participant", "city", "bicycle", "walking", "public_transport", "private_car")

#data preparation
# Participants position
transport_city$participant[transport_city$city == "Castelló"] <- transport_city$participant[transport_city$city == "Castelló"] - 20
transport_city$participant[transport_city$city == "Malta"] <- transport_city$participant[transport_city$city == "Malta"] - 40
# Bicicle = 1
transport_city$bicycle[is.na(transport_city$bicycle)] <- 0
# Walking = 2
transport_city$walking[is.na(transport_city$walking)] <- 0
transport_city$walking[transport_city$walking != 0] <- 2
# Public Transport = 3
transport_city$public_transport[is.na(transport_city$public_transport)] <- 0
transport_city$public_transport[transport_city$public_transport != 0] <- 3
# car = 4
transport_city$private_car[is.na(transport_city$private_car)] <- 0
transport_city$private_car[transport_city$private_car != 0] <- 4



transport_grid_bicycle <- data.frame ( transport_city$participant, transport_city$bicycle, transport_city$bicycle, transport_city$city )
names(transport_grid_bicycle) <- c("X", "Y", "Z", "city")
transport_grid_walking <- data.frame ( transport_city$participant, transport_city$walking, transport_city$walking, transport_city$city )
names(transport_grid_walking) <- c("X", "Y", "Z", "city")
transport_grid_public <- data.frame ( transport_city$participant, transport_city$public_transport, transport_city$public_transport, transport_city$city )
names(transport_grid_public) <- c("X", "Y", "Z", "city")
transport_grid_private <- data.frame ( transport_city$participant, transport_city$private_car, transport_city$private_car, transport_city$city )
names(transport_grid_private) <- c("X", "Y", "Z", "city")
transport_grid <- rbind(transport_grid_bicycle, transport_grid_walking, transport_grid_public, transport_grid_private )
transport_grid <- transport_grid[transport_grid$Z>0, ]
transport_grid$Y <- factor(transport_grid$Y, mode_factors, labels = mode_labels )

ggplot(transport_grid, aes(X, Y, z= Z)) + 
  geom_tile(aes(fill = Z, width=0.7, height=0.7)) + 
  scale_fill_gradientn(colours = c( "#33691e", "orange", "gray"), values = c(0,0.8,1)) +
  theme_bw() + 
  scale_x_continuous(breaks = seq(1:20)) +
  facet_grid(city ~ .) + xlab("Participant") + ylab("") +
  theme(legend.position="none")
  


svg(filename="graphs/A_transport_tode_graph1.svg", 
    width=6, height=4, pointsize=10)
ggplot(transport_grid, aes(X, Y, z= Z)) + 
  geom_tile(aes(fill = Z, width=0.7, height=0.7)) + 
  scale_fill_gradientn(colours = c( "#33691e", "orange", "gray"), values = c(0,0.8,1)) +
  theme_bw() + 
  scale_x_continuous(breaks = seq(1:20)) +
  facet_grid(city ~ .) + xlab("Participant") + ylab("") +
  theme(legend.position="none")
dev.off()



