# LECTURE 10:Shiny 
# Wednesday, January 26, 2022

# ----------------------------------- CODE -----------------------------------


# PACKAGES 

#install.packages("")
library("tidyverse")
library("dplyr")
library("tidytext")
library("tidyr")
library("rvest")
library("rnaturalearth")
library("shiny")


setwd("~/Desktop/NSCI Data Science/Lecture/Lecture10")


# ----------------------------- NEW TOOLS ----------------------------------

world <- ne_countries(returnclass = "sf")                      

