##########
################ DATA CLEANING FOR ROLLER COASTER DATA
################ FOR LAB 2 OF STATS 7, SUMMER 2020
################ MARY RYAN
################ CREATED: 08.09.2020
################ UPDATED: 08.09.2020
##########

rollercoaster <- read.csv("./lab-regression/datasets_1307_3124_rollercoasters.csv", header=T)

rollercoaster.edited <- rollercoaster %>% 
   dplyr::select(-custom_design) %>% 
   mutate(inversions = ifelse(inversions == -1, NA, inversions))

write.csv(rollercoaster.edited, "rollercoaster_edited.csv")
