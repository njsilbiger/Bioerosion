### Script to read in DICOMS ####
### By Nyssa Silbiger ####
### Created on 2023-08-22 ####



# Read in libraries
library(here)
library(tidyverse)
library(oro.dicom)


## Load image path #####


img1<-readDICOM("Data/Bioerosion_Block57R_2019_Dicom/")
img2<-readDICOM("Data/Bioerosion_Block57R_2023_Dicom/")


# create a function to turn each matrix in the list into a dataframe

mat2df<-function(x){
  as.data.frame(x) %>% 
    mutate(row = rownames(.)) %>% 
    tidyr::pivot_longer(-row, names_to = "col") %>%
    mutate(row = as.numeric(row), col = readr::parse_number(col))
}


# Make it into a huge dataframe named bu slice number
img1_df<-img1$img %>%
  set_names(paste0("slice_",0:(length(img1$img)-1))) %>% # set the names to be slice names
  map(mat2df) 

rm(img1)

# add the function to turn each slice of the DICOM into a dataframe with row, column and value for easier analysis and plotting
img1_df<- map2_df(img1_df,names(img1_df), ~ mutate(.x, ID = .y)) ## turn it into a big dataframe
  

img1_df %>%
  filter(ID == "slice_ 12" | ID == "slice_ 18") %>%
  ggplot(aes(x = col, y = row, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "white", mid = "darkorange", high = "black",
    #limits = c(0, 3), midpoint = 1.5, oob = scales::squish
  ) +
  theme_void() +
  theme(legend.position = "none")+
  facet_wrap(~ID)


  #lmap(mat2df) # this is causing a problem with the names... stopped here

a<-img1$img$`Data/Bioerosion_Block57R_2019_Dicom/Bioerosion_Block57R_2019_Dicom_220.dcm`
a23<-img2$img$`Data/Bioerosion_Block57R_2023_Dicom/Bioerosion_Block57R_2023_Dicom_220.dcm`

data_for_ggplot <- as.data.frame(a) %>% 
  mutate(row = rownames(.)) %>% 
  tidyr::pivot_longer(-row, names_to = "col") %>%
  mutate(row = as.numeric(row), col = readr::parse_number(col))

data_for_ggplot23 <- as.data.frame(a23) %>% 
  mutate(row = rownames(.)) %>% 
  tidyr::pivot_longer(-row, names_to = "col") %>%
  mutate(row = as.numeric(row), col = readr::parse_number(col))

p1 <- ggplot(data_for_ggplot, aes(x = col, y = row, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "white", mid = "darkorange", high = "black",
    #limits = c(0, 3), midpoint = 1.5, oob = scales::squish
  ) +
  labs(title = "2019") +
  theme_void() +
  theme(legend.position = "none")

p2 <- ggplot(data_for_ggplot23, aes(x = col, y = row, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "white", mid = "darkorange", high = "black",
    #limits = c(0, 3), midpoint = 1.5, oob = scales::squish
  ) +
  labs(title = "2023") +
  theme_void() +
  theme(legend.position = "none")

p1+p2
