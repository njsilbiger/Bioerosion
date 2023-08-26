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

# img 1
img1_df<-img1$img %>%
  set_names(paste0("slice_",0:(length(img1$img)-1))) %>% # set the names to be slice names
  map(mat2df) 

# remove the original one to save space
rm(img1)

#image 2
img2_df<-img2$img %>%
  set_names(paste0("slice_",0:(length(img2$img)-1))) %>% # set the names to be slice names
  map(mat2df) 

# remove the original one to save space
rm(img2)

# add the function to turn each slice of the DICOM into a dataframe with row, column and value for easier analysis and plotting
img1_df<- map2_df(img1_df,names(img1_df), ~ mutate(.x, ID = .y)) %>% ## turn it into a big dataframe
  rename(value2019 = value)
 
img2_df<- map2_df(img2_df,names(img2_df), ~ mutate(.x, ID = .y)) %>% ## turn it into a big dataframe
  rename(value2023 = value)

img_diff <-left_join(img1_df, img2_df)%>%
  mutate(diff = value2023-value2019, # calculate the difference between the two years
         value2019_norm = 0.0009*value2019-5.7891,# normalize to the fantom to calcualte density
         value2023_norm = 0.0013*value2023-16.618, # 60,67,57,63,79,70,68,73,89,66
         #     value2023_norm = 0.0013*value2023-17.452, # everything else
         value2019_norm = ifelse(value2019_norm < 0,0, value2019_norm),# make the negatives 0
         value2023_norm = ifelse(value2023_norm < 0,0, value2023_norm),
         value2019_binary = ifelse(value2019_norm>0,1,0), # if anything is there give it a 1
         value2023_binary = ifelse(value2023_norm>0,1,0), # if anything is there give it a 1
         diff_density = value2023_norm-value2019_norm, # subtract the normalized data,
         diff_binary = value2023_binary - value2019_binary, # subtract the binary data
         binary_norm = case_when(diff_density > 10 ~ 1, # give a 1 if accretion
                            diff_density < -10 ~ -1, # a -1 if erosion
                            .default = 0) # 0 of no change, but based on a threshold of 100
         )
 
img_diff <- img_diff%>%
  mutate(value2019_binary = ifelse(value2019_norm>0,1,0), # if anything is there give it a 1
         value2023_binary = ifelse(value2023_norm>0,1,0), # if anything is there give it a 1
         binary_norm = case_when(diff_density > 10 ~ 1, # give a 1 if accretion
                                 diff_density < -8 ~ -1, # a -1 if erosion
                                 .default = 0), # 0 of no change, but based on a threshold of 100
         # subtract the normalized data,
         diff_binary = value2023_binary - value2019_binary # subtract the binary data
  )  # 0 of no change, but based on a threshold of 100
  


img_diff %>%
  ggplot()+
  geom_histogram(aes(x = value2019_norm), alpha = 0.5, color = "red")+
  geom_histogram(aes(x = value2023_norm), alpha = 0.5, color = "blue")


p1<-img_diff %>%
  filter(ID == "slice_120")%>%
  ggplot(aes(x = col, y = row, fill = diff_binary)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "black", mid = "white", high = "red",
    limits = c(-1, 1), midpoint = 0, oob = scales::squish
  ) +
  theme_void() 



p1<-img_diff %>%
  filter(ID == "slice_320")%>%
  ggplot(aes(x = col, y = row, fill = diff_density)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "white", mid = "darkorange", high = "black",
    #limits = c(0, 3), midpoint = 1.5, oob = scales::squish
  ) +
  theme_void() 
  theme(legend.position = "none")


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

a<-img1$img$`Data/Bioerosion_Block57R_2019_Dicom/Bioerosion_Block57R_2019_Dicom_0120.dcm`
a23<-img2$img$`Data/Bioerosion_Block57R_2023_Dicom/Bioerosion_Block57R_2023_Dicom_0120.dcm`

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
