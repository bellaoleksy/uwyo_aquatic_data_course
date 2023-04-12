######################################################################
#Script to get some summary statistics on the western LimnoSat dataset
######################################################################
library(tidyverse)
library(ggthemes)

limnosatRaw <- read_csv("~/Library/CloudStorage/GoogleDrive-bellaoleksy@gmail.com/My Drive/Collaborations/uwyo_aquatic_data_course/Data/LimnoSat/limnoSat_westernUSA.csv")

limnosatMeta <- read_csv("~/Library/CloudStorage/GoogleDrive-bellaoleksy@gmail.com/My Drive/Collaborations/uwyo_aquatic_data_course/Data/LimnoSat/limnoSat_westernUSA_metadata.csv") %>%
  select(1:2)



head(limnosatRaw)
str(limnosatRaw)

#Let's make sure that the IDs are read as characters, not numbers
limnosatRaw <- limnosatRaw %>%
  mutate(lagoslakeid = as.character(as.numeric(lagoslakeid)),
         nhdplusv2_comid = as.character(as.numeric(nhdplusv2_comid)))

#How many stats?
unique(limnosatRaw$lake_centroidstate)

#How many unique lakes?
length(unique(limnosatRaw$nhdplusv2_comid))
length(unique(limnosatRaw$lagoslakeid))

#How many observations per lake per year?
dwL_count <-  limnosatRaw %>%
  group_by(lagoslakeid, year) %>%
  mutate(n = length(unique(date))) %>%
  group_by(lagoslakeid) %>%
  summarize(median_n=median(n, na.rm=TRUE))

hist(dwL_count$median_n)
#Some lakes are more data rich than others


# How many lakes are generally green or blue?
# We can use a 530nm cut off to categorize the lakes.
# Let me demonstrate why

#color for manually created legend
#based on Forel-Ule index
bg.fui = tibble(
  ymin = c(470,475,480,485,489,495,509,530,549,559,564,567,568,569,570,573,575,577,579,581,583),
  ymax = c(475,480,485,489,495,509,530,549,559,564,567,568,569,570,573,575,577,579,581,583,590),
  color = c(
    "#2158bc", "#316dc5", "#327cbb", "#4b80a0", "#568f96", "#6d9298", "#698c86", 
    "#759e72", "#7ba654", "#7dae38", "#94b660","#94b660", "#a5bc76", "#aab86d", 
    "#adb55f", "#a8a965", "#ae9f5c", "#b3a053", "#af8a44", "#a46905", "#9f4d04")
)


ggplot() + 
  geom_rect(data = bg.fui, 
            aes(xmin = ymin, xmax = ymax, ymin = -5, ymax = 100, fill = color)) + 
  geom_vline(xintercept = 530, linetype = 3) +
  geom_histogram(data = limnosatRaw, aes(x=dWL, y = after_stat(density)), fill = NA,
                 color = 'black', alpha = .3, bins = 40) + 
  geom_density(data = limnosatRaw, aes(x=dWL)) + 
  scale_x_continuous(expand = c(0, 0))+ #get rid of whitespace
  scale_fill_identity() + 
  theme_few() +
  coord_cartesian(ylim = c(0,.04))  +
  labs(x="Dominant wavelength (nm)",
       y="Density")


# For now, let's look at the median color of each
# lake over the last decade

limnosatModern <- limnosatRaw %>%
  filter(year>=2012) %>%
  group_by(lagoslakeid, lake_centroidstate) %>%
  summarize(dWL = median(dWL, na.rm=TRUE)) %>%
  mutate(group = cut(dWL, 
                     breaks = c(0,530,Inf), 
                     labels = c('Blue', #blue/clear
                                'Green'))) #green/brown/murky

#How many lakes in each grouping? 
limnosatModern %>%
  ungroup() %>%
  count(group)

#How many lakes are green or blue in each state?
limnosatModern %>%
  ungroup() %>%
  count(group, lake_centroidstate) %>%
  pivot_wider(names_from = lake_centroidstate, values_from = n) #pivot wider for easier viewing

#How many lakes are green or blue in each state?
#With percentages this time
limnosatModern %>%
  ungroup() %>%
  count(group, lake_centroidstate) %>%
  group_by(lake_centroidstate) %>%
  mutate(sum=sum(n)) %>%
  group_by(group, lake_centroidstate) %>%
  summarize(perc = (n/sum)* 100) %>%
  pivot_wider(names_from = lake_centroidstate, values_from = perc) #pivot wider for easier viewing



