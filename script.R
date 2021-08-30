
################################################################################
# Load libraries
################################################################################

library(tidyverse)
library(sf)
library(cmapgeo)
library(cmapplot)

################################################################################
# Load MDT data from separate repo
################################################################################

source("../mydailytravel/R/data_cleaning.R")
source("../mydailytravel/R/helper_fns.R")
setwd("../task3_mr")

################################################################################
# Prep data
################################################################################

# Identify which trips end in the central area (zones 1-77)
central_area <- 
  cmapgeo::zone_sf %>% 
  filter(zone17 <= 77)

tracts_in_central_area <-
  cmapgeo::tract_sf[lengths(st_intersects(tract_sf,central_area))!=0,] %>% 
  select(geoid_tract) %>% 
  # Manually remove three that are only bordering the central area
  filter(geoid_tract != "17031310800",
         geoid_tract != "17031071500",
         geoid_tract != "17031841000") %>% 
  
  mutate(state_fips = substr(geoid_tract,1,2),
         county_fips = substr(geoid_tract,3,5),
         tract_fips = substr(geoid_tract,6,11)) %>% 
  mutate(tract_fips_n = as.numeric(tract_fips))

st_geometry(tracts_in_central_area) <- NULL

# Create helper value to allow for collapsing of distinct work trip chains
chain_id <-
  # Load chain data from MDT
  chains %>% 
  # Arrange to allow for identification of separate work chains
  arrange(sampno,perno,placeno) %>% 
  # Group by sample and person to identify individuals
  group_by(sampno,perno) %>% 
  # Identify distinct sets of trips, varying by whether they are home_to_work or
  # not. Note that this will not vary between other categories of work chain
  # trips, since we are only trying to capture journeys from home to work, and
  # not work to work or work to home.
  mutate(chain_id = cumsum(c(1,diff(home_to_work) != 0))) %>% 
  ungroup() 

# Add back chain information
mdt_chains <- 
  mdt %>% 
  left_join(chain_id %>% select(sampno,perno,placeno,
                                home_to_work,work_to_work,work_to_home,
                                chain_id),
            by = c("sampno","perno","placeno"))

# Create base data
mode_share_base <-
  mdt_chains %>%                              # 125463 records
  # Exclude "beginning" trips
  filter(mode_c != "beginning") %>%   # 97374
  # Exclude trips with no travel distance.
  filter(distance_pg > 0) %>%         # 97316
  # Exclude trips with a "missing" mode
  filter(mode_c != "missing") %>%        # 97279
  # Add central area flag to 'county_chi_name'
  mutate(destination_place = case_when(
    # Flag all tracts in the central area
    state_fips == 17 & 
      county_fips == 31 &
      tract_fips %in% tracts_in_central_area$tract_fips_n ~ "Central area",
    # For Chicago locations that are outside the CBD, mark them appropriately
    county_chi_name == "Chicago" ~ "Chicago (excl. CBD)",
    # All other locations will be marked with the original county location,
    # which included a "Suburban Cook" designation.
    TRUE ~ county_chi_name 
  )) %>% 
  # Exclude NA destinations
  filter(!is.na(destination_place)) %>%   # 96907
  # Remove destinations in Grundy and DeKalb
  filter(!(destination_place %in% c("Grundy","DeKalb"))) %>% # 95169
  # Add factor ordering
  mutate(destination_place = factor(destination_place)) %>% 
  # Keep relevant variables
  select(sampno,perno,weight,placeno,
         mode,mode_c,tpurp,tpurp_c,travtime_pg_calc,distance_pg,
         home_to_work,chain_id,chain_c,
         home_county_chi,
         destination_place,county_chi_name,county_chi_name_lag)
  

################################################################################
# Analyze destination mode share
################################################################################

destination_alltrips_mode_share <-
  pct_calculator(mode_share_base,
                 breakdown_by = "mode_c",
                 second_breakdown = "destination_place",
                 weight = "weight") 

destination_worktrips_mode_share <-
  pct_calculator(mode_share_base %>%
                   # Keep only trips to work
                   filter(home_to_work == 1) %>% 
                   # Keep the longest leg of a chain, as that is likeliest to be
                   # the relevant travel component
                   group_by(sampno,perno,chain_id) %>% 
                   arrange(desc(distance_pg)) %>% 
                   distinct(sampno,perno,chain_id,.keep_all = TRUE) %>% 
                   ungroup(),
                 breakdown_by = "mode_c",
                 second_breakdown = "destination_place",
                 weight = "weight")

################################################################################
# Destination mode share exports
################################################################################

# Export all trips mode share

t1 <- 
  destination_alltrips_mode_share %>% 
  select(mode = mode_c,
         destination = destination_place,
         trips = total,
         pct = pct,
         n = total_n) %>% 
  mutate(pct = round(pct, 4)) %>% 
  pivot_wider(names_from = mode,values_from = pct)

write.csv(t1,"destination_alltrips_mode_share.csv")

# Export work trips mode share

t2 <-
  destination_worktrips_mode_share %>% 
  select(mode = mode_c,
         destination = destination_place,
         trips = total,
         pct = pct,
         n = total_n) %>% 
  mutate(pct = round(pct, 4)) %>% 
  pivot_wider(names_from = mode,values_from = pct)

write.csv(t2,"destination_worktrips_mode_share.csv")
  
# Create labels for all trips mode share chart
p1_labels <-
  destination_alltrips_mode_share %>%
  filter(mode_c %in% c("walk","transit","bike","schoolbus","other")) %>%
  group_by(destination_place) %>%
  summarize(label = sum(pct))

# Create plot
p1 <-
  # Get data
  destination_alltrips_mode_share %>%
  # Add labels
  left_join(p1_labels, 
            by = c("destination_place")) %>%
  # Make changes for graphing
  mutate(
    # Reorder factors and capitalize
    mode_c = recode_factor(factor(mode_c,levels = 
                                    c("driver","passenger","walk",
                                      "transit","bike","schoolbus","other")),
                           "passenger" = "Passenger",
                           "driver" = "Driver",
                           "walk" = "Walk",
                           "transit" = "Transit",
                           "bike" = "Bike",
                           "schoolbus" = "School bus",
                           "other" = "Other"),
    # Make driver/passenger go on the left-hand-side of the graph
    pct = ifelse(mode_c %in% c("Driver","Passenger"),-1 *pct,pct)) %>% 
  
  # Create ggplot object
  ggplot(aes(x = pct, y = reorder(destination_place,label))) +
  geom_col(aes(fill = mode_c),
           position = position_stack(reverse = T)) +
  
  # Add CMAP style
  theme_cmap(gridlines = "v", vline = 0,
             xlab = "Mode share by destination",
             strip.text = element_text(family = "Whitney Semibold",
                                       hjust = 0.5)) +
  # Add colors
  scale_fill_manual(values = c("#e5bd72","#8c0000","#36d8ca","#6d8692","#efa7a7","#3d6600","#0084ac"),
                    labels = c("Driver","Passenger","Walk","Transit","Bike","School bus","Other")) +
  
  # Adjust axis
  scale_x_continuous(breaks = seq(-1,.75,by = .25), 
                     labels = scales::label_percent()(abs(seq(-1,.75,by = .25))),
                     limits = c(-1,.75),
                     expand = expansion(mult = c(.05,0))
  ) +
  
  # Adjust legend for formatting
  guides(fill = guide_legend(ncol = 7,
                             override.aes = list(fill = c("#8c0000","#e5bd72","#36d8ca",
                                                          "#6d8692","#efa7a7","#3d6600",
                                                          "#0084ac")))) 

# Export finalized graphic
finalize_plot(p1,
              title = "Mode share by destination, all trips (2019).",
              caption = "Note: The Central Area includes all trips to 
              tracts within the area bounded by North Ave., Ashland Ave., 
              Cermak Rd., and Lake Michigan.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My 
              Daily Travel data.",
              filename = "p1",
              height = 4,
              mode = c("png","pdf"),
              overwrite = T)


# Create labels
p2_labels <-
  destination_worktrips_mode_share %>%
  filter(mode_c %in% c("walk","transit","bike","schoolbus","other")) %>%
  group_by(destination_place) %>%
  summarize(label = sum(pct))

# Create plot
p2 <-
  # Get data
  destination_worktrips_mode_share %>%
  # Add labels
  left_join(p2_labels, 
            by = c("destination_place")) %>%
  # Make changes for graphing
  mutate(
    # Reorder factors and capitalize
    mode_c = recode_factor(factor(mode_c,levels = 
                                    c("driver","passenger","walk",
                                      "transit","bike","schoolbus","other")),
                           "passenger" = "Passenger",
                           "driver" = "Driver",
                           "walk" = "Walk",
                           "transit" = "Transit",
                           "bike" = "Bike",
                           "schoolbus" = "School bus",
                           "other" = "Other"),
    # Make driver/passenger go on the left-hand-side of the graph
    pct = ifelse(mode_c %in% c("Driver","Passenger"),-1 *pct,pct)) %>% 
  
  # Create ggplot object
  ggplot(aes(x = pct, y = reorder(destination_place,label))) +
  geom_col(aes(fill = mode_c),
           position = position_stack(reverse = T)) +
  
  # Add CMAP style
  theme_cmap(gridlines = "v", vline = 0,
             xlab = "Mode share by destination",
             strip.text = element_text(family = "Whitney Semibold",
                                       hjust = 0.5)) +
  # Add colors
  scale_fill_manual(values = c("#e5bd72","#8c0000","#36d8ca","#6d8692","#efa7a7","#3d6600","#0084ac"),
                    labels = c("Driver","Passenger","Walk","Transit","Bike","School bus","Other")) +
  
  # Adjust axis
  scale_x_continuous(breaks = seq(-1,.75,by = .25), 
                     labels = scales::label_percent()(abs(seq(-1,.75,by = .25))),
                     limits = c(-1,.75),
                     expand = expansion(mult = c(.05,0))
  ) +
  
  # Adjust legend for formatting
  guides(fill = guide_legend(ncol = 7,
                             override.aes = list(fill = c("#8c0000","#e5bd72","#36d8ca",
                                                          "#6d8692","#efa7a7","#3d6600",
                                                          "#0084ac")))) 

# Export finalized graphic
finalize_plot(p2,
              title = "Mode share by destination, trips to work (2019).",
              caption = "Note: The Central Area includes all trips to 
              tracts within the area bounded by North Ave., Ashland Ave., 
              Cermak Rd., and Lake Michigan. Includes only work trips identified
              as part of a journey from home to work. For work chains with 
              multiple legs, only includes the segment with the longest travel 
              distance.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My 
              Daily Travel data.",
              filename = "p2",
              height = 4,
              mode = c("png","pdf"),
              overwrite = T)


################################################################################
# Travel statistics
################################################################################

# Travel statistics by destination
destination_worktrip_travel_stats <-
  mode_share_base %>% 
  # Keep only trips from home to work
  filter(home_to_work == 1) %>%
  # Group by person and trip chain ID
  group_by(sampno,perno,chain_id) %>%
  # Create summaries of entire chains
  arrange(sampno,perno,placeno) %>% 
  summarize(distance_pg = sum(distance_pg),
            travtime_pg_calc = sum(travtime_pg_calc),
            weight = median(weight),
            destination_place = last(destination_place),
            legs = n()) %>%
  ungroup() %>%
  # Create flag for multi-leg journeys
  mutate(multi_leg = ifelse(legs == 1,"Single","Multi")) %>% 
  group_by(destination_place
           # ,multi_leg
           ) %>% 
  summarize(distance = weighted.mean(distance_pg,wt = weight),
            time = weighted.mean(travtime_pg_calc, wt = weight),
            n = n())  


# Travel statistics by destination for non-work trips
destination_nonworktrip_travel_stats <-
  mode_share_base %>% 
  # Exclude all trips connected to a work trip chain. Note this excludes more
  # than just those analyzed above, which were only home to work - this also
  # excludes work to work and work to home to provide a picture of non-work
  # travel.
  filter(chain_c != "work") %>% 
  group_by(destination_place) %>% 
  summarize(distance = weighted.mean(distance_pg,wt = weight),
            time = weighted.mean(travtime_pg_calc, wt = weight),
            n = n()) 


# Travel statistics by destination for all trips
destination_alltrip_travel_stats <-
  mode_share_base %>% 
  group_by(destination_place) %>% 
  summarize(distance = weighted.mean(distance_pg,wt = weight),
            time = weighted.mean(travtime_pg_calc, wt = weight),
            n = n())  


# Export travel stats
write.csv(destination_worktrip_travel_stats,"destination_worktrip_travel_stats.csv")
write.csv(destination_alltrip_travel_stats,"destination_alltrip_travel_stats.csv")
write.csv(destination_nonworktrip_travel_stats,"destination_nonworktrip_travel_stats.csv")

 
# # Chart of travel statistics
# p3 <-
#   destination_travel_stats %>% 
#   mutate(destination_place = factor(destination_place,
#                                     levels = rev(levels(destination_travel_stats$destination_place)))) %>% 
#   ggplot(aes(x = distance, 
#              y = destination_place)) +
#   geom_col(aes(fill = flag),
#            position = position_dodge2(reverse = T)) +
#   theme_cmap(vline = 0, gridlines = "v",
#              xlab = "Distance traveled (miles)") +
#   cmap_fill_discrete("friday")
# 
# finalize_plot(p3,
#          "Distance traveled by destination, journeys to work vs. all trips (2019).",
#          "Note: 'Trips to work' includes all trips recorded as 
#               part of a trip chain between a survey respondent's home location 
#               and their work location. The Central Area includes all trips to 
#               tracts within the area bounded by North Ave., Ashland Ave., 
#               Cermak Rd., and Lake Michigan.
#               <br><br>
#               Source: Chicago Metropolitan Agency for Planning analysis of My 
#               Daily Travel data.",
#          filename = "p3",
#          # height = 4,
#          # mode = c("png","pdf"),
#          overwrite = T)
# 
# p4 <-
#   destination_travel_stats %>% 
#   mutate(destination_place = factor(destination_place,
#                                     levels = rev(levels(destination_travel_stats$destination_place)))) %>% 
#   ggplot(aes(x = time, 
#              y = destination_place)) +
#   geom_col(aes(fill = flag),
#            position = position_dodge2(reverse = T)) +
#   theme_cmap(vline = 0, gridlines = "v",
#              xlab = "Trip duration (minutes)")+
#   cmap_fill_discrete("friday")
# 
# finalize_plot(p4,
#               "Travel time by destination, journeys to work vs. all trips (2019).",
#               "Note: 'Trips to work' includes all trips recorded as 
#               part of a trip chain between a survey respondent's home location 
#               and their work location. The Central Area includes all trips to 
#               tracts within the area bounded by North Ave., Ashland Ave., 
#               Cermak Rd., and Lake Michigan.
#               <br><br>
#               Source: Chicago Metropolitan Agency for Planning analysis of My 
#               Daily Travel data.",
#               filename = "p4",
#               # height = 4,
#               # mode = c("png","pdf"),
#               overwrite = T)

# # Summary of travel into the CBD
# 
# cbd_mode_share <-
#   pct_calculator(mode_share_base %>%
#                    mutate(flag = "All trips") %>%
#                    filter(
#                      destination_place == "Central area",
#                      !(county_chi_name_lag %in% c("Grundy","DeKalb")),
#                      !is.na(county_chi_name_lag)) %>%
#                    rbind(mode_share_base %>%
#                            filter(home_to_work == 1,
#                                   destination_place == "Central area",
#                                   !(county_chi_name_lag %in% c("Grundy","DeKalb")),
#                                   !is.na(county_chi_name_lag)) %>%
#                              mutate(flag = "Journey to work")),
#                  breakdown_by = "mode_c",
#                  second_breakdown = "county_chi_name_lag",
#                  third_breakdown = "flag",
#                  weight = "weight") %>%
#   # mutate(pct = round(pct,2)) %>%
#   arrange(county_chi_name_lag) # %>% View()
# 
# # Create labels
# p5_labels <-
#   cbd_mode_share %>%
#   filter(mode_c %in% c("walk","transit","bike","schoolbus","other")) %>%
#   group_by(county_chi_name_lag,flag) %>%
#   summarize(label = sum(pct))
# 
# # Create plot
# p5 <-
#   # Get data
#   cbd_mode_share %>%
#   # Add labels
#   left_join(p5_labels,
#             by = c("county_chi_name_lag",
#                    "flag")) %>%
#   # Make changes for graphing
#   mutate(
#     # Reorder factors and capitalize
#     mode_c = recode_factor(factor(mode_c,levels =
#                                     c("driver","passenger","walk",
#                                       "transit","bike","schoolbus","other")),
#                            "passenger" = "Passenger",
#                            "driver" = "Driver",
#                            "walk" = "Walk",
#                            "transit" = "Transit",
#                            "bike" = "Bike",
#                            "schoolbus" = "School bus",
#                            "other" = "Other"),
#     # Make driver/passenger go on the left-hand-side of the graph
#     pct = ifelse(mode_c %in% c("Driver","Passenger"),-1 *pct,pct)) %>%
# 
#   # Create ggplot object
#   ggplot(aes(x = pct, y = reorder(county_chi_name_lag,label))) +
#   geom_col(aes(fill = mode_c),
#            position = position_stack(reverse = T)) +
# 
#   # Add CMAP style
#   theme_cmap(gridlines = "v", vline = 0,
#              xlab = "Mode share") +
#   # Add colors
#   scale_fill_manual(values = c("#e5bd72","#8c0000","#36d8ca","#6d8692","#efa7a7","#3d6600","#0084ac"),
#                     labels = c("Driver","Passenger","Walk","Transit","Bike","School bus","Other")) +
# 
#   # Adjust axis
#   scale_x_continuous(breaks = seq(-.75,1,by = .25),
#                      labels = scales::label_percent()(abs(seq(-.75,1,by = .25))),
#                      limits = c(-.75,1),
#                      expand = expansion(mult = c(.05,0))
#   ) +
# 
#   # Adjust legend for formatting
#   guides(fill = guide_legend(ncol = 7,
#                              override.aes = list(fill = c("#8c0000","#e5bd72","#36d8ca",
#                                                           "#6d8692","#efa7a7","#3d6600",
#                                                           "#0084ac")))) +
#   facet_wrap(~flag,ncol = 1)
# 
# # Export finalized graphic
# finalize_plot(p5,
#               title = "Mode share into the CBD by home location, trips to and from work vs. other trips (2019).",
#               caption = "Note: The Central Area includes all trips to tracts
#               within the area bounded by North Ave., Ashland Ave., Cermak Rd.,
#               and Lake Michigan.
#               <br><br>
#               Source: Chicago Metropolitan Agency for Planning analysis of My Daily Travel data.",
#               filename = "p5",
#               # height = 4,
#               # mode = c("png","pdf"),
#               overwrite = T)
