### Get the good stuff, race/gender breakdown for sample ----
  # Load packages, define directory ----
    setwd("C:/Users/qh2030/Downloads")

    library(tidyverse)
  # Read the data from ACS, 2019, for NY zip codes ----
    acs <- read.csv("ACSDP5Y2019.DP05_data_with_overlays_2021-11-12T140410.csv")
    
    data =  acs %>%
      
            # rename the relevant variables
            mutate(zip = NAME,
                   total_pop        = DP05_0001E,
                   total_18         = DP05_0021E,
                   male_18          = DP05_0026E,
                   female_18        = DP05_0027E,
                   total_white      = DP05_0037E,
                   total_black      = DP05_0038E,
                   total_aian       = DP05_0039E,
                   total_asian      = DP05_0044E,
                   total_islander   = DP05_0052E,
                   total_other      = DP05_0057E,
                   total_morerace   = DP05_0058E,
                   total_hisp       = DP05_0071E,
                   total_nohisp     = DP05_0076E,
                   white_nohisp     = DP05_0077E,
                   black_nohisp     = DP05_0078E,
                   aian_nohisp      = DP05_0079E,
                   asian_nohisp     = DP05_0080E,
                   islander_nohisp  = DP05_0081E,
                   other_nohisp     = DP05_0082E,
                   morerace_nohisp  = DP05_0083E) %>%
      
            # retain only the relevant variables
            dplyr::select(zip,
                          total_pop,
                          total_18,
                          male_18,
                          female_18,
                          total_white,
                          total_black,
                          total_aian,
                          total_asian,
                          total_islander,
                          total_other,
                          total_morerace,
                          total_hisp,
                          total_nohisp,
                          white_nohisp,
                          black_nohisp,
                          aian_nohisp,
                          asian_nohisp,
                          islander_nohisp,
                          other_nohisp,
                          morerace_nohisp) %>%
      
            # zip code should be mononumeric
            mutate(zip = gsub("ZCTA5 ", "", zip))
      
            # make everything numeric
            data[] <- lapply(data, function(x) as.numeric(x))
    
  # Create list of all zip codes sampled by TRIE ----
    nyc <- c( # 1. LES & Chinatown
                10002, 10003, 10009, 10013,
              # 2. Morningside Heights and Hamilton Heights
                10025, 10027, 10031, 10032,
              # 3. Central Harlem
                10026, 10027, 10030, 10037, 10039,
              # 4. East Harlem
                10029, 10035,
              # 5. Washington Heights and Inwood
                10032, 10033, 10034, 10040,
              # 6. Mott Haven & Melrose
                10451, 10454, 10455, 10456,
              # 7. Hunts Point & Longwood
                10455, 10459, 10474,
              # 8. Morrisania & Crotona
                10456, 10459, 10460,
              # 9. Highbridge & Concourse
                10452,
              # 10. Fordham and University Heights
                10453, 10458,
              # 11. Belmont & East Tremont
                10457, 10458,
              # 12. Kingsbridge
                10463, 10468,
              # 13. Parkchester & Soundview
                10472, 10473,
              # 14. Williambridge & Baychester, Edenwald
                10466, 10467, 10468,
              # 15. Bedstuy
                11205, 11206, 11216, 11221, 11233, 11238,
              # 16. Bushwick
                11206, 11207, 11221, 11237,
              # 17. East NY & Starrett City
                11207, 11208, 11239,
              # 18. Sunset Park
                11220, 11232,
              # 19. Coney Island
                11224, 11235,
              # 20. Flatbush & Midwood
                11226,
              # 21. Brownsville
                11212, 11233,
              # 22. East Flatbush
                11203, 11226,
              # 23. Flatlands & Canarsie
                11236,
              # 24. Queensbridge & Astoria
                11101,
              # 25. Jackson Heights
                11368, 11369,
              # 26. Elmhurst & Corona
                11368,
              # 27. Briarwood, Flushing South
                11435,
              # 28. Kew Gardens & Woodhaven
                11419, 11421,
              # 29. Woodhaven, Richmond Hill, South Ozone Park
                11419, 11420,
              # 30. Jamaica & Hollis
                11412, 11423, 11432, 11433, 11434, 11435, 11436,
              # 31. Queens Village
                11429,
              # 32. Rockaway & Broad Channel
                11691, 11692, 11693, 11694,
              # 33. St. George, Stapleton, Port Richmond
                10301, 10303, 10304, 10310)
    
    length(unique(nyc)) # there are repeated zip codes; we have a total of 74 distinct zips
    
    

  # Filter ACS with TRIE zip list ----
    data =  data %>%
            filter(zip %in% nyc) # the total should have 74 rows = 74 distinct zip
    
            
  # Recode data to get the categories we want ----
    data =  data %>%
            mutate( # % gender to get the actual n for each racial/ethnic later on
                    percent_F = female_18 / total_18,
                    percent_M = male_18 / total_18,
                    # % 18 and above
                    prop_18 = total_18 / total_pop,
                    # no. of hispanic race = total_race - race_nohisp
                    white_hisp = total_white - white_nohisp,
                    black_hisp = total_black - black_nohisp,
                    asian_hisp = total_asian - asian_nohisp,
                    aian_hisp  = total_aian  - aian_nohisp,
                    islander_hisp = total_islander - islander_nohisp,
                    morerace_hisp = total_morerace - morerace_nohisp,
                    # actual cells we want - female
                    white_nohisp_F = white_nohisp * percent_F * prop_18,
                    white_hisp_F   = white_hisp   * percent_F * prop_18,
                    black_nohisp_F = black_nohisp * percent_F * prop_18,
                    black_hisp_F   = black_hisp   * percent_F * prop_18,
                    asian_nohisp_F = asian_nohisp * percent_F * prop_18,
                    asian_hisp_F   = asian_hisp   * percent_F * prop_18,
                    aian_nohisp_F  = aian_nohisp  * percent_F * prop_18,
                    aian_hisp_F    = aian_hisp    * percent_F * prop_18,
                    island_nohisp_F = islander_nohisp * percent_F * prop_18,
                    islander_hisp_F = islander_hisp   * percent_F * prop_18,
                    morerace_nohisp_F = morerace_nohisp * percent_F * prop_18,
                    morerace_hisp_F   = morerace_hisp   * percent_F * prop_18,
                    # actual cells we want - male
                    white_nohisp_M = white_nohisp * percent_M * prop_18,
                    white_hisp_M   = white_hisp   * percent_M * prop_18,
                    black_nohisp_M = black_nohisp * percent_M * prop_18,
                    black_hisp_M   = black_hisp   * percent_M * prop_18,
                    asian_nohisp_M = asian_nohisp * percent_M * prop_18,
                    asian_hisp_M   = asian_hisp   * percent_M * prop_18,
                    aian_nohisp_M  = aian_nohisp  * percent_M * prop_18,
                    aian_hisp_M    = aian_hisp    * percent_M * prop_18,
                    island_nohisp_M = islander_nohisp * percent_M * prop_18,
                    islander_hisp_M = islander_hisp   * percent_M * prop_18,
                    morerace_nohisp_M = morerace_nohisp * percent_M * prop_18,
                    morerace_hisp_M   = morerace_hisp   * percent_M * prop_18
                   )
    
    # Random check ----
    data_check <- data %>% 
                  mutate(test = (total_white *prop_18) - (white_nohisp_M + white_hisp_M + white_nohisp_F + white_hisp_F)) %>% 
                  dplyr::select(test) # this should be zero or close to zero due to rounding
    
  # Retain only variables we need ----
    data = data %>%
           dplyr::select(white_nohisp_F   ,
                         white_hisp_F     ,
                         black_nohisp_F   ,
                         black_hisp_F     ,
                         asian_nohisp_F   ,
                         asian_hisp_F     ,
                         aian_nohisp_F    ,
                         aian_hisp_F      ,
                         island_nohisp_F  ,
                         islander_hisp_F  ,
                         morerace_nohisp_F,
                         morerace_hisp_F  ,
                         white_nohisp_M   ,
                         white_hisp_M     ,
                         black_nohisp_M   ,
                         black_hisp_M     ,
                         asian_nohisp_M   ,
                         asian_hisp_M     ,
                         aian_nohisp_M    ,
                         aian_hisp_M      ,
                         island_nohisp_M  ,
                         islander_hisp_M  ,
                         morerace_nohisp_M,
                         morerace_hisp_M  )
  # Transform variables into its sum ----
    final <- as.data.frame(colSums(data)) 
    
    final <- final %>%
             mutate(name = rownames(final)) %>%
             pivot_wider(names_from = name,
                         values_from = c("colSums(data)"))
    
  # Constraining strata size to n = 800
    scalar = 800 / rowSums(final) 
    
    final[] = lapply(final, "*", scalar) 
  
    final = final %>%
            pivot_longer(
              cols = colnames(final), names_to = "x",
                         values_to = "n")
    
    final_round = final
    
    final_round[,2] = lapply(final[,2], round, 0)
    
    # Check that row should sum to 800
      sum(final$n) # 800
      
      sum(final_round$n) # 802 if rounded
      