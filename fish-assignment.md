
# Unit 3: Fisheries Collapse Analysis Report
### Nazerke Sharauova and Latosha Sanchez

This work focuses on the examining the results of fisheries collapse research reported earlier by [Worm et al](https://doi.org/10.1126/science.1132294). In particular, two analyses are conducted to corroborate 
the original findings. One evaluates an individual fish taxon called "Gadus morhua" (a.k.a. "North-Atlantic cod"), 
and how the landings of this taxon collapsed in recent years. Another analysis revisits the taxa collapse 
findings of the original research work, and corroborates that the taxa collapse trend still exists.

## Data for Analysis
To conduct both analyses, we use the data from the [RAM Legacy Stock Assessment Database](https://doi.org/10.5281/zenodo.2542918).

In particular, we focus on reported landings and catches, and specifically consider TC-MT and TCbest-MT series, where TC
stands for Total Catches, and the MT refers to Metric Tons. 

Below we load necessary libraries, and download the data as necessary.

```r
library("tidyverse")
library("readxl")

excel_version <- "RLSADB v4.44/DB Files With Assessment Data/RLSADB v4.44 (assessment data only).xlsx"
if(!file.exists(excel_version)){
  download.file("https://zenodo.org/record/2542919/files/RLSADB%20v4.44.zip?download=1",
                "ramlegacy.zip")
unzip("ramlegacy.zip") 
}
```

Next, we read the data into tables, and print their names to verify that they were successfully loaded.

```r
sheets <- readxl::excel_sheets(excel_version)
ram <- lapply(sheets, readxl::read_excel, path = excel_version)
names(ram) <- sheets
names(ram)
```

```
##  [1] "area"                         "assessment"                  
##  [3] "assessmethod"                 "assessor"                    
##  [5] "biometrics"                   "bioparams"                   
##  [7] "bioparams_assessments_views"  "bioparams_ids_views"         
##  [9] "bioparams_notes_views"        "bioparams_sources_views"     
## [11] "bioparams_units_views"        "bioparams_values_views"      
## [13] "management"                   "stock"                       
## [15] "taxonomy"                     "timeseries"                  
## [17] "timeseries_assessments_views" "timeseries_ids_views"        
## [19] "timeseries_notes_views"       "timeseries_sources_views"    
## [21] "timeseries_units_views"       "timeseries_values_views"     
## [23] "timeseries_years_views"       "tsmetrics"
```

## Investigating North-Atlantic Cod

To conduct the analysis of the North-Atlantic cod, we connect the stock, area, and timeseries tables, then filter specifically for Gadus morhua, the scientific name of the North-Atlantic cod. Also, we limit our analysis to USA and Canada, but keep all available years, and the results depict very troubling picture.


```r
ram$timeseries %>%
  left_join(ram$stock) %>% 
  left_join(ram$area) %>% 
  left_join(ram$tsmetrics, by = c("tsid" = "tsunique")) %>%
  filter(scientificname == "Gadus morhua", 
         country == "USA" | country == "Canada") %>%
  filter(tsid == "TC-MT") %>% 
  group_by(tsyear) %>%
  summarize(total_landings = sum(tsvalue, na.rm = TRUE)) %>% 
  ggplot(aes(tsyear, total_landings)) + 
  geom_line(size = .75) +
  labs(title = "Fish Landings of Atlantic Cod (in tons)", 
       y = "Total Landings (MT)", 
       x = "Year" ) +
  theme(plot.title = element_text(size = 18, face = 'bold', color = 'steelblue4'), 
        axis.title = element_text(size = 12, color = 'steelblue4', face = 'bold'),
        axis.text = element_text(size = 11, face = 'bold'), 
        panel.grid.major = element_line(color = 'white', size = 1), panel.grid.minor = element_line()) 
```

![](fish-assignment_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

As it can be seen from the graph above, the total landings of the cod was relatively stable ever since it was started to be recorded in 1850s, then the total landings grew starting in 1950 with the rise of fisheries and overall efforts by the fishing industry, which then inevitebly led to the catastrophic collapse of the landings closer to 2000s and beyond. And this is happened regardless of the sustainable fish farming efforts that were underway in 2000s and more recently in 2010s. And so Gadus morhua consistutes one of the many fish taxa that collapsed due to overfishing in 2000s and after.

## Analysis of Stock Collapse

In this analysis, we revisit the original research findings, where the focus was on the collapse of the stock for various taxa (fish types). Specifically, the stock collapse was defined as the drop of the landings/catches below 10% of the previously reported maximum. While the original research work considered collapses pertaining to Large Marine Ecosystems, we examine the stock collapses of various taxa directly, based on the best timeseries data present in RAM database (i.e., TCbest-MT).

Specific steps for obtaining collapse information included joining stock, area, and timeseries tables, filtering on catch/landings and specific time range [1950-2016]. Then the data was grouped by scientific name (taxon) to obtain total landings for each taxon+year pair. To keep the data valid, total landings that didn't result in valid value were removed. Next, the data was arranged by years, so that cumulative maximum would be valid in timeseries. Then, cumulative maximum for total landings from 1950 to given year were calculated for each year, and in case the given year had stock collapse (less than 10% of cumulative maximum landings so far), it was marked positive for collapse, otherwise it marked negative. Then, additional cleaning was done to remove preceeding years in series per taxon where cumulative maximum remained zero (in certain cases data was either zero or not recorded) until non-zero landings were recorded. Finally, when grouping for the unique year+[collapse/noncollapse] pairs, and summarizing by counting the collapses and non-collapses, and again grouping for year, we received the total collapses per year divided by total taxa recorded in that year. The code and results are presented below.


```r
  collapse_data <-
  ram$timeseries %>% 
  left_join(ram$stock) %>% 
  left_join(ram$area) %>%
  left_join(ram$tsmetrics, by = c("tsid" = "tsunique")) %>%
  filter(tsid == "TCbest-MT") %>%
  filter(tscategory == "CATCH or LANDINGS") %>% 
  filter(tsyear > 1949, tsyear < 2017) %>%
  group_by(scientificname, tsyear) %>%
  summarize(total_landings = sum(tsvalue, na.rm = TRUE)) %>%
  filter(!is.na(total_landings)) %>%
  arrange(tsyear) %>%
  mutate(cummax = cummax(total_landings)) %>%
  mutate(collapsed = total_landings < 0.1 * cummax) %>%
  filter(cummax != 0) %>%
  group_by(tsyear, collapsed) %>%
  summarize(collapsed_taxa = sum(collapsed == TRUE), noncollapsed_taxa = sum(collapsed == FALSE)) %>%
  group_by(tsyear) %>% 
  summarize(total_taxa = sum(collapsed_taxa + noncollapsed_taxa), collapsed_taxa = sum(collapsed_taxa)) %>%
  mutate(collapsed_percent = collapsed_taxa/total_taxa*100)

collapse_data
```

```
## # A tibble: 67 x 4
##    tsyear total_taxa collapsed_taxa collapsed_percent
##     <dbl>      <int>          <int>             <dbl>
##  1   1950         96              0             0    
##  2   1951         98              1             1.02 
##  3   1952        104              0             0    
##  4   1953        105              2             1.90 
##  5   1954        107              2             1.87 
##  6   1955        109              1             0.917
##  7   1956        111              1             0.901
##  8   1957        111              2             1.80 
##  9   1958        112              1             0.893
## 10   1959        113              2             1.77 
## # … with 57 more rows
```

Looking at the table above, we can verify that the taxa were counted for each year, and number of collapsed taxa, as well as relative percentage were computed correctly. When plotting this data below, however, an alarming picture emerged.


```r
collapse_data %>%
 ggplot(aes(tsyear, collapsed_percent)) +
 geom_point(shape=23, fill="black", color="black", size=3) +
 scale_y_reverse() + 
 labs( title = "Global Loss of Species", 
       y = "Collapsed taxa (%)", 
       x = "Year" ) +
 theme(plot.title = element_text(size = 18, face = 'bold', color = 'steelblue4'), 
       axis.title = element_text(size = 12, color = 'steelblue4', face = 'bold'),
       axis.text = element_text(size = 11, face = 'bold'), 
       panel.background = element_blank(),
       panel.grid.major = element_blank(), 
       panel.grid.minor = element_blank(),
       axis.line = element_line(colour = "black"), 
       panel.border = element_rect(colour = "black", fill=NA, size=1)
 )
```

![](fish-assignment_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

The graph above shows percentage of taxa for which the stock collapsed broked down by year. In agreement with the original research work, the results clearly indicate alarming rate of collapse - 21%-26% in most recent years. When setting the end year to 2006, it can be seen that the results were around 10%-12%, the difference here being in methodology of aggregating the data per LMEs (original work) versus all available data (our analysis method). If the methodology of the original research work was applied today, it very well might lead even higher numbers than 29% that was reported originally reported. This demonstrates that the stock collapse rates might have accelerated significantly in 64 LMEs.


