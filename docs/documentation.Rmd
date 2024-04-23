---
output: html_document
title: Rec DST
---



This shiny application was developed to make the Recreation Demand Model (RDM) for Summer Flounder, Black Sea Bass, and Scup directly accessible to Fisheries Managers. If you have questions, please reach out to Kimberly Bastille (kimberly.bastille@noaa.gov) and Andrew (Lou) Carr-Harris (andrew.carr-harris@noaa.gov).

Details and meeting notes from the Recreational Demand Model Decision Support Tool Working Group can be found at [NOAA Fisheries](https://www.fisheries.noaa.gov/event/recreational-demand-model-decision-support-tool-working-group-summer-flounder-black-sea-bass). 

A full descirption of the model is available - link to lou's tech memo



# Decision Support Tool Glossary
Glossary of terms used in the Recreational Fisheries Decision Support Tool. 

## Regulations
**BagLimit** - Possession Limit - The number of possible kept fish. 

**Season** - 2024 date range indicating open fishing seasons. 

**Length** - Minimum length of possible kept fish in inches. 


## Fishing modes
**For Hire** - Fishing that occurs on party (headboats) or charter vessels.  

**Private** - Fishing that occurs on privately-owned or rented vessels. 

**Shore** - Fishing that occurs from shore.


## Model output (statistics)
**Change in angler satisfaction (`$`)** - Angler satisfaction in dollars is computed as the compensating variation (CV) generated from a change in fishing trip outcomes. CV is a standard welfare calculation and represents the amount of money an individual would need to equate their satisfaction in an altered scenario to that of the original scenario. In other words, it is the compensatory amount of money needed to hold an angler’s satisfaction constant in the face of changing trip outcomes. The RDM tool displays the difference between two separate calculations of CV. The first is the total CV associated with changes in trip outcomes between fishing conditions in 2022, the baseline year, and fishing conditions in 2024 under status-quo regulations (CVSQ_2024). The second is the total CV associated with changes in trip outcomes between fishing conditions in 2022 and fishing conditions in 2024 under alternative, user-selected regulations (CValt_2024). The values of changes in satisfaction ($) displayed in the RDM tool are (CVSQ_2024 - CValt_2024). Thus, a positive value indicates that CV under user-selected regulations is higher than CV under status-quo regulations, while a negative value indicates that CV under user-selected regulations is lower than CV under status-quo regulations. 

**Harvest numbers** - Estimated number of fish harvested by anglers. 

**Harvest pounds** - Estimated pounds of fish harvested by anglers.

**Release numbers** - Estimated number of fish released by anglers.
 
**Release pounds** - Estimated pounds of fish released by anglers. 

**Dead release numbers** - Estimated number of dead discarded fish. Calculated as the product of total release numbers and the stock assessment-based discard mortality rate (10% for summer flounder, 15% for black Sea Bass, 15% for scup).  

**Dead release pounds** - Estimated pounds of dead discarded fish. Calculated as the product of total release pounds and the stock assessment-based discard mortality rate (10% for summer flounder, 15% for black Sea Bass, 15% for scup).  

**Total estimate trips** - Estimated number of trips taken by recreational anglers under 2024 user-selected regulations. 

## Column Definitions
**Status-quo value (median)** - The value of the statistic under status-quo 2024 regulations, computed as the median of the distribution of 100 simulated values.

**Alternative option value** - The value of the statistic under user-selected alternative 2024 regulations. Computed by multiplying the values in the “Status-quo value (median)” and “% difference from status-quo outcome (median)” columns. 

**% difference from status-quo outcome (median)** - The percent difference in the statistic’s value under status-quo 2024 regulations and user-selected alternative 2024 regulations, ((Xalt_2024 - XSQ_2024)/XSQ_2024)*100), computed as the median of the distribution of 100 simulated percent differences. 

**% under harvest target (out of 100 simulations)** - The number of simulations in which harvest pounds are equal to or less than the harvest target. Harvest targets apply to summer flounder and scup only. 
