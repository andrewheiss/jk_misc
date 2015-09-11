# Projects

## ✅ Cables trafficking

Instructions: `Cables Trafficking.csv`

> Calculate VAR1: the percent of available cables that are on TIP

> Calculate VAR2: the percent of cables available

> Create country year dataset with cow code, year, and these two variables

> Create map of density of TIP effort as defined by VAR1: the percent of available cables that are on TIP


TODO: 

* [x] Create new variables and dataset
* [x] Create map with shaded countries, points for each embassy(?)

Scripts: 

* [`cables_geocode_embassies.R`](cables_geocode_embassies.R)
* [`cables_tip.R`](cables_tip.R)

Output: 

* `data/cables_panel.dta`
* [`figures/map_avg_tip_effort.pdf`](figures/map_avg_tip_effort.pdf)
* [`figures/map_avg_tip_effort.png`](figures/map_avg_tip_effort.png)
* [`figures/map_avg_tip_effort_binned.pdf`](figures/map_avg_tip_effort_binned.pdf)
* [`figures/map_avg_tip_effort_binned.png`](figures/map_avg_tip_effort_binned.png)


## ⭕️ Funding

Instructions: `Funding/Funding analysis.docx`

TODO:

* [x] Lots of figures
* [ ] Load content into qualitative data software and find common themes
* [x] Clean database

Script: None so far

Output: 

* `data/funding_clean.dta`
* [`figures/fig_grants_purpose.pdf`](figures/fig_grants_purpose.pdf)
* [`figures/fig_grants_purpose.png`](figures/fig_grants_purpose.png)
* [`figures/fig_grants_to_all_sectors_collapsed_sectors.txt`](figures/fig_grants_to_all_sectors_collapsed_sectors.txt)
* [`figures/fig_grants_to_all_sectors.pdf`](figures/fig_grants_to_all_sectors.pdf)
* [`figures/fig_grants_to_all_sectors.png`](figures/fig_grants_to_all_sectors.png)
* [`figures/fig_grants_to_igos_collapsed_igos.txt`](figures/fig_grants_to_igos_collapsed_igos.txt)
* [`figures/fig_grants_to_igos.pdf`](figures/fig_grants_to_igos.pdf)
* [`figures/fig_grants_to_igos.png`](figures/fig_grants_to_igos.png)


## ⭕️ Improvements

Instructions: `improvements/Statistics.docx`

TODO:

* [ ] Generate data
* [ ] Graph changes over time or make table 

Script: None so far

Output: 

* None so far


## ✅ Map of criminalization

Instructions: `Map of criminalization.docx`

TODO:

* [x] Create map

Script: [`criminalization.R`](criminalization.R)

Output: 

* [`figures/map_criminalization.pdf`](figures/map_criminalization.pdf)
* [`figures/map_criminalization.png`](figures/map_criminalization.png)


## ✅ Map of flows

Instructions: `Map of flows.docx`

TODO:

* [x] Create base map with R
* [x] Add labels and arrows in Illustrator
* [x] Make Greenland part of Denmark

Script: [`unodc_base_map.R`](unodc_base_map.R)

Output: 

* [`figures/unodc_base_map.pdf`](figures/unodc_base_map.pdf)
* [`figures/unodc_new.pdf`](figures/unodc_new.pdf)
* [`figures/unodc_new.png`](figures/unodc_new.png)
* [`figures/unodc_new.ai`](figures/unodc_new.ai)


## ✅ Map of when in report

Instructions: `map of when in report.xlsx`

TODO:

* [x] Determine when countries were added to TIP report
* [x] Create maps for 2001-2007+
* [x] Check if we can collapse 2006-2010 instead of 2007-2010 so the grid in the figure is nicer

Script: [`year_joined.R`](year_joined.R)

Output: 

* [`data/year_joined.csv`](data/year_joined.csv)
* [`figures/map_joined_report.pdf`](figures/map_joined_report.pdf)
* [`figures/map_joined_report.png`](figures/map_joined_report.png)


## ⭕️ Reactions

Instructions: `Reactions.docx`

TODO:

* [ ] See file 

Script: None so far

Output: 

* Improve table


## ⭕️ Timelines

Instructions: `timeslines/Timeline.docx`

TODO:

* [x] Make cool timelines for Indonesia 
* [ ] Add meeting dots
* [ ] Verify TIP recommendations happened in those years

Script: [`indonesia_timeline.R`](indonesia_timeline.R)

Output: 

* [`figures/Indonesia timeline.pdf`](figures/Indonesia timeline.pdf)
* [`figures/Indonesia timeline-01.png`](figures/Indonesia timeline-01.png)
* [`figures/Indonesia timeline-02.png`](figures/Indonesia timeline-02.png)
* [`figures/indonesia_raw.pdf`](figures/indonesia_raw.pdf)


## ✅ "Update this figure and add other stats…"

Instructions: `Update this figure and add other stats….docx`

*Updated data from [http://www.state.gov/j/tip/rls/tiprpt/2015/243366.htm](http://www.state.gov/j/tip/rls/tiprpt/2015/243366.htm) and elsewhere.*

TODO:

* [x] Recreate figure 
* [x] Add 2015 data

Script: [`tier_placements.R`](tier_placements.R)

Output: 

* [`data/tiers_2015.csv`](data/tiers_2015.csv)
* [`figures/tier_percents.pdf`](figures/tier_percents.pdf)
* [`figures/tier_percents.png`](figures/tier_percents.png)
