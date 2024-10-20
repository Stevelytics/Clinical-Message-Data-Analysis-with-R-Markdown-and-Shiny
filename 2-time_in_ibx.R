
# Preliminary Analysis of Time in Inbox for Hadeed Project
#
# Author: James Henderson
# Written: October 12, 2022
# Updated: December 21, 2022
# 79: -------------------------------------------------------------------------

# libraries: ------------------------------------------------------------------
library(tidyverse)
library(data.table)
library(lubridate)
library(plotly)

# paths: ----------------------------------------------------------------------
path = 'C:/Users/USER/Documents/Projects/Upwork/R'

db = 'C:/Users/USER/Documents/Projects/Upwork/R'
pci = sprintf('%s/Primary Care Initiatives', db)
path_to_signal = 
  sprintf('%s/Care Team Optimization (CTO)/Metrics/Epic Signal', pci)
path_to_gm_data = sprintf('%s/GenMed/data', db)

ep_str = '2021Jan_to_2022Dec'
yr_start = ymd('2021-11-01')
yr_end = ymd('2022-10-01')

path_to_data = 
  sprintf('%s/Care Team Optimization (CTO)/Metrics/', pci)

# data: -----------------------------------------------------------------------

# inbox messaging data
#load(sprintf('%s/RData/ibx2-v1.RData', path)) #ibx2

# cFTE and panel-size history
db = 'C:/Users/USER/Documents/Projects/Upwork/R'
pci = sprintf('C:/Users/USER/Documents/Projects/Upwork/R', db)

ps_cfte_file = sprintf('ps_cfte_2021Jan_to_2022Nov.RData', path_to_gm_data)
load(ps_cfte_file) #ps_cfte

# Epic Signal Data
es_data_file = sprintf(
  'Epic_Signal_2021Jan_to_2023Dec.RData', 
  path_to_data
)
foo = load(es_data_file) #es_data

#dir(path_to_gm_data)
#wow_file =  sprintf('%s/wow_analytic_%s.RData', path_to_gm_data, ep_str)

# time in inbox: --------------------------------------------------------------
#es_data[grepl('Time in In Basket', metric), .N, metric][order(-N)][1:50]

ibx_metrics = c(
  'Time in In Basket per Day',
  'Time in In Basket per Day - Patient Medical Advice Request',
  'Time in In Basket per Day - Patient Calls',
  'Time in In Basket per Day - Send Patient Message'
)


tibx = es_data[
  metric %in% ibx_metrics &
    grepl('(GENERAL)|(GEN MED F)|(MED PED)', department),
  .(
    provider_name,
    provider_type,
    department,
    month = floor_date(end, 'month') |> as.Date(),
    start,
    end,
    metric,
    minutes = numerator,
    days = denominator
  )  
]

tibx[, last_year := (month >= yr_start) & (month <= yr_end)]
tibx[, dup := duplicated(.SD)]
tibx = tibx[dup == FALSE]

# merge panel size and cFTE information: --------------------------------------
clinic_xwalk2 = c(
  'Canton Gen Med' = 'CHC GENERAL MEDICINE',
  'E. Ann Arbor Gen Med' = 'EAA GENERAL MEDICINE',
  'Briarwood Gen Med' = 'BW03 GENERAL MEDICINE',
  'Brighton Gen Med' = 'BHC GENERAL MEDICINE',
  'Saline Gen Med' = 'SHC GENERAL MEDICINE',
  'Northville Gen Med' = 'NHC GENERAL MEDICINE',
  'Taubman GMF' = 'TC GEN MED FACULTY',
  'Taubman GMO' = 'TC GEN MED FACULTY',
  'W. Ann Arbor Parkland Gen Med' = 'WAA GENERAL MEDICINE',
  'Brighton Med-Peds' = 'BHC MED PED', # Exclude for now, not a full year
  'Canton Med-Peds' = 'CHC MED PED',
  'E. Ann Arbor Med-Peds' = 'EAA MED PED'
)

ps_cfte[, clinic2 := clinic_xwalk2[as.character(clinic)]]

gm_cfte = ps_cfte[
  !is.na(clinic2) & faculty_resident == 'Faculty',
  .(cfte = sum(cfte), panel_size = sum(panel_size)),
  .(provider, clinic2, measure_end_date)
]
gm_cfte[, month := floor_date(measure_end_date, 'month')]

gm_cfte_tot = gm_cfte[
  grepl('^Phys', provider), 
  .(cfte = sum(cfte), panel_size = sum(panel_size)),
  .(clinic2, month)
]

# suspect these totals are off b/c include different providers: ---------------
gm_cfte[, prov := str_extract(provider, '-(.*), ..') |> 
         str_replace('-', '') |> str_to_lower()
]
#gm_cfte[grepl('Phys-Lin, E', provider)]
#gm_cfte[, .N, .(provider, prov)][grepl('Buc', prov)]
#gm_cfte[, .N, .(provider, prov)][, .N, prov][N > 1]
#tibx
tibx[, prov := str_extract(provider_name, '[^,]*, ..') |> str_to_lower()]

# summarize: ------------------------------------------------------------------

# No longer using, retain until next update. 
#gt2 = gm_cfte_tot[
#  month >= ymd('2023-01-01') & month <= ymd('2023-12-31'),
#  .(cfte = sum(cfte), panel_size = sum(panel_size)),
#  clinic2
#]

tibx[
  gm_cfte,
  `:=`(cfte = i.cfte, panel_size = i.panel_size),
  on = c('prov', 'month')
]
#tibx[, .N, is.na(cfte)]

if ( FALSE ) {
  tibx[
    is.na(cfte) &  month >= ymd('2023-01-01') & month <= ymd('2023-12-31'),
    .N,
    .(provider_name, prov)] %>%
    .[order(-N)]
}

tib_month = tibx[
  !is.na(cfte) & last_year == TRUE,  
  .(
    minutes = sum(minutes),
    days = sum(days),
    mean = sum(minutes) / sum(days),
    cfte = sum(cfte),
    panel_size = sum(panel_size)
  ),
  keyby = .(metric, clinic2 = department, month)
]


tib_tot = tibx[
  !is.na(cfte) & last_year == TRUE,  
     .(
       minutes = sum(minutes),
       days = sum(days),
       mean = sum(minutes) / sum(days),
       cfte = sum(cfte),
       panel_size = sum(panel_size)
     ),
     keyby = .(metric, clinic2 = department)
]

#tib_tot = merge(tib_tot, gt2, by = 'clinic2')
tib_tot[, `Avg Time` := round(minutes / cfte / 8 / 60, 2)] #hrs per cfte per month

# Total time
tt1 = tib_tot[metric == ibx_metrics[1]] 
tt1 = tt1[order(`Avg Time`)]
tt1[, Clinic := factor(clinic2, clinic2)]

ibx_titles = str_replace(ibx_metrics, ' per Day', '')
p1 = ggplot(tt1, aes(x = `Avg Time`, y = Clinic)) +
  geom_col() +
  theme_bw() +
  xlab('Hours in In Basket / cFTE / month') +
  ylab('') +
  ggtitle(ibx_titles[1])
#ggplotly(p1)

# Portal Messages
tt2 = tib_tot[metric == ibx_metrics[2]] 
tt2 = tt2[order(`Avg Time`)]
tt2[, Clinic := factor(clinic2, clinic2)]
p2 = ggplot(tt2, aes(x = `Avg Time`, y = Clinic)) + 
  geom_col() +  
  theme_bw() +
  xlab('Hours in In Basket / cFTE / month') +
  ylab('') +
  ggtitle(ibx_titles[2])
#ggplotly(p2)

# Patient calls
tt3 = tib_tot[metric == ibx_metrics[3]] 
tt3 = tt3[order(`Avg Time`)]
tt3[, Clinic := factor(clinic2, clinic2)]
p3 = ggplot(tt3, aes(x = `Avg Time`, y = Clinic)) + 
  geom_col() +
  theme_bw() +
  xlab('Hours in In Basket / cFTE / month') +
  ylab('') +
  ggtitle(ibx_titles[3])
#ggplotly(p3)


# Send Patient Message
tt4 = tib_tot[metric == ibx_metrics[4]] 
tt4 = tt4[order(`Avg Time`)]
tt4[, Clinic := factor(clinic2, clinic2)]
p4 = ggplot(tt4, aes(x = `Avg Time`, y = Clinic)) + 
  geom_col() +
  theme_bw() +
  xlab('Hours in In Basket / cFTE / month') +
  ylab('') +
  ggtitle(ibx_titles[4])
#ggplotly(p4)

