# Preliminary Analysis of Volume and Phys Touch Rate by GenMed Clinics
#
# Author: James Henderson
# Written: September 28, 2022
# Updated: January 26, 2024
# 79: -------------------------------------------------------------------------

# libraries: ------------------------------------------------------------------
library(tidyverse)
library(data.table)
library(lubridate)
library(plotly)

# utilities: ------------------------------------------------------------------
pwc = function(n) {
  format(n, big.mark = ',')
}
`%nin%` = function(x, y) !(x %in% y)

# data: -----------------------------------------------------------------------
path = 'C:/Users/USER/Documents/Projects/Upwork/R'
db = '/Users/jbhender/Dropbox (University of Michigan)'

# inbox messaging data
load(sprintf('ibx2_revised_data.RData', path)) #ibx2

# cFTE and panel-size history
bar = load(sprintf('ps_cfte_2021Jan_to_2022Nov.RData', db)) # month_effort

# delete when finished
if ( FALSE ) {
  pci = sprintf('%s/Primary Care Initiatives', db)
  path_to_data = 
    sprintf('%s/Care Team Optimization (CTO)/Metrics/panel_cFTE_data', pci)
  path_to_gm_data = sprintf('%s/GenMed/data', db)
  dir(path_to_gm_data)
  ps_cfte_file = sprintf('%s/ps_cfte_2021Jan_to_2022Nov.RData', path_to_gm_data)
  load(ps_cfte_file) #ps_cfte
}

save_plots = FALSE

# simple counts: --------------------------------------------------------------
ibx2_revised[, last_year := month >= ymd('2021-12-01') & month <= ymd('2022-11-01')]

# count messages once per msg_id (sent not received)
n_month = ibx2_revised[
  last_year == TRUE,
  .N,
  .(enc_dept, month, msg_type, msg_id)
  ] %>% 
  .[, .(N = .N, n_to = sum(N)), .(enc_dept, month, msg_type)]

n_phys_month = ibx2_revised[
  prov_type == 'Physician' & last_year == TRUE,
  .N,
  .(enc_dept, month, msg_type, msg_id)
] %>% 
  .[, .N, .(enc_dept, month, msg_type)]

n_phys_month_sent = ibx2_revised[
  phys_sender == TRUE & last_year == TRUE,
  .N,
  .(enc_dept, month, msg_type, msg_id)
] %>% 
  .[, .N, .(enc_dept, month, msg_type)]


# aggregate cfte
ps_cfte[, month := floor_date(measure_end_date, 'month') |> as.Date()]
ps_cfte[, clinic := as.character(clinic)]
ps_cfte[, clinic2 := ifelse(grepl('Taubman', clinic), 'Taubman', clinic)]

cfte_total = ps_cfte[,
  .(cfte = sum(cfte), panel_size = sum(panel_size)), 
  .(month, clinic2, provider_type)
]

# clinic crosswalk 
clinic_xwalk = c(
  'Canton Gen Med' = 'CHC GENERAL MEDICINE',
  'E. Ann Arbor Gen Med' = 'EAA GENERAL MEDICINE',
  'Briarwood Gen Med' = 'BW03 GENERAL MEDICINE',
  'Brighton Gen Med' = 'BHC GENERAL MEDICINE',
  'Saline Gen Med' = 'SHC GENERAL MEDICINE',
  'Northville Gen Med' = 'NHC GENERAL MEDICINE',
  'Taubman' = 'TC GEN MED FACULTY',
  'Taubman' = 'TC GEN MED RESIDENT',
  'W. Ann Arbor Parkland Gen Med' = 'WAA GENERAL MEDICINE',
  'Brighton Med-Peds' = 'BHC MED PED', # Exclude for now, not a full year
  'Canton Med-Peds' = 'CHC MED PED',
  'E. Ann Arbor Med-Peds' = 'EAA MED PED'
)
cfte_total[, enc_dept := clinic_xwalk[clinic2]]

# add cfte and panel size information to output: ------------------------------
n_month[
  cfte_total[provider_type == 'Physician' & !is.na(enc_dept)],
  `:=`(cfte = i.cfte, panel_size = i.panel_size),
  on = c('enc_dept', 'month')
]

ex_dept =  c('TC GEN MED RESIDENT', 'BHC MED PED')
n_month = n_month[enc_dept %nin% ex_dept]

n_phys_month[
  cfte_total[provider_type == 'Physician' & !is.na(enc_dept)],
  `:=`(cfte = i.cfte, panel_size = i.panel_size),
  on = c('enc_dept', 'month')
]
n_phys_month = n_phys_month[enc_dept %nin% ex_dept]

n_phys_month_sent[
  cfte_total[provider_type == 'Physician' & !is.na(enc_dept)],
  `:=`(cfte = i.cfte, panel_size = i.panel_size),
  on = c('enc_dept', 'month')
]
n_phys_month_sent = n_phys_month_sent[enc_dept %nin% ex_dept]

# total message volume per cfte: ----------------------------------------------
n_month[, msg_per_fte := N / cfte]
n_month[, `:=`(Clinic = enc_dept, `Messages per cFTE` = round(msg_per_fte, 1))]
p_n_trend = 
  ggplot(n_month, aes(x = month, y = `Messages per cFTE`, color = Clinic)) + 
  facet_wrap(~msg_type) + 
  geom_line() + 
  theme_bw() + 
  xlab('') + 
  ylab('# Messages / cFTE / month') 
#ggplotly(p_n_trend)

# total volume: ---------------------------------------------------------------
n_tot = n_month[,
  .(n_msg = sum(N), cfte = sum(cfte) / 12),
  keyby = .(enc_dept, msg_type)
]
n_tot[, msg_per_fte_month := round(n_msg / cfte / 12, 1)]
clin_ord = n_tot[msg_type == 'Patient Medical Advice Request'] %>% 
  .[order(msg_per_fte_month), enc_dept]
n_tot[, Clinic := factor(enc_dept, clin_ord)]
n_tot[, 
      Type := factor(
        msg_type, 
        rev(c('Patient Medical Advice Request', 'Patient Calls'))
      )
]

n_tot[, `:=`(`Avg Messages` = msg_per_fte_month)]
p_n = ggplot(n_tot, aes(y = Clinic, x = `Avg Messages`, fill = Type)) + 
  geom_col(
    position = position_dodge()
  ) +
  theme_bw() +
  scale_fill_manual(values = rev(c('darkblue', 'darkgrey'))) +
  xlab('Total messages per (physician) cFTE per month') #+
#  ggtitle('August 2021 - July 2022')
ggplotly(p_n)
# physician volume sent: ------------------------------------------------------
n_phys_sent_tot = n_phys_month_sent[,
  .(n_msg = sum(N), cfte = sum(cfte) / 12),
  keyby = .(enc_dept, msg_type)
]

n_phys_sent_tot[, msg_per_fte_month := round(n_msg / cfte / 12, 1)]
clin_ord = n_phys_sent_tot[msg_type == 'Patient Medical Advice Request'] %>% 
  .[order(msg_per_fte_month), enc_dept]
n_phys_sent_tot[, Clinic := factor(enc_dept, clin_ord)]
n_phys_sent_tot[, 
      Type := factor(
        msg_type, 
        rev(c('Patient Medical Advice Request', 'Patient Calls'))
      )
]
n_phys_sent_tot[, `:=`(`Avg Messages` = msg_per_fte_month)]
p_n_phys_sent = ggplot(
  n_phys_sent_tot, 
   aes(y = Clinic, x = `Avg Messages`, fill = Type)) + 
  geom_col(
    position = position_dodge()
  ) +
  theme_bw() +
  scale_fill_manual(values = rev(c('darkblue', 'darkgrey'))) +
  xlab('Total physician messages sent per cFTE per month') #+
#  ggtitle('August 2021 - July 2022')

# physician volume per cFTE: --------------------------------------------------
n_phys_month[, msg_per_fte := N / cfte]
p_n_phys_trend = 
  ggplot(n_phys_month, aes(x = month, y = msg_per_fte, color = enc_dept)) + 
  facet_wrap(~msg_type) + 
  geom_line() +
  theme_bw() 

# physician + resident volume per cFTE: ---------------------------------------

# total phys volume: ----------------------------------------------------------
n_phys_tot = n_phys_month[,
  .(n_msg = sum(N), cfte = sum(cfte) / 12),
  keyby = .(enc_dept, msg_type)
]
n_phys_tot[, msg_per_fte_month := n_msg / cfte / 12]
clin_ord = n_phys_tot[msg_type == 'Patient Medical Advice Request'] %>% 
  .[order(msg_per_fte_month), enc_dept]
n_phys_tot[, Clinic := factor(enc_dept, clin_ord)]
n_phys_tot[, 
  Type := factor(
    msg_type, 
    rev(c('Patient Medical Advice Request', 'Patient Calls'))
  )
]

n_phys_tot[, `Avg Messages` := round(msg_per_fte_month, 1)]
p_n_phys = 
  ggplot(n_phys_tot, aes(y = Clinic, x = `Avg Messages`, fill = Type)) + 
  geom_col(
    position = position_dodge()
  ) +
  theme_bw() +
  scale_fill_manual(values = rev(c('darkblue', 'darkgrey'))) +
  xlab('Physician messages per cFTE per month') #+
#  ggtitle('August 2021 - July 2022')
  
# physician encounter touch rate: ---------------------------------------------
pt = ibx2_revised[
  last_year == TRUE, 
  .(phys = any(phys_user)),
  .(enc_dept, month, msg_type, csn)
]
# including encounter provider get's us up to 433992; 46% more events
#  would want to flag encounter providers in wide format possibly 

pt_rate = pt[, .(.N, p = 100 * mean(phys)), .(enc_dept, month, msg_type)]
pt_rate[, Clinic := enc_dept]
p_ptr_trend = ggplot(pt_rate, aes(x = month, y = p, color = Clinic)) + 
  facet_wrap(msg_type ~ .) +
  geom_line() +
  theme_bw() +
  ylab('% portal encounters with 1+ physician message') +
  ylim(c(0, 100))

# encounter physician sent rate: ----------------------------------------------
# TODO: Investigate why phys_sender is NA for these 629 cases. 
ibx2_revised[is.na(phys_sender) == TRUE, phys_sender := FALSE]
pt_sent = ibx2_revised[
  last_year == TRUE, 
  .(
    phys_to = any(phys_user),
    phys_send = any(phys_sender)
  ),
  .(enc_dept, month, msg_type, csn)
]
# including encounter provider get's us up to 433992; 46% more events
#  would want to flag encounter providers in wide format possibly 

pt_sent_rate = pt_sent[, 
  .(.N,
    n_to = sum(phys_to), 
    p = 100 * mean(phys_send),
    p2 = 100 * sum(phys_send) / sum(phys_to)
    ), 
  .(enc_dept, month, msg_type)
]
pt_sent_rate[, Clinic := enc_dept]
p_ptr_sent_trend = ggplot(
  pt_sent_rate,
  aes(x = month, y = p2, color = Clinic)
) + 
  facet_wrap(msg_type ~ .) +
  geom_line() +
  theme_bw() +
  ylab('% portal encounters with 1+ physician messages') +
  ylim(c(0, 100))

# total for year prior: -------------------------------------------------------
pt_rate2 = pt[, 
  .(.N, p = 100 * mean(phys)), 
  .(Clinic = enc_dept, Type = msg_type)
]
clin_ord = pt_rate2[Type == 'Patient Medical Advice Request']  %>% 
  .[order(p), Clinic]
pt_rate2[, Clinic := factor(Clinic, clin_ord)]
pt_rate2[, Type := factor(
  Type, 
  c('Patient Medical Advice Request', 'Patient Calls')
)]

pt_rate2[, `:=`(`%` = round(p, 1))]
p_ptr = ggplot(pt_rate2, aes(x = `%`, y = Clinic, fill = Type)) + 
  geom_col(
    position = position_dodge(width = -0.9)
  ) +
  theme_bw() +
  scale_fill_manual(values = rev(c('darkgrey', 'darkblue'))) + 
  xlab('% of portal encounters with at least one physician message') +
  xlim(c(0, 100)) #+
#  ggtitle('August 2021 - July 2022')

# phys sent encounter rate: ---------------------------------------------------
pt_sent_rate2 = pt_sent[,
  .(.N, p = 100 * mean(phys_send)), 
  .(Clinic = enc_dept, Type = msg_type)
]
clin_ord = pt_sent_rate2[Type == 'Patient Medical Advice Request']  %>% 
  .[order(p), Clinic]
pt_sent_rate2[, Clinic := factor(Clinic, clin_ord)]
pt_sent_rate2[, Type := factor(
  Type, 
  c('Patient Medical Advice Request', 'Patient Calls')
)]

pt_sent_rate2[, `:=`(`%` = round(p, 1))]
p_psr = ggplot(pt_sent_rate2, aes(x = `%`, y = Clinic, fill = Type)) + 
  geom_col(
    position = position_dodge(width = -0.9)
  ) +
  theme_bw() +
  scale_fill_manual(values = rev(c('darkgrey', 'darkblue'))) + 
  xlab('% of portal encounters with 1+ message sent by a physician') +
  xlim(c(0, 100)) #+


# save
if ( save_plots ) {
  for ( p in grep('^p_', ls(), value = TRUE) ) {
    ggsave(
      sprintf('~/Desktop/hadeed_plots/%s.pdf', p),
      get(p)    
    )
  }
}

# cc behavior: ----------------------------------------------------------------
ibx2_revised[, n_msg := .N, msg_id]
cc = ibx2_revised[
  msg_type == 'Patient Medical Advice Request', 
  .(phys_all = all(phys_user), phys_any = any(phys_user), n_msg = n_msg[1]),
  .(msg_id, enc_dept)
]
#cc[n_msg > 1, .N, keyby = .(phys_all, phys_any)]

cc_clinic = cc[, .N, keyby = .(enc_dept, cc = n_msg > 1, phys_all, phys_any)]

cc_clinic[, percent := 100 * N / sum(N), enc_dept]
#cc_clinic[cc == TRUE & phys_any][order(-percent)]
# Are these messages sent to the resident and physician? 
#cc_clinic[cc == TRUE & phys_any & !phys_all][order(-percent)]

cc_clinic2 = cc[, .N, keyby = .(enc_dept, cc = n_msg > 1, phys_any)]
cc_clinic2[, percent := 100 * N / sum(N), enc_dept]
cc_clinic2[, percent := round(percent, 1)]
cc_clinic2[,
 `Sent To` := ifelse(
   cc,
   ifelse(phys_any, 'Physician & Non-physician', 'Multiple Non-physicians'),
   ifelse(phys_any, 'Physician only', 'Non-physician only')
 ) 
]
cc_clinic2[,
  `Sent To` := factor(
    `Sent To`,
    c(
      'Physician only', 
      'Physician & Non-physician', 
      'Non-physician only',
      'Multiple Non-physicians' 
    )
  )
]
clin_ord = 
  cc_clinic2[phys_any == TRUE, .(phys_percent = sum(percent)), enc_dept] %>%
  .[order(phys_percent), enc_dept]
cc_clinic2[, Clinic := factor(enc_dept, clin_ord)]
cc_clinic2 = cc_clinic2[order(Clinic)]

p_msg_pct = ggplot(cc_clinic2, aes(x = percent, y = Clinic, fill = `Sent To`)) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  theme_bw() +
  xlab('% of patient medical advice request messages') +
  scale_fill_manual(
    values = c('darkblue', 'slateblue', 'goldenrod', 'orange')
  )
#ggplotly(p_msg_percent)

if ( save_plots ) {
  ggsave(
    '~/Desktop/hadeed_plots/p_msg_percent.pdf',
    p_msg_percent
  )
}

# cc behavior by sender type: -------------------------------------------------
ibx2_revised[cc, `:=`(phys_any = i.phys_any, phys_all = i.phys_all), on = 'msg_id']

cc_phys_sender = ibx2_revised[
  last_year == TRUE & 
  msg_type == 'Patient Medical Advice Request' & 
  n_msg > 1 & phys_any == TRUE, 
  .N, 
  .(enc_dept, sender_type, msg_id)
] %>%
  .[, .N, keyby = .(enc_dept, sender_type)]
cc_phys_sender[, `%` := round(100 * N / sum(N), 1), enc_dept]
cc_phys_sender = cc_phys_sender[order(enc_dept, -N)]
cc_phys_sender[, `Total CCs to Phys` := sum(N), enc_dept]
# order by 'clin_ord' from figure above
cc_phys_sender[, Clinic := factor(enc_dept, levels = clin_ord)]
role_lev = c('Physician', 'Resident', 'PA', 'NP', 'RN', 'LPN', 'MA', 'Other')
cc_phys_sender[, `Sender Role` := factor(sender_type, levels = role_lev)]
cc_phys_sender[, text := sprintf('N: %s / %s', pwc(N), pwc(sum(N))), enc_dept]
p_cc_role = ggplot(
  cc_phys_sender,
  aes(x = `%`, y = Clinic, fill = `Sender Role`, text = text)
) +
  geom_col(position = position_stack('reverse' = TRUE)) +
  theme_bw() +
  xlab("% of clinic's messages CC'd to physicians by sender role") +
  scale_fill_manual(
    values = c(
      Physician = 'darkblue', 
      Resident = 'slateblue',
      PA = 'darkgreen',
      NP = 'green4',
      RN = 'darkgrey',
      LPN = 'grey80',
      MA = 'orange',
      Other = 'thistle',
      'NA' = 'black'
    )
  )
#ggplotly(p_cc_role)


# breakdown or roles on cc'd messages: ----------------------------------------

#ibx2[n_msg > 1, ]

# model for encounter phys touch: ---------------------------------------------
#dt = pt[month >= ymd('2021-08-01')]

# turnaround time: ------------------------------------------------------------
tat = ibx2_revised[
  last_year == TRUE & 
  msg_type == 'Patient Medical Advice Request',
  .(
    csn_start = csn_start[1],
    tat_min = min(min_csn_done_time),
    tat_max = max(max_csn_done_time)),
  keyby = .(enc_dept, csn)
]

# TAT min done: ---------------------------------------------------------------
tat[, `:=`(
  tat_hr = as.numeric(tat_min - csn_start) / (60 * 60)
)]

# exclude messages not done and exclude those with negative times
tat_summary = tat[
  !is.na(tat_hr) & tat_hr > 0, 
  .(
    .N,
    pct_72 = mean(tat_hr <= 72) * 100,
    pct_48 = mean(tat_hr <= 48) * 100,
    tat_mean = mean(tat_hr),
    tat_med = median(tat_hr),
    tat_90 = quantile(tat_hr, 0.9),
    tat_mad = mean(abs(tat_hr - median(tat_hr)))
  ), 
  enc_dept
]
tat_summary[, `:=`(
  p_lwr = floor((N + 1) / 2 - qnorm(.975) * sqrt(N) / 2) / N,
  p_upr = ceiling((N + 1) / 2 + qnorm(.975) * sqrt(N) / 2) / N
)]

tat_ord = tat_summary[order(tat_med), enc_dept]
tat[, Clinic := factor(enc_dept, tat_ord)]
tat_summary[, Clinic := factor(enc_dept, tat_ord)]

p_tat_min_box = ggplot(
  tat[!is.na(tat_hr) & tat_hr > 0],
  aes(x = tat_hr, y = Clinic)
) +
  geom_boxplot(outlier.shape = NA) +
  xlim(c(0, 72)) +
  xlab('Turnaround Time (min done), hours') +
  ylab('')
  theme_bw() 

# CI for median
lwr = upr = vector('numeric', length = nrow(tat_summary))
names(lwr) = names(upr) = tat_summary[, enc_dept]
for ( i in 1:nrow(tat_summary) ) {
  cln = tat_summary[i, enc_dept]
  p = tat_summary[i, c(p_lwr, p_upr)]
  q = tat[
    enc_dept == cln & !is.na(tat_hr) & tat_hr > 0,
    quantile(tat_hr, p)
  ]
  lwr[i] = q[1]
  upr[i] = q[2]
}
tat_summary[, 
  `:=`(
    tat_med_lwr = ..lwr[enc_dept], 
    tat_med_upr = ..upr[enc_dept]
  )
]
tat_summary[, `:=`(
  Median = round(tat_med, 1),
  `95% CI` = sprintf('(%.1f-%.1f)', tat_med_lwr, tat_med_upr)
)]

p_tat_min_med = ggplot(
  tat_summary, 
  aes(y = Clinic, x = Median, `95% CI` = `95% CI`)
) + 
  geom_point() + 
  geom_errorbarh(aes(xmin = tat_med_lwr, xmax = tat_med_upr)) + 
  theme_bw() + 
  xlab('Median Turnaround Time (min done), hours') +
  ylab('')
#ggplotly(p_tat_med)

tat_min_pct = melt(
  tat_summary, 
  id.vars = c('Clinic', 'N'), 
  measure.vars = c('pct_72', 'pct_48')
)
tat_min_pct[, tat := 'min']

# TAT max done: ---------------------------------------------------------------

tat[, `:=`(
  tat_hr = as.numeric(tat_max - csn_start) / (60 * 60)
)]

# exclude messages not done and exclude those with negative times
tat_summary = tat[
  !is.na(tat_hr) & tat_hr > 0, 
  .(
    .N,
    pct_72 = mean(tat_hr <= 72) * 100,
    pct_48 = mean(tat_hr <= 48) * 100,
    tat_mean = mean(tat_hr),
    tat_med = median(tat_hr),
    tat_90 = quantile(tat_hr, 0.9),
    tat_mad = mean(abs(tat_hr - median(tat_hr)))
  ), 
  enc_dept
]
tat_summary[, `:=`(
  p_lwr = floor((N + 1) / 2 - qnorm(.975) * sqrt(N) / 2) / N,
  p_upr = ceiling((N + 1) / 2 + qnorm(.975) * sqrt(N) / 2) / N
)]

tat_ord = tat_summary[order(tat_med), enc_dept]
tat[, Clinic := factor(enc_dept, tat_ord)]
tat_summary[, Clinic := factor(enc_dept, tat_ord)]

p_tat_max_box = ggplot(
  tat[!is.na(tat_hr) & tat_hr > 0],
  aes(x = tat_hr, y = Clinic)
) +
  geom_boxplot(outlier.shape = NA) +
  xlim(c(0, 72)) +
  xlab('Turnaround Time (max done), hours') +
  theme_bw() +
  ylab('')

# CI for median
lwr = upr = vector('numeric', length = nrow(tat_summary))
names(lwr) = names(upr) = tat_summary[, enc_dept]
for ( i in 1:nrow(tat_summary) ) {
  cln = tat_summary[i, enc_dept]
  p = tat_summary[i, c(p_lwr, p_upr)]
  q = tat[
    enc_dept == cln & !is.na(tat_hr) & tat_hr > 0,
    quantile(tat_hr, p)
  ]
  lwr[i] = q[1]
  upr[i] = q[2]
}
tat_summary[, 
            `:=`(
              tat_med_lwr = ..lwr[enc_dept], 
              tat_med_upr = ..upr[enc_dept]
            )
]
tat_summary[, `:=`(
  Median = round(tat_med, 1),
  `95% CI` = sprintf('(%.1f-%.1f)', tat_med_lwr, tat_med_upr)
)]

p_tat_max_med = ggplot(
  tat_summary, 
  aes(y = Clinic, x = Median, `95% CI` = `95% CI`)
) + 
  geom_point() + 
  geom_errorbarh(aes(xmin = tat_med_lwr, xmax = tat_med_upr)) + 
  theme_bw() + 
  xlab('Median Turnaround Time (max done), hours') +
  ylab('')
#ggplotly(p_tat_med)

tat_max_pct = melt(
  tat_summary, 
  id.vars = c('Clinic', 'N'), 
  measure.vars = c('pct_72', 'pct_48')
)
tat_max_pct[, tat := 'max']

# pct done at target thresholds: ----------------------------------------------
tat_pct = rbind(tat_min_pct, tat_max_pct)
setnames(tat_pct, 'value', 'p')
tat_pct[, `:=`(
  lwr = p - qnorm(.975) * sqrt(p * (100 - p) / N),
  upr = p + qnorm(.975) * sqrt(p * (100 - p) / N)
)]
tat_pct[, ]

tat_pct[, `%` := round(p, 1)]
tat_pct[, `TAT` := ifelse(tat == 'min', 'First "Done"', 'Last "Done"')]
tat_pct[, Measure := ifelse(
  variable == 'pct_48', 
  '% done in 48 hours',
  '% done in 72 hours'
)]
cln_ord = tat_pct[variable == 'pct_48' & tat == 'max'][order(p), Clinic]
tat_pct[, Clinic := factor(as.character(Clinic), cln_ord)]
p_tat_pct = ggplot(
  tat_pct,
  aes(
    y = Clinic,
    x = `%`,
    color = TAT
  )
) +
  geom_point() +
  geom_errorbarh(aes(xmin = lwr, xmax = upr)) + 
  facet_wrap(~Measure, nrow = 1) + 
  theme_bw()  +
  ylab('')

