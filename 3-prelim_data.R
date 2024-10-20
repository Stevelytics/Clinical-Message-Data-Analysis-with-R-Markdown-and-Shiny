#' ---
#' title: PARADIGM-Hadeed Preliminary Data
#' date: "`r format.Date(Sys.Date(), '%B %d, %Y')`"
#' author: James Henderson, PhD
#' output:
#'   flexdashboard::flex_dashboard:
#'     orientation: rows
#'     source_code: embed
#'     vertical layout: scroll
#' ---

#+ r setup, include=FALSE
source('1-ibx_prelim.R', local = TRUE)
source('2-time_in_ibx.R', local = TRUE)

#' About {.sidebar data-width=300}
#' ============================================================================
#' 
#' ### Navigation
#' Use the tabs at the top of the page to select a reporting topic.
#' Hover over plots for additional detail. 
#' 
#' ### Notes
#' #### cFTE data
#'  - cFTE data is sourced from this [dashboard][1]
#'     + Only using *physician* cFTE for clinics, APPs are not included.
#'     + Resident time is not included, but physician cFTE on Taubman GMO is.
#'  - This is the slowest to update data source, currently limiting us to data 
#'    through November 2022.
#'      

#' #### In Basket Messages Data
#'  - In Basket message data are sourced from the [UMMG dashboard][2]
#'  - Taubman messages with "TC GEN MED RESIDENT" as encounter department are
#'     *not* included except where explicilty referred to.
#'  - Data from December 2021 - November 2022 are included.
#'  - Available from June 2021 w/ little effort, longer back with some effort. 
#'  
#'  [1]: https://tab.med.umich.edu/#/views/PatientCounts/PanelcFTETrend?:iid=1
#'  [2]: https://tab.med.umich.edu/#/views/InBasket_16219755160520/Volume?:iid=1

#' #### Time in In Basket
#' - Time in In Basket is sourced from Epic Signal.
#' - Currently using December 2021 - November 2022  
#' - Available from January 2021 - December 2022. 
#'   

#' In Basket Message Volume
#' ============================================================================
#' 
#' Row {data-height=900}
#' ----------------------------------------------------------------------------
#' ### Total Message Volume
ggplotly(p_n)
nn = n_tot[Type == 'Patient Medical Advice Request'] %>% 
  .[order(-Clinic)] %>% 
  .[c(1, grep('TC', Clinic)), `Avg Messages`]
n_lwr_str = sprintf('%.0f%% (%.1f/%.1f)', 100 * nn[2] / nn[1], nn[2], nn[1])

#' ### Physician Message Volume
ggplotly(p_n_phys + theme(legend.position = 'bottom'))

#' 
#' Row {data-height=450}
#' ----------------------------------------------------------------------------
#' ### Key Points
#' + Taubman gets the $2^{nd}$ *lowest* total volume of portal messages per
#'   cFTE. 
#' + Relative to the clinic with highest message volume, 
#'   Taubman gets only `r n_lwr_str` as many messages.  
#' + However, Taubman is $2^{nd}$ only to Northville in *physician*
#'    message volume. 
#' + See the [next tab](#physician-involvement) for the physician touch rate. 
#' 
#' ### Additional Notes
#' + Total Message Volume (above left) is based on the number of unique
#'   message ids, and should therefore represent messages *sent*, and not, 
#'   e.g., count messages with multiple recipients once per recipient. 
#' + Physician Message Volume (above right) is based on unique message ids with
#'   a physician as the (a) *recipient*. 
#' + For messages *sent by physicians* see the [Message Sent](#messages-sent)
#'   tab.  

#' 
#' Physician Involvement
#' ============================================================================
#'
#' Row {data-height=900}
#' ----------------------------------------------------------------------------
#' ### Physician Touch Rate
ggplotly(p_ptr)

#' ### "Carbon Copy" Behavior
ggplotly(p_msg_pct)

#' Row {data-height=900}
#' ----------------------------------------------------------------------------
#' 
#' ### Additional Notes
#' + *Physician Touch Rate* (above left) summarizes the percentage of message
#'   *encounters* with one or more messages with a physician *recipient*. 
#' + *"Carbon Copy" Behavior* (above right) classifies each message id based on
#'   the role(s) of its recipient(s) and summarizes these classifications. 
#' + Residents are classified as "non-physicians" in the "Carbon Copy" Behavior
#'   figure .
#' + *"Carbon Copy* by Sender Role* (right) summarizes the the breakdown among
#'    non-physician roles who sent CC'd messages with a physician recipient.  
#'   

#' ### "Carbon Copy" by Sender Role
ggplotly(p_cc_role)

#' Messages Sent
#' ============================================================================
#' 
#' Row {data-height=900}
#' ----------------------------------------------------------------------------
#' ### Physician **sent** message volume
ggplotly(p_n_phys_sent)

#' ### % Encounters with message(s) sent by physician
ggplotly(p_psr)

#' Row 
#' ----------------------------------------------------------------------------
#' ### Key Points. 
#' + Taubman physicians **send** the fewest messages per cFTE for both 
#'   portal and call encounters (above left). 
#' + Taubman physicians also send 1+ messages on the smallest percent of
#'   portal encounters (above right).  
#' 
#' ### Additional Notes
#' + None.   
#' 

#' Turnaround Time
#' ============================================================================
#' 
#' Row
#' ----------------------------------------------------------------------------
#' ### Median time to first marked "done"
#' 
ggplotly(p_tat_min_med)

#' ### Median time to last marked "done"
ggplotly(p_tat_max_med)

#' Row
#' ----------------------------------------------------------------------------
#' ### % done in 48/72 hours
ggplotly(p_tat_pct)

#' ### Notes
#' - Based on encounters with first message within the prior year
#' - Encounters not yet marked "done" are excluded.
#' - A small number (~0.3%) of encounters with negative turn-around times are
#'   excluded. These most likely represent cases where the first message in the
#'   encounter is outside the scope of the data pull. 
#'   

#' Time in In Basket
#' ============================================================================
#' 
#' Row
#' ----------------------------------------------------------------------------
#' ### Time in In Basket
ggplotly(p1)

#' ### Patient Medical Advice Request
ggplotly(p2)

#' Row
#' ----------------------------------------------------------------------------
#' ### Send Patient Message
ggplotly(p4)

#' ### Patient Phone Calls
ggplotly(p3)
