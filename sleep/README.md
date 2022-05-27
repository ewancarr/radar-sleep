---
title: Summary of derived sleep measures
author: Ewan Carr
date: 2022-05-26
---

# Things to be aware of

* RADAR surveys take place every 3 months (0, 3, 6, ..., 24).
* We measure sleep in the 4 weeks prior to each survey.
* We're using daily sleep measures covering a 24 hour period.
* We summarise the daily sleep measures (e.g., median, variance) for each 4 week period.
* To be included in a given 4 week period, participants must have provided FitBit sleep data on **at least 7 weekdays** (non-consecutive) during the 4 weeks.
* Most measures are summarised for **weekdays only**.
* Some participants (around 10%) have more than one 'sleep event' per day. We are selecting the single longest sleep event (i.e., based on total sleep time). Other sleep events on that day are discarded. 
* Most measures describe sleep in the preceding 4 weeks. Some describe the *change* in sleep between the (i) the past 4 weeks; (ii) the 4 weeks preceding the previous 3-monthly survey:

```
         Last                               This    
        survey                             survey   
        (t-3)                                (t)    
                                                    
┌─────────┐║                       ┌─────────┐║     
│  Sleep  │║                       │  Sleep  │║     
│ Last 4w │║ ───────────────────▶  │ Last 4w │║     
└─────────┘║                       └─────────┘║     
     │                                  ▲           
     │                                  │           
     │                                  │           
     └───────── Change in sleep ────────┘           

```

# Sleep measures

A = Past 4 weeks

B = Change, past 4 weeks (this survey period) versus past 4 weeks (last survey period). 

| Variable       | Label                             | Comment                                                      | When |
| -------------- | --------------------------------- | ------------------------------------------------------------ | ---- |
| `tst_med`      | Total sleep time, median          | Weekdays only.                                               | A    |
| `tst_var`      | Total sleep time, variance        | Weekdays only.                                               | A    |
| `slp_eff_med`  | Sleep efficiency, median          | Percentage of total sleep time to time in bed. Weekdays only. | A    |
| `sol_med`      | Sleep onset latency, median       | Time between last FitBit step and 'sleep onset'. Weekdays only. | A    |
| `sfi_med`      | Sleep fragmentation index, median | Number of awakenings per hour sleep. Weekdays only.          | A    |
| `hysom_ever`   | Any days sleeping 10+ hours       | Weekdays only.                                               | A    |
| `cm3_son_med`  | Sleep onset, change in median     | **Positive** values indicate sleep onset is **later** this survey period, compared to last. Weekdays only. | B    |
| `cm3_soff_med` | Sleep offset, change in median    | As above.                                                    | B    |
| `son_rel_var`  | Relative sleep onset, variance    | Variance around participant's median sleep onset, past 4 weeks. Weekdays only. | A    |
| `soff_rel_var` | Relative sleep offset, variance.  | As above. Weekdays only.                                     | A    |
| `smid_med`     | Sleep midpoint, median            | Clock time. Weekdays only.                                   | A    |
| `smid_var`     | Sleep midpoint, variance          | Variance around participant's median sleep midpoint, past 4 weeks. Weekdays only. | A    |
| `sjl`          | Social jet lag                    | Absolute value of the difference in the midpoint of sleep times between weekdays and weekends. | A    |

# Details

Most of the above measures are taken directly from WP8 or Dan (`dailyFeatures_fitbit_sleep.csv`) -- I've just summarised for the relevant 4-week periods.

The exception is **sleep onset latency**. We've calculated this by:

1. For each participant:
   1. For each weekday in each 4 week period where they provide sleep data: 
      1. Extract their FitBit steps data (from `connect_fitbit_intraday_steps`)[^path].
      2. Identify the time of the last step immediately preceding 'sleep onset'.
      3. Calculate sleep onset latency as the period between 'last FitBit step' and 'sleep onset'.

[^path]: `RADAR-CNS/RADAR-P/MDD/connect_fitbit_intraday_steps` on Dan's Synology server.

