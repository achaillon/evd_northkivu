

# ---- Package loading (quiet & deduplicated) ----
suppressPackageStartupMessages({
        suppressWarnings({
                pacman::p_load(
                        # Visualization
                        ggplot2, ggpubr, geomtextpath, viridis, grid,gridExtra, ggthemes,
                        echarty, plotly, highcharter, leaflet, leaflet.extras, leaflet.minicharts,
                        unhcrthemes, forestploter, gt, gtExtras, gtforester,
                        
                        # Tables and summaries
                        kableExtra, reactablefmtr, formattable, compareGroups, gtsummary, DT,ggtext,
                        
                        # Diagrammes
                        DiagrammeR,DiagrammeRsvg,
                        
                        # Fonts and icons
                        extrafont, emojifont, fontawesome, bsicons,htmlwidgets,
                        
                        # Data handling and cleaning
                        tidyverse, tidyr, dplyr, purrr, stringr, lubridate, scales, here,
                        readr, haven, Hmisc, CoordinateCleaner, reshape2,glue,
                        
                        # Modeling and stats
                        rstatix, lme4, lmerTest, broom.mixed, survival, survminer,
                        flexsurv, tidymodels, glmnet, ranger, broom, combinat,pROC,ROCR,
                        
                        # Misc utilities
                        htmltools, httr, bslib, rlang, stats
                )
        })
})


