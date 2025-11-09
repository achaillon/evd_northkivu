
pal_classic=c('#5470c6', '#91cc75', '#fac858', '#ee6666', '#73c0de', '#3ba272', '#fc8452', '#9a60b4', '#ea7ccc')

box_fun <- function(data, y_axis, x_axis, colors = NULL, add_pvalue = TRUE, add_title=TRUE) {
        
        # Ensure columns are symbols for dplyr
        x_col <- ensym(x_axis)
        y_col <- ensym(y_axis)
        
        # Compute boxplot stats
        hc_stats <- data %>%
                group_by(!!x_col) %>%
                summarise(
                        low = min(!!y_col, na.rm = TRUE),
                        q1 = quantile(!!y_col, 0.25, na.rm = TRUE),
                        median = median(!!y_col, na.rm = TRUE),
                        q3 = quantile(!!y_col, 0.75, na.rm = TRUE),
                        high = max(!!y_col, na.rm = TRUE)
                ) %>%
                ungroup()
        
        # Default colors if not provided
        if(is.null(colors)) colors <- RColorBrewer::brewer.pal(n = nrow(hc_stats), name = "Set2")
        
        # Create boxplot data with fillColor and black border
        box_data <- pmap(list(hc_stats$low, hc_stats$q1, hc_stats$median, hc_stats$q3, hc_stats$high, colors),
                         function(low, q1, median, q3, high, color) {
                                 list(low = low, q1 = q1, median = median, q3 = q3, high = high,
                                      fillColor = color, color = "black")
                         })
        
        # Prepare jitter points
        points_data <- data %>%
                mutate(x = as.numeric(factor(!!x_col, levels = hc_stats[[as.character(x_col)]])) - 1) %>%
                select(x, y = !!y_col)
        
        # Base chart
        hc <- highchart() %>%
                hc_chart(type = "boxplot") %>%
                hc_add_series(
                        name = as.character(x_col),
                        data = box_data,
                        showInLegend = FALSE
                        
                ) %>%
                hc_add_series(
                        name = "Individual",
                        data = map2(points_data$x, points_data$y, ~list(x = .x + runif(1, -0.15, 0.15), y = .y)),
                        type = "scatter",
                        marker = list(symbol = "circle", radius = 4, fillColor = "white", lineWidth = 1, lineColor = "black"),
                        enableMouseTracking = TRUE,
                        showInLegend = FALSE
                ) %>%
                hc_xAxis(categories = hc_stats[[as.character(x_col)]]) %>%
                hc_yAxis(title = list(text = as.character(y_col)))
        
        if(add_title) {
                
                
                hc <- hc|>
                        hc_title(text = paste(as.character(y_col), "by", as.character(x_col)))
        }
        
        # Add pairwise p-values for >2 groups
        if(add_pvalue & nrow(hc_stats) > 1) {
                combos <- combn(hc_stats[[as.character(x_col)]], 2, simplify = FALSE)
                annotations <- list(labels = list())
                
                for(i in seq_along(combos)) {
                        g1 <- data %>% filter(!!x_col == combos[[i]][1]) %>% pull(!!y_col)
                        g2 <- data %>% filter(!!x_col == combos[[i]][2]) %>% pull(!!y_col)
                        pval <- t.test(g1, g2)$p.value
                        
                        # place annotation above the boxes
                        x_pos <- mean(match(combos[[i]], hc_stats[[as.character(x_col)]])) - 1
                        y_pos <- max(data[[as.character(y_col)]], na.rm = TRUE) + 0.5 + i*0.5
                        
                        annotations$labels[[i]] <- list(
                                point = list(x = x_pos, y = y_pos),
                                text = paste0(combos[[i]][1], " vs ", combos[[i]][2], ": p=", signif(pval, 3)),
                                style = list(color = "white", fontSize = "12px")
                        )
                }
                
                hc <- hc %>% hc_annotations(annotations)
        }
        
        return(hc)
}

setcol <- function(palette_name, numcol){
        mycolors <- colorRampPalette(palette_name)(numcol)
        return(mycolors)
}
# 0, Not related|1, Unlikely|2, Probably|3, Definitely|99, Unknown

theme_unhcr_void <- function(...) {
        theme_unhcr() +
                theme(#legend.position = "none",
                        axis.text.y = element_blank(),  # Remove axis text
                        axis.text.x = element_blank(),  # Remove axis text
                        axis.title.x = element_blank(),  # Remove axis titles
                        axis.title.y = element_blank(),  # Remove axis titles
                        axis.ticks = element_blank(),  # Remove axis ticks
                        panel.grid.major = element_blank(),  # Remove grid lines
                        
                        panel.grid.minor = element_blank(),  # Remove grid lines
                        panel.background = element_blank()  # Optional: remove panel background
                ) 
}
# Function to rename plotly traces based on patterns
rename_traces <- function(p, pattern_map) {
        for(i in seq_along(p$x$data)) {
                trace_name <- p$x$data[[i]]$name
                for(pattern in names(pattern_map)) {
                        if(grepl(pattern, trace_name)) {
                                p$x$data[[i]]$name <- pattern_map[[pattern]]
                                break
                        }
                }
        }
        p
}
to_attrib_num2char_label <- function(vec_in) {
        lbl <- attr(vec_in, "label", exact = TRUE)  # get label if it exists
        
        vec_out <- dplyr::case_when(
                vec_in %in% c("1") ~ "Unlikely",
                vec_in %in% c("0") ~ "Not related",
                vec_in %in% c("2") ~ "Probably",
                vec_in %in% c("3") ~ "Definitely",
                vec_in %in% c("99") ~ NA,
                .default = NA
        )
        
        # restore label
        attr(vec_out, "label") <- lbl
        return(vec_out)
}


plot_table_flexsurv_high <- function(flex_plot, table_df, trace_map = NULL,color_sel="#4575b4") {
        
        # Convert ggsurvflexplot to Plotly
        p_plot <- ggplotly(flex_plot$plot)
        
        # Apply trace renaming if provided
        if (!is.null(trace_map)) {
                p_plot <- rename_traces(p_plot, trace_map)
        }
        
        # Create Plotly table
        p_table <- plot_ly(
                type = "table",
                header = list(
                        values = names(table_df),
                        align = "center",
                        fill = list(color = color_sel),
                        font = list(color = "white", size = 12)
                ),
                cells = list(
                        values = t(table_df),
                        align = "center",
                        fill = list(color = c("white", "whitesmoke")),
                        font = list(size = 11)
                )
        )
        
        # Combine plot + table vertically
        htmltools::tagList(
                htmltools::div(
                        style="display:flex; flex-direction:column; gap:1rem;",
                        as_widget(p_plot),
                        as_widget(p_table)
                )
        )
}

plot_table_flexsurv <- function(flex_plot, table_df, trace_map = NULL, color_sel="#4575b4",
                                plot_height = "400px", table_height = "200px") {
        
        # Convert ggsurvflexplot to Plotly
        p_plot <- ggplotly(flex_plot$plot) %>% layout(height = as.numeric(sub("px","",plot_height)))
        
        # Apply trace renaming if provided
        if (!is.null(trace_map)) {
                p_plot <- rename_traces(p_plot, trace_map)
        }
        
        # Create Plotly table
        p_table <- plot_ly(
                type = "table",
                header = list(
                        values = names(table_df),
                        align = "center",
                        fill = list(color = color_sel),
                        font = list(color = "white", size = 12)
                ),
                cells = list(
                        values = t(table_df),
                        align = "center",
                        fill = list(color = c("white", "whitesmoke")),
                        font = list(size = 11)
                )
        ) %>% layout(height = as.numeric(sub("px","",table_height)))
        
        # Combine plot + table vertically
        htmltools::tagList(
                htmltools::div(
                        style=paste0("display:flex; flex-direction:column; gap:0.5rem;"),
                        htmltools::div(as_widget(p_plot), style=paste0("height:", plot_height,"; overflow:auto;")),
                        htmltools::div(as_widget(p_table), style=paste0("height:", table_height,"; overflow:auto;"))
                )
        )
}

# 0, Not present|1, Mild|2, Moderate|3, Severe|4, Life-threatening
to_scale_num2char_label <- function(vec_in) {
        lbl <- attr(vec_in, "label", exact = TRUE)  # get label if it exists
        
        vec_out <- dplyr::case_when(
                vec_in %in% c("1") ~ "Mild",
                vec_in %in% c("0") ~ "Not present",
                vec_in %in% c("2") ~ "Moderate",
                vec_in %in% c("3") ~ "Severe",
                vec_in %in% c("4") ~ "Life-threatening",
                vec_in %in% c("99") ~ NA,
                .default = NA
        )
        
        # restore label
        attr(vec_out, "label") <- lbl
        return(vec_out)
}



to_posneg_num2char <- function(vec_in){
        vec_out <- case_when(
                vec_in %in% c("1") ~ "Positive",
                vec_in %in% c("0") ~ "Negative",
                vec_in %in% c("2") ~ "Indeterminate",
                vec_in %in% c("99") ~ NA,
                .default = NA
        ) 
        return(vec_out)
}
to_posneg_num2char_label <- function(vec_in) {
        lbl <- attr(vec_in, "label", exact = TRUE)  # get label if it exists
        
        vec_out <- dplyr::case_when(
                vec_in %in% c("1") ~ "Positive",
                vec_in %in% c("0") ~ "Negative",
                vec_in %in% c("2") ~ "Indeterminate",
                vec_in %in% c("99") ~ NA,
                .default = NA
        )
        
        # restore label
        attr(vec_out, "label") <- lbl
        return(vec_out)
}


to_yn_num2char <- function(vec_in){
        vec_out <- case_when(
                vec_in %in% c("1") ~ "Yes",
                vec_in %in% c("2") ~ "No",
                vec_in %in% c("99") ~ NA,
                .default = NA
        ) 
        return(vec_out)
}
to_yn_num2char_label <- function(vec_in) {
        lbl <- attr(vec_in, "label", exact = TRUE)  # get label if it exists
        
        vec_out <- dplyr::case_when(
                vec_in %in% c("1") ~ "Yes",
                vec_in %in% c("2") ~ "No",
                vec_in %in% c("99") ~ NA,
                .default = NA
        )
        
        # restore label
        attr(vec_out, "label") <- lbl
        return(vec_out)
}

theme_gt_compact_color <- function(gt_table, color = "grey50") {
        gt_table %>%
                # Set smaller font size and compact row spacing
                gt::tab_options(
                        table.font.size = "small",
                        data_row.padding = gt::px(3),
                        heading.padding = gt::px(4),
                        summary_row.padding = gt::px(3),
                        row.striping.include_table_body = TRUE,
                        table_body.hlines.color = alpha(color,0.8),
                        row.striping.background_color = alpha(color,0.2)  # apply custom color here
                ) %>%
                # Apply compact styling to column labels
                gt::tab_style(
                        style = list(
                                gt::cell_text(size = "small", weight = "bold"),
                                gt::cell_borders(sides = "bottom", color = color, weight = gt::px(1))
                        ),
                        locations = gt::cells_column_labels()
                )
}



theme_gt_compact <- function(gt_table) {
        gt_table %>%
                # Set smaller font size and compact row spacing
                tab_options(
                        table.font.size = "small",
                        data_row.padding = px(3),  # Reduce row padding for compact view
                        heading.padding = px(4),   # Smaller padding for headers
                        summary_row.padding = px(3),
                        row.striping.include_table_body = TRUE,
                        table_body.hlines.color = "grey80"
                ) %>%
                # Apply compact styling to column labels
                tab_style(
                        style = list(
                                cell_text(size = "small", weight = "bold"),
                                cell_borders(sides = "bottom", color = "grey70", weight = px(1))
                        ),
                        locations = cells_column_labels()
                ) %>%
                # Add alternating row striping
                tab_options(
                        row.striping.background_color = "grey95"
                )
}
theme_gt_notcompact <- function(gt_table) {
        gt_table %>%
                # Set smaller font size and compact row spacing
                tab_options(
                        table.font.size = "large",
                        data_row.padding = px(2),  # Reduce row padding for compact view
                        heading.padding = px(3),   # Smaller padding for headers
                        summary_row.padding = px(2),
                        row.striping.include_table_body = TRUE,
                        table_body.hlines.color = "grey80"
                ) %>%
                # Apply compact styling to column labels
                tab_style(
                        style = list(
                                cell_text(size = "large", weight = "bold"),
                                cell_borders(sides = "bottom", color = "grey70", weight = px(1))
                        ),
                        locations = cells_column_labels()
                ) %>%
                # Add alternating row striping
                tab_options(
                        row.striping.background_color = "grey95"
                )
}
# Flexible age group function
bin_ages <- function(
                df,
                age_var,
                age_breaks = c(0, 5, 18, 25, 35, 50, Inf),
                age_labels = c("<5", "5-17", "18-24", "25-34", "35-49", "50+")
                # age_breaks = c(0, 1, 5, 12, 19, 65, Inf),
                # age_labels = c("<1", "1-5", "5-12", "12-19", "19-65", "65+")
) {
        dplyr::mutate(
                df,
                age_group = cut(
                        .data[[age_var]],
                        breaks = age_breaks,
                        labels = age_labels,
                        include.lowest = TRUE,
                        right = FALSE
                )
        )
}
bin_ages2 <- function(
                df,
                age_var,
                age_breaks = c(0, 5, 18, 50, Inf),
                age_labels = c("<5", "5-17", "18-49", "50+")
                # age_breaks = c(0, 1, 5, 12, 19, 65, Inf),
                # age_labels = c("<1", "1-5", "5-12", "12-19", "19-65", "65+")
) {
        dplyr::mutate(
                df,
                age_group2 = cut(
                        .data[[age_var]],
                        breaks = age_breaks,
                        labels = age_labels,
                        include.lowest = TRUE,
                        right = FALSE
                )
        )
}

bin_ages4 <- function(
                df,
                age_var,
                age_breaks = c(0, 5, Inf),
                age_labels = c("<5", "5-17")
                # age_breaks = c(0, 1, 5, 12, 19, 65, Inf),
                # age_labels = c("<1", "1-5", "5-12", "12-19", "19-65", "65+")
) {
        dplyr::mutate(
                df,
                age_group2 = cut(
                        .data[[age_var]],
                        breaks = age_breaks,
                        labels = age_labels,
                        include.lowest = TRUE,
                        right = FALSE
                )
        )
}

bin_ages3 <- function(
                df,
                age_var,
                age_breaks = c(0, 2, 5,12, 18, Inf),
                age_labels = c("<2", "2-5","5-12", "12-17",  "18+")
) {
        dplyr::mutate(
                df,
                age_group = cut(
                        .data[[age_var]],
                        breaks = age_breaks,
                        labels = age_labels,
                        include.lowest = TRUE,
                        right = FALSE
                )
        )
}


bin_ct <- function(
                df,
                ct_var,
                ct_breaks = c(0, 16, 18, 20, 22,24,26,28,30,32,34,36,38, Inf),
                ct_labels = c("<16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34", "34-36", "36-38", "38+")
                # age_breaks = c(0, 1, 5, 12, 19, 65, Inf),
                # age_labels = c("<1", "1-5", "5-12", "12-19", "19-65", "65+")
) {
        dplyr::mutate(
                df,
                ct_group = cut(
                        .data[[ct_var]],
                        breaks = ct_breaks,
                        labels = ct_labels,
                        include.lowest = TRUE,
                        right = FALSE
                )
        )
}
## CHEKBOX HEMOR
clean_checkbox_labels <- function(data, checkboxvars) {
        
        for (var in checkboxvars) {
                # Get question-level label
                q_label <- var_label(data[[var]])
                if (is.null(q_label)) q_label <- var
                
                # Select checkbox columns for this variable
                check_cols <- data %>%
                        dplyr::select(!contains("factor")) |>
                        dplyr::select(contains(var) & contains("___")) %>%
                        colnames()
                
                if (length(check_cols) == 0) next
                
                # Extract current labels
                oldlabels <- var_label(data[check_cols]) |> unlist()
                
                # Extract text between "choice=" and last ")"
                newlabels <- paste0(q_label, " ", str_extract(oldlabels, "(?<=choice=).*?(?=\\)$)"))
                
                # Name the vector with column names — required by sjlabelled
                names(newlabels) <- check_cols
                
                # Assign back
                var_label(data[check_cols]) <- newlabels
        }
        
        return(data)
}

hex_to_rgba <- function(hex, alpha = 1) {
        rgb <- col2rgb(hex) / 255
        sprintf("rgba(%d,%d,%d,%.2f)", rgb[1]*255, rgb[2]*255, rgb[3]*255, alpha)
}



plot_map_lgd_fun_norender_who3<- function(data_in,zoom_level){
        tilesURL1<-"https://tiles.arcgis.com/tiles/5T5nSi527N4F7luB/arcgis/rest/services/WHO_Polygon_Raster_Basemap/MapServer/tile/{z}/{y}/{x}" # no names
        tilesURL2<-"https://tiles.arcgis.com/tiles/5T5nSi527N4F7luB/arcgis/rest/services/WHO_Polygon_Raster_Disputed_Areas_and_Borders/MapServer/tile/{z}/{y}/{x}"
        tilesURL3<-"https://tiles.arcgis.com/tiles/5T5nSi527N4F7luB/arcgis/rest/services/WHO_Point_Raster_Basemap/MapServer/tile/{z}/{y}/{x}" # no names
        tilesURL4<- "https://tiles.arcgis.com/tiles/5T5nSi527N4F7luB/arcgis/rest/services/WHO_Polygon_Basemap_Disputed_Areas_and_Borders_VTP/VectorTileServer"
        mylon<- mean(countryref[countryref$adm1_code=="COD",]$centroid.lon)
        mylat<- mean(countryref[countryref$adm1_code=="COD",]$centroid.lat)
        # mylon<-28.7500 # mean(countryref[countryref$adm1_code=="COD",]$centroid.lon)
        # mylat<- -0.6660 
        # contributors_range <- range(c(data_in$contributors), na.rm = TRUE) 
        # contributors_colors <- c("white",  "#a5c5ff", "#8badfe", "#7396ee", "#5b82d8", "#436ec1", "#265bab")
        # bins_contributors <- c(0, seq(1, to = contributors_range[2], length = length(contributors_colors)))
        # cv_pal_contributors <- colorBin(palette = contributors_colors, domain = data_in$contributors, bins = bins_contributors, na.color = "transparent")
        
        myzoom=zoom_level
        
        case_range <- range(c(data_in$Cases), na.rm = TRUE) 
        cases_colors <- c("white", "#bfddff","#FFFFBF", "#FEE08B","#FDAE61","#F46D43","#D53E4F")
        bins_case <- c(0, seq(1, to = case_range[2], length = length(cases_colors)))
        cv_pal_case <- colorBin(palette = cases_colors, domain = c(data_in$Cases), bins = bins_case, na.color = "transparent")
        
        
        death_range <- range(c(data_in$Death), na.rm = TRUE) 
        death_colors <- c("white", "#F8B195", "#F67280","#b53737", "#C06C84", "#6C5B7B", "#355C7D")
        bins_death <- c(0,seq(1,to=death_range[2],length=length(death_colors)))
        cv_pal_death <- colorBin(palette = death_colors, domain = data_in$Death, bins = bins_death, na.color = "transparent")
        
        leaflet(data_in,
                options = leaflet::leafletOptions(attributionControl=T, maxZoom = 9, zoomSnap = 0.1, zoomDelta = 0.1)) %>% 
                addMapPane("tiles1", zIndex = 410) %>%  # Level 1: bottom
                addMapPane("tiles2", zIndex = 430) %>%          # Level 3: top
                addMapPane("tiles3", zIndex = 440) %>%          # Level 4:
                addMapPane("tiles4", zIndex = 450) %>%          # Level 4:
                addMapPane("tiles5", zIndex = 460) %>%
                addMapPane("polygons", zIndex = 420) %>%  
                addMapPane("markers_pane", zIndex = 470) %>%
                # leaflet::addTiles(tilesURL1,options  = tileOptions(opacity=1,weight = 1,pane = "tiles1", noWrap = F))|>
                addTiles(urlTemplate = "", attribution = "The designations employed and the presentation of the material in this publication do not imply the expression of any opinion whatsoever on the part of WHO concerning the legal status of any country, territory, city or area or of its authorities, or concerning the delimitation of its frontiers or boundaries. Dotted and dashed lines on maps represent approximate border lines for which there may not yet be full agreement.",
                         options  = tileOptions(pane = "tiles1", noWrap = T))|>
                # leaflet::addTiles(tilesURL2,options = tileOptions(transparent=F,weight = 1,color = "#9c9c9c",pane = "tiles3", noWrap = F) ) |>
                # addPolylines(data = spatialborder,dashArray = (1),color = "white",weight=1, options = pathOptions(pane = "tiles4"))|>
                # addPolylines(data = spatialborder,dashArray = (3),weight=1, color = "#9c9c9c",options = pathOptions(pane = "tiles5"))|>
                setView(lng = mylon, lat = mylat, zoom = myzoom)|>  # Adjust lng, lat, and zoom as needed
                addTiles(options  = tileOptions(opacity=1,weight = 1,pane = "tiles1", noWrap = F)) %>%
                # leaflet::fitBounds(~-100,-60,~60,70) %>%
                # addProviderTiles(providers$Stadia.StamenTerrain) |>
                # addProviderTiles(providers$CyclOSM) |>
                # addProviderTiles(providers$Esri.NatGeoWorldMap) |>
                # addProviderTiles(providers$Esri.WorldStreetMap) |>
                # addProviderTiles(providers$OpenStreetMap.Mapnik)|>
                addLayersControl(
                        position = "topright",
                        # baseGroups = c("Cases","Contributors"),
                        baseGroups = c("Cases","Deaths"),
                        
                        options = layersControlOptions(collapsed = FALSE)
                ) %>%
                hideGroup("Deaths") %>%
                addScaleBar(
                        position = c("topleft"),
                        options = scaleBarOptions(
                                maxWidth = 100,
                                metric = TRUE,
                                imperial = TRUE,
                                updateWhenIdle = TRUE)
                ) %>%
                
                addPolygons(
                        fillColor = ~cv_pal_case(Cases),
                        color = "#636363",
                        weight = 1,
                        opacity = 1,
                        fillOpacity = 0.8,
                        group = "Cases",
                        # popup = ~paste(adm2_viz_name, ": ", cases, " case(s)"),
                        popup = ~sprintf(  "<div style='font-size:14px; color:#61a0a8; font-family:Arial, sans-serif;'>
                                           <strong>%s </strong><br/>Case(s): %g<br/></div>", adm1_viz_name, Cases) %>% lapply(htmltools::HTML),
                        
                        options = pathOptions(pane = "polygons")
                ) |>
                addPolygons(
                        fillColor = ~cv_pal_death(Death),
                        color = "#636363",
                        weight = 1,
                        opacity = 1,
                        fillOpacity = 0.8,
                        group = "Deaths",
                        # popup = ~paste(adm2_viz_name, ": ", cases, " case(s)"),
                        popup = ~sprintf(  "<div style='font-size:14px; color:#61a0a8; font-family:Arial, sans-serif;'>
                                           <strong>%s </strong><br/>Death(s): %g<br/></div>", adm1_viz_name, Death) %>% lapply(htmltools::HTML),
                        
                        options = pathOptions(pane = "polygons")
                ) |>
                
                # Add legends, initially hidden
                addLegend("bottomright", 
                          pal = cv_pal_case, 
                          values = ~Cases, 
                          title = "Reported cases", 
                          opacity = 1, 
                          group = "Cases",
                          labFormat = labelFormat(digits = 0))|>
                addLegend("bottomright",
                          pal = cv_pal_death,
                          values = ~Death,
                          title = "Deaths",
                          opacity = 1,
                          group = "Deaths",
                          labFormat = labelFormat(digits = 0))
        
}

