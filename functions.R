# Helper Functions for Pollen App

# Authentication function
# Checks if the provided username and password match a record in the database
authenticate_user <- function(username, password) {
  # Create a query string to find the user in the database
  query <- sprintf('{"username": "%s", "password": "%s"}', username, password)
  # Execute the query and get the result
  result <- mongo_conn$find(query)
  # Return TRUE if a matching record is found, FALSE otherwise
  return(nrow(result) > 0)
}

# Geocoding function
# Converts a place name to geographic coordinates using the Nominatim API
geocode <- function(place_name) {
  # Construct the URL for the Nominatim API request
  url <- paste0("https://nominatim.openstreetmap.org/search?q=", 
                URLencode(place_name), 
                "&format=json&limit=1")
  
  # Make the API request with error handling
  response <- tryCatch({
    GET(url, add_headers("User-Agent" = "RStudio/MyApp"))
  }, error = function(e) {
    # Show an error modal if the request fails
    showModal(modalDialog(
      title = "Error",
      "Failed to reach the geocoding service. Please try again.",
      easyClose = TRUE,
      footer = NULL
    ))
    return(NULL)
  })
  
  # Return NULL if the response is NULL
  if (is.null(response)) return(NULL)
  
  # Parse the JSON content of the response
  content <- content(response, "text")
  result <- fromJSON(content)
  
  # Show an error modal if no results are found
  if(length(result) == 0) {
    showModal(modalDialog(
      title = "Location Not Found",
      "Please try again.",
      easyClose = TRUE,
      footer = NULL
    ))
    return(NULL)
  }
  
  # Return the latitude and longitude of the first result
  return(list(lat = as.numeric(result$lat[1]), lon = as.numeric(result$lon[1])))
}

# Pollen data retrieval function
# Fetches pollen data for a given location and date from the Google Pollen API

# Create a cache function with a 1-hour expiry
library(memoise)
cache <- memoise::memoise(function(...) get_pollen_data_impl(...), ~memoise::timeout(3600))

get_pollen_data <- function(place_name, day_offset = 0) {
  tryCatch({
    cache(place_name, day_offset)
  }, error = function(e) {
    warning(paste("Error in get_pollen_data:", e$message))
    NULL
  })
}

get_pollen_data_impl <- function(place_name, day_offset = 0) {
  # Load pollen_api_key from environment
  pollen_api_key <- Sys.getenv("POLLEN_API_KEY")
  
  # Check if API key is available
  if (pollen_api_key == "") {
    stop("POLLEN_API_KEY environment variable is not set")
  }
  
  # Convert place name to geographic coordinates
  coords <- tryCatch({
    geocode(place_name)
  }, error = function(e) {
    stop(paste("Geocoding failed:", e$message))
  })
  
  if (is.null(coords)) {
    stop("Failed to get coordinates for the given place name")
  }
  
  # Validate the day_offset parameter
  if (day_offset < 0 || day_offset > 4) {
    stop("day_offset must be between 0 and 4.")
  }
  
  # Construct the API URL with necessary parameters
  url <- paste0(
    "https://pollen.googleapis.com/v1/forecast:lookup?key=", pollen_api_key,
    "&location.latitude=", coords$lat,
    "&location.longitude=", coords$lon,
    "&days=5"  # Request 5 days of forecast data
  )
  
  # Make the API request with error handling
  res <- tryCatch({
    GET(url)
  }, error = function(e) {
    stop(paste("API request failed:", e$message))
  })
  
  # Check for HTTP errors
  if (http_error(res)) {
    stop(paste("HTTP error:", http_status(res)$message))
  }
  
  content <- content(res, as = "text")
  data <- fromJSON(content, flatten = TRUE)
  
  # Define expected columns for pollen and plant data
  pollen_expected_cols <- c("code", "indexInfo.value", "indexInfo.category", "inSeason", "healthRecommendations")
  plant_expected_cols <- c("displayName", "indexInfo.value", "indexInfo.displayName", "indexInfo.category", "inSeason", 
                           "plantDescription.type", "plantDescription.family", "plantDescription.season", 
                           "plantDescription.specialColors", "plantDescription.specialShapes", 
                           "plantDescription.crossReaction")
  
  # Helper function to safely extract data and handle missing columns
  safe_get_data <- function(data_list, index, expected_cols) {
    if (length(data_list) > index) {
      df <- as.data.frame(data_list[[index + 1]], stringsAsFactors = FALSE)
      
      # Add missing columns with NA values
      for (col in expected_cols) {
        if (!col %in% names(df)) {
          df[[col]] <- NA
        }
      }
      
      return(df)
    } else {
      # Return empty dataframe with expected columns if data is missing
      return(data.frame(matrix(ncol = length(expected_cols), nrow = 0, 
                               dimnames = list(NULL, expected_cols))))
    }
  }
  
  # Extract pollen and plant data for the specified day
  pollen_data <- safe_get_data(data$dailyInfo$pollenTypeInfo, day_offset, pollen_expected_cols)
  plant_data <- safe_get_data(data$dailyInfo$plantInfo, day_offset, plant_expected_cols)
  
  # Ensure all required pollen types are present
  required_types <- c("GRASS", "TREE", "WEED")
  existing_types <- pollen_data$code
  missing_types <- setdiff(required_types, existing_types)
  
  # Add missing pollen types with NA values
  if (length(missing_types) > 0) {
    for (type in missing_types) {
      new_row <- data.frame(matrix(NA, nrow = 1, ncol = ncol(pollen_data), 
                                   dimnames = list(NULL, names(pollen_data))))
      new_row$code <- type
      pollen_data <- rbind(pollen_data, new_row)
    }
  }
  
  # Create a dataframe with pollen type information
  type_df <- data.frame(
    type = pollen_data$code,
    level = pollen_data$indexInfo.value,
    category = pollen_data$indexInfo.category,
    in_season = pollen_data$inSeason,
    health_recommendations = ifelse(
      is.null(pollen_data$healthRecommendations) | 
        is.na(pollen_data$healthRecommendations) | 
        pollen_data$healthRecommendations == "", 
      "No health recommendations available.", 
      sapply(pollen_data$healthRecommendations, function(x) paste(x, collapse = " "))
    )
  )
  
  # Create a dataframe with plant information
  plant_df <- data.frame(
    plant_name = plant_data$displayName,
    plant_level = plant_data$indexInfo.value,
    index_info = plant_data$indexInfo.displayName,
    plant_category = plant_data$indexInfo.category,
    season = plant_data$inSeason,
    plant_type = plant_data$plantDescription.type,
    plant_family = plant_data$plantDescription.family,
    plant_season = plant_data$plantDescription.season,
    plant_special_colors = sapply(plant_data$plantDescription.specialColors, 
                                  function(x) ifelse(is.null(x), NA, paste(x, collapse = ", "))),
    plant_special_shapes = sapply(plant_data$plantDescription.specialShapes, 
                                  function(x) ifelse(is.null(x), NA, paste(x, collapse = ", "))),
    plant_cross_reaction = sapply(plant_data$plantDescription.crossReaction, 
                                  function(x) ifelse(is.null(x), NA, paste(x, collapse = ", ")))
  )
  
  # Combine all data into a list for return
  result_list <- list(
    type_data = type_df,
    plant_data = plant_df,
    location = data.frame(
      lat = coords$lat,
      lon = coords$lon,
      place = place_name
    )
  )
  
  return(result_list)
}

# Function for making HTML gauge
# Creates an HTML representation of a gauge for displaying pollen levels
gauge_html <- function(level, category, in_season) {
  # Handle cases where level or category is NA
  if (is.na(level) || is.na(category)) {
    level <- NA
    category <- "Missing Data"
    color <- "white"  # Default color for unknown category
    display_level <- "Missing Data"
  } else {
    # Assign colors based on pollen category
    color <- switch(category,
                    "Very Low" = "#727763",
                    "Low" = "#A6AC8A",
                    "Moderate" = "#d9bba8",
                    "High" = "#B66C48",
                    "Very High" = "#8C421F",
                    "white"  # Default color if category does not match
    )
    display_level <- sprintf("%d/5", level)
  }
  
  # Determine if the pollen is in season
  in_season_text <- ifelse(is.na(in_season), "Missing Data", ifelse(in_season, "Yes", "No"))
  
  # Calculate the angle for the gauge based on the pollen level
  angle <- ifelse(is.na(level), 0, (level / 5) * 300)  
  
  # Generate the HTML for the gauge
  sprintf('
    <div id="circular-barplot-container" style="width: 150px; height: 180px; margin: auto; position: relative;">
      <!-- Circular barplot -->
      <div style="width: 100%%; height: 150px; position: relative;">
        <div style="width: 100%%; height: 100%%; border-radius: 50%%; position: absolute; top: 0; left: 0; 
             background: conic-gradient(from 210deg, %s 0deg %ddeg, #f5f5f5 %ddeg 300deg, transparent 300deg 360deg);">
        </div>
        <!-- Inner white cover -->
        <div style="width: 90%%; height: 90%%; background-color: #EAE3D3; border-radius: 50%%; 
             position: absolute; top: 5%%; left: 5%%;">
        </div>
        <!-- Text inside the gauge -->
        <div style="width: 100%%; height: 100%%; position: absolute; top: 0; left: 0; 
             display: flex; align-items: center; justify-content: center; font-size: 24px; color: #777777; font-weight: bold;">
          %s
        </div>
      </div>
      <!-- Text Information below the gauge -->
      <div style="text-align: center; color: #777777; font-size: 13px;">
        <span style="color: %s;"><strong>%s</strong></span> Pollen<br>
        In Season: %s
      </div>
    </div>
  ', color, angle, angle,
          ifelse(is.na(level),
                 '<span style="font-size: 12px;">Missing Data</span>',
                 sprintf('<span style="color: %s; font-size: 28px;"><strong>%d</strong></span><span style="font-size: 15px; opacity: 0.4; font-weight: normal;">/5</span>', color, level)
          ),
          color, category, in_season_text)
}

# Function for making leaflet popup
# Creates the content for a leaflet popup showing pollen information
create_popup <- function(place, type_data, selected_type) {
  # Filter the data for the selected pollen type
  selected_info <- type_data %>% filter(type == selected_type)
  
  # If no data is available for the selected type, use placeholder values
  if (nrow(selected_info) == 0) {
    selected_info <- data.frame(
      level = "Missing Data",
      category = "Missing Data",
      in_season = "Missing Data"
    )
  }
  
  # Generate the HTML content for the popup
  paste0(
    "<div style='text-align: center;'>",
    "<span style='font-size: 14px; color: #777777;'>Universal Pollen Index</span><br>",
    "<span style='font-size: 14px; color: #777777;'>for </span>",  
    "<span style='font-size: 20px; color: #777777;'>", place, "</span><br>",  
    "</div>",
    gauge_html(selected_info$level, selected_info$category, selected_info$in_season),
    "<div style='text-align: center; margin-top: 10px;'>",
    "<div style='display: flex; justify-content: center;'>",  
    "<button onclick='Shiny.setInputValue(\"popup_button\", \"GRASS\", {priority: \"event\"})' class='btn btn-default' style='border-radius: 4px; padding: 5px 10px; border: 1px solid #ccc; background-color: ", ifelse(selected_type == 'GRASS', '#A6AC8A', 'transparent'), "; color: ", ifelse(selected_type == 'GRASS', 'white', '#777777'), "; font-size: 12px; cursor: pointer;'>GRASS</button> ",
    "<button onclick='Shiny.setInputValue(\"popup_button\", \"WEED\", {priority: \"event\"})' class='btn btn-default' style='border-radius: 4px; padding: 5px 10px; border: 1px solid #ccc; background-color: ", ifelse(selected_type == 'WEED', '#A6AC8A', 'transparent'), "; color: ", ifelse(selected_type == 'WEED', 'white', '#777777'), "; font-size: 12px; cursor: pointer;'>WEED</button> ",
    "<button onclick='Shiny.setInputValue(\"popup_button\", \"TREE\", {priority: \"event\"})' class='btn btn-default' style='border-radius: 4px; padding: 5px 10px; border: 1px solid #ccc; background-color: ", ifelse(selected_type == 'TREE', '#A6AC8A', 'transparent'), "; color: ", ifelse(selected_type == 'TREE', 'white', '#777777'), "; font-size: 12px; cursor: pointer;'>TREE</button>",
    "</div>",
    "</div>"
  )
}

# Function for resetting pollen log data
# Creates an empty dataframe with the correct structure for the pollen log
reset_pollen_log_data <- function() {
  data.frame(
    Date = as.Date(character()),
    Location = character(),
    Grass_Level = numeric(),
    Weed_Level = numeric(),
    Tree_Level = numeric(),
    Symptoms = character(),
    Symptoms_Severity = numeric(),
    stringsAsFactors = FALSE
  )
}

# Function for updating map
# Updates the leaflet map with new pollen data
update_map <- function(data, selected_type) {
  if (!is.null(data)) {
    # Create the popup content
    popup_content <- create_popup(data$location$place, data$type_data, selected_type)
    
    # Update the leaflet map
    leafletProxy("pollenMap") %>%
      clearMarkers() %>%
      clearPopups() %>%
      setView(lng = data$location$lon, lat = data$location$lat, zoom = 8) %>% 
      addCircleMarkers(lng = data$location$lon, lat = data$location$lat, color = "#A6AC8A") %>% 
      addPopups(lng = data$location$lon, lat = data$location$lat,
                popup = popup_content,
                options = popupOptions(
                  closeButton = FALSE,
                  closeOnClick = FALSE,
                  autoPan = FALSE  # Prevents automatic panning
                )) %>%
      # Center both the location and the popup
      htmlwidgets::onRender("
        function(el, x) {
          var map = this;
          map.on('popupopen', function(e) {
            var px = map.project(e.target._latlng); // Use marker's latlng
            px.y -= e.popup._container.clientHeight/2;
            map.panTo(map.unproject(px), {animate: true});
          });
        }
      ")
  }
}

# Function for updating popups
# Updates only the popup content on the map without changing the view
update_popups <- function(data, selected_type) {
  if (!is.null(data)) {
    # Create the new popup content
    popup_content <- create_popup(data$location$place, data$type_data, selected_type)
    
    # Update only the popup on the map
    leafletProxy("pollenMap") %>%
      clearPopups() %>%
      addPopups(lng = data$location$lon, lat = data$location$lat,
                popup = popup_content,
                options = popupOptions(closeButton = FALSE, closeOnClick = FALSE))
  }
}

# Function for updating info panel
# Updates the health recommendations and plant information panels
update_info_panel <- function(data, selected_type) {
  if (!is.null(data)) {
    # Filter data for the selected pollen type
    type_data <- data$type_data[data$type_data$type == selected_type, ]
    
    # Get health recommendations
    health_recommendations <- type_data$health_recommendations
    if (!is.null(health_recommendations) && !any(is.na(health_recommendations)) && health_recommendations != "") {
      health_text <- health_recommendations
    } else {
      health_text <- "No health recommendations available."
    }
    
    # Get plant information
    plant_data <- data$plant_data[data$plant_data$plant_type == selected_type & !is.na(data$plant_data$plant_type), ]
    if (nrow(plant_data) > 0) {
      plant_info <- apply(plant_data, 1, function(row) {
        paste(
          paste0("<strong>Plant Name:</strong> ", ifelse(is.na(row["plant_name"]), "Not available", row["plant_name"]), ";"),
          paste0("<strong>In Season:</strong> ", ifelse(is.na(row["season"]), "Not available", ifelse(row["season"], "Yes", "No")), ";"),
          paste0("<strong>Pollen Level:</strong> ", ifelse(is.na(row["plant_level"]), "Not available", row["plant_level"]), ";"),
          paste0("<strong>Category:</strong> ", ifelse(is.na(row["plant_category"]), "Not available", row["plant_category"]), ";"),
          sep = "\n"
        )
      })
      plant_text <- HTML(paste(plant_info, collapse = "<br><br>"))
    } else {
      plant_text <- "No specific plant information available."
    }
    
    list(health_text = health_text, plant_text = plant_text)
  }
}

# Function for creating new log entries
# Creates new entries for the pollen log based on user input
create_new_log_entries <- function(data, selected_date, place, symptoms, symptom_severities) {
  lapply(seq_along(symptoms), function(i) {
    data.frame(
      Date = format(selected_date, "%Y-%m-%d"),
      Location = place,
      Grass_Level = as.numeric(data$type_data$level[data$type_data$type == "GRASS"]),
      Weed_Level = as.numeric(data$type_data$level[data$type_data$type == "WEED"]),
      Tree_Level = as.numeric(data$type_data$level[data$type_data$type == "TREE"]),
      Symptoms = symptoms[i],
      Symptoms_Severity = symptom_severities[i],
      stringsAsFactors = FALSE
    )
  })
}

# Function for formatting log data for display
# Prepares the pollen log data for display in a DataTable
format_log_data_for_display <- function(log_data) {
  if (is.null(log_data) || nrow(log_data) == 0) {
    return(data.frame(Message = "No data available in the table"))
  }
  
  log_data %>%
    dplyr::rename(
      `Date` = Date,
      `Location` = Location,
      `Grass Level` = Grass_Level,
      `Weed Level` = Weed_Level,
      `Tree Level` = Tree_Level,
      `Symptoms` = Symptoms,
      `Severity` = Symptoms_Severity
    ) %>%
    dplyr::select(`Date`, `Location`, `Grass Level`, `Weed Level`, `Tree Level`, `Symptoms`, `Severity`)
}

# Function for generating bar chart
# Creates a bar chart showing average symptom intensity
generate_bar_chart <- function(log_data, for_pdf = FALSE) {
  # Set font family based on output type
  font_family <- if(for_pdf) "sans" else "Quicksand"
  
  # Check if there's enough data to create a meaningful chart
  if (nrow(log_data) < 5) {
    if (for_pdf) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = "Add more data to produce a bar chart.", 
                        size = 6, family = font_family) + 
               theme_void())
    } else {
      return(plot_ly() %>% 
               add_annotations(
                 text = "Add more data to produce a bar chart.",
                 showarrow = FALSE,
                 font = list(size = 16, family = font_family)
               ))
    }
  }
  
  # Summarize symptom data
  symptom_summary <- log_data %>%
    group_by(Symptoms) %>%
    summarise(AvgIntensity = mean(Symptoms_Severity, na.rm = TRUE))
  
  # Create chart based on output type
  if (for_pdf) {
    return(ggplot(symptom_summary, aes(y = Symptoms, x = AvgIntensity)) +
             geom_bar(stat = "identity", fill = "#A6AC8A") +
             theme_minimal(base_family = font_family) +
             labs(x = "Average Symptom Intensity", y = "") +
             theme(text = element_text(family = font_family, size = 12)))
  } else {
    return(plot_ly(symptom_summary, 
                   y = ~Symptoms, 
                   x = ~AvgIntensity, 
                   type = 'bar', 
                   marker = list(color = '#A6AC8A')) %>%
             layout(
               title = "",
               xaxis = list(title = "", zeroline = FALSE),
               yaxis = list(title = "Average Symptom Intensity"),
               font = list(family = font_family, size = 12)
             ))
  }
}

# Save user data to MongoDB
# Updates the user's pollen log in the MongoDB database
save_user_data <- function(username, updated_log, mongo_conn) {
  mongo_conn$update(
    query = sprintf('{"username": "%s"}', username),
    update = sprintf('{"$set": {"pollen_log": %s}}', toJSON(updated_log, auto_unbox = TRUE)),
    upsert = TRUE
  )
}

# Function for generating multi-line chart
# Creates a multi-line chart showing pollen levels and symptoms over time
generate_multi_line_chart <- function(log_data, for_pdf = FALSE, max_labels = 10) {
  # Check if there's enough data to create a meaningful chart
  if (nrow(log_data) < 5) {
    if (for_pdf) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = "Add more data to produce a chart.", 
                        size = 6, family = "sans") + 
               theme_void())
    } else {
      return(plot_ly() %>% add_annotations(
        text = "Add more data to produce a chart.",
        showarrow = FALSE,
        font = list(size = 16, family = "Quicksand")
      ))
    }
  }
  
  # Prepare data for plotting
  plot_data <- log_data %>%
    mutate(Date = as.Date(Date)) %>%
    group_by(Date) %>%
    summarise(
      Grass_Level = mean(Grass_Level, na.rm = TRUE),
      Weed_Level = mean(Weed_Level, na.rm = TRUE),
      Tree_Level = mean(Tree_Level, na.rm = TRUE),
      Avg_Symptoms_Intensity = mean(Symptoms_Severity, na.rm = TRUE)
    ) %>%
    pivot_longer(cols = c(Grass_Level, Weed_Level, Tree_Level, Avg_Symptoms_Intensity), 
                 names_to = "Measure", values_to = "Value")
  
  # Get unique dates
  unique_dates <- unique(plot_data$Date)
  date_range <- range(unique_dates)
  total_days <- as.numeric(difftime(date_range[2], date_range[1], units = "days"))
  
  # Determine label interval and format based on the date range
  if (total_days <= 31) {
    label_interval <- max(1, floor(length(unique_dates) / max_labels))
    date_format <- "%Y-%m-%d"
  } else if (total_days <= 90) {
    label_interval <- max(7, floor(total_days / max_labels))  # At least weekly
    date_format <- "%Y-%m-%d"
  } else if (total_days <= 365) {
    label_interval <- max(30, floor(total_days / max_labels))  # At least monthly
    date_format <- "%Y-%m"
  } else {
    label_interval <- max(90, floor(total_days / max_labels))  # At least quarterly
    date_format <- "%Y-%m"
  }
  
  # Select dates for labels
  label_dates <- unique_dates[seq(1, length(unique_dates), by = label_interval)]
  
  # Ensure we don't exceed max_labels
  if (length(label_dates) > max_labels) {
    label_dates <- label_dates[round(seq(1, length(label_dates), length.out = max_labels))]
  }
  
  # Set colors for different measures
  colors <- c("Grass_Level" = "#A6AC8A", "Weed_Level" = "#727763", 
              "Tree_Level" = "#EAE3D3", "Avg_Symptoms_Intensity" = "#B66C48")
  
  # Create base plot
  p <- ggplot(plot_data, aes(x = Date, y = Value, color = Measure, group = Measure)) +
    geom_line() +
    geom_point(size = 1.3) +
    scale_color_manual(values = colors) +
    scale_x_date(breaks = label_dates, 
                 labels = format(label_dates, date_format),
                 expand = expansion(mult = c(0.02, 0.02))) +
    labs(x = NULL, y = "Pollen Levels/Symptoms Intensity", color = NULL) +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 8),
          axis.text.x = element_text(angle = 35, hjust = 1))
  
  # Adjust plot based on output type
  if (for_pdf) {
    p <- p +
      theme(text = element_text(family = "sans"),
            axis.title.y = element_text(size = 12))
    
    return(p)
  } else {
    p <- p +
      theme(text = element_text(family = "Quicksand"),
            axis.title.y = element_text(size = 14))
    
    p_ly <- ggplotly(p, tooltip = c("x", "y", "color")) %>% 
      layout(font = list(family = "Quicksand"),
             yaxis = list(title = list(text = "Pollen Levels/Symptoms Intensity", font = list(size = 14))),
             xaxis = list(title = list(text = "")),
             legend = list(orientation = "h", y = -0.2, font = list(size = 8)),
             margin = list(t = 40, b = 100, l = 50, r = 20))
    
    # Custom hover text
    for (i in 1:length(p_ly$x$data)) {
      measure_name <- p_ly$x$data[[i]]$name
      p_ly$x$data[[i]]$text <- paste("Date:", format(plot_data$Date[plot_data$Measure == measure_name], "%Y-%m-%d"),
                                     "<br>", measure_name, ":", sprintf("%.2f", plot_data$Value[plot_data$Measure == measure_name]))
      p_ly$x$data[[i]]$hoverinfo <- "text"
    }
    
    return(p_ly)
  }
}

# Generate data encouragement message
# Creates a message encouraging users to log more data
generate_data_encouragement <- function(data) {
  if (nrow(data) < 30) {
    div(
      class = "alert alert-info",
      "Tip: For more accurate and insightful graphs, try to log your symptoms daily for at least a month. This will help identify long-term trends and patterns in your allergy symptoms."
    )
  }
}

# Function to generate pollen log message or data table
# Displays either the pollen log data table or a message if no data is available
generate_pollen_log_content <- function(has_data, pollen_log_data) {
  if (has_data) {
    dataTableOutput("pollen_log")
  } else {
    div(
      style = "text-align: center; padding: 20px;",
      h4("No data available"),
      p("Add pollen and symptom data to view your log.")
    )
  }
}

# Function to generate chart message or plotly output
# Displays either a chart or a message if insufficient data is available
generate_chart_content <- function(has_sufficient_data, chart_type) {
  if (has_sufficient_data) {
    plotlyOutput(chart_type)
  } else {
    div(
      style = "text-align: center; padding: 20px;",
      h4("Insufficient data for meaningful visualization"),
      p("Add more data (at least 5 entries over 3 different days) to see ", 
        if(chart_type == "bar_chart") "average symptom intensity." 
        else "pollen levels and symptoms over time.")
    )
  }
}

# Function to toggle visibility of log buttons
# Shows or hides log-related buttons based on data availability
toggle_log_buttons <- function(session, has_data) {
  if (has_data) {
    shinyjs::show("log_buttons")
  } else {
    shinyjs::hide("log_buttons")
  }
}

# Initialize map
# Creates the initial leaflet map with default view and markers
initialize_map <- function(data, selected_type) {
  popup_content <- create_popup(data$location$place, data$type_data, selected_type)
  leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = data$location$lon, lat = data$location$lat, zoom = 8) %>% 
    addCircleMarkers(lng = data$location$lon, lat = data$location$lat, color = "#A6AC8A") %>% 
    addPopups(lng = data$location$lon, lat = data$location$lat,
              popup = popup_content,
              options = popupOptions(
                closeButton = FALSE,
                closeOnClick = FALSE,
                autoPan = FALSE  # Prevents automatic panning
              )) %>%
    # Center both the location and the popup
    htmlwidgets::onRender("
      function(el, x) {
        var map = this;
        map.on('popupopen', function(e) {
          var px = map.project(e.target._latlng); // Use marker's latlng
          px.y -= e.popup._container.clientHeight/2;
          map.panTo(map.unproject(px), {animate: true});
        });
      }
    ")
}

# Load initial pollen data
# Fetches initial pollen data for Zagreb, Croatia
load_initial_pollen_data <- function() {
  get_pollen_data("Zagreb Croatia", day_offset = 0)
}

# Handle form submission
# Processes the form submission for new pollen data requests
handle_form_submission <- function(place, date) {
  selected_date <- as.Date(date)
  day_offset <- as.numeric(selected_date - Sys.Date())
  
  if (is.na(day_offset) || day_offset < 0 || day_offset > 4) {
    return(NULL)
  }
  
  get_pollen_data(place, day_offset = day_offset)
}

# Reset form
# Resets the form inputs to their default values
reset_form <- function(session) {
  updateTextInput(session, "place", value = "Zagreb Croatia")
  updateDateInput(session, "date", value = Sys.Date())
}

# Create symptom severity inputs
# Generates UI inputs for symptom severity based on selected symptoms
create_symptom_severity_inputs <- function(symptoms) {
  lapply(symptoms, function(symptom) {
    if (symptom != "No Symptoms") {
      sliderInput(
        inputId = paste0("severity_", make.names(symptom)),
        label = paste("Severity of", symptom),
        min = 1, max = 5, value = 1, step = 1
      )
    } else {
      NULL  # Return NULL for "No Symptoms" to skip creating a slider
    }
  })
}

# Get plant image
# Retrieves the image for a specific plant or displays a placeholder
get_plant_image <- function(plant_name) {
  plant_name <- tolower(gsub(" ", "_", plant_name))
  image_path <- file.path("www", "plant_images", paste0(plant_name, ".jpeg"))
  
  if (file.exists(image_path)) {
    tags$img(src = file.path("plant_images", paste0(plant_name, ".jpeg")),
             alt = plant_name,
             width = "100%",
             height = "auto")
  } else {
    tags$div(
      style = "width: 100%; height: 300px; background-color: #f0f0f0; display: flex; justify-content: center; align-items: center;",
      tags$p("Image not available")
    )
  }
}

# Create plant glossary UI
# Generates the UI for displaying detailed plant information
create_plant_glossary_ui <- function(plant) {
  tagList(
    h3(plant$plant_name),
    tags$ul(
      tags$li(strong("Type: "), plant$type),
      tags$li(strong("Family: "), plant$family),
      tags$li(strong("Latin Name: "), plant$latin_name),
      tags$li(strong("Occurrances: "), plant$occurrances),
      tags$li(strong("Season: "), plant$season),
      tags$li(strong("Special Colors: "), plant$special_colors),
      tags$li(strong("Special Shapes: "), plant$special_shapes),
      tags$li(strong("Cross Reaction: "), plant$cross_reaction),
      tags$li(strong("Cross Reaction with Food: "), plant$cross_reaction_with_food),
      tags$li(strong("Allergy Symptoms: "), plant$allergy_symptoms),
      tags$li(strong("Allergy Management: "), plant$allergy_management)
    )
  )
}

# Create tour steps
# Generates the steps for the app tour based on whether log data is available
create_tour_steps <- function(has_log_data = FALSE) {
  base_steps <- list(
    list(
      element = "#home_page",
      intro = "Welcome to the Pollen App! This tool helps you monitor pollen levels, track your allergy symptoms, and make informed decisions about your outdoor activities.",
      tab = "pollen-map"
    ),
    list(
      element = "#navbar",
      intro = "The app is divided into four main sections: Pollen Map, My Log, Plant Glossary, and About. Each tab offers unique features to help you manage your allergies effectively.",
      tab = "pollen-map"
    ),
    list(
      element = "#pollen-map-tab",
      intro = "The Pollen Map tab provides up to 5 days of pollen information for over 65 countries worldwide.",
      tab = "pollen-map"
    ),
    list(
      element = "#place",
      intro = "Enter any location to check its pollen levels. The app supports cities, regions, and even specific addresses globally.",
      tab = "pollen-map"
    ),
    list(
      element = "#date",
      intro = "Select a date to view pollen data. You can see current levels or forecast up to 4 days in advance, helping you plan your activities.",
      tab = "pollen-map"
    ),
    list(
      element = "#submit",
      intro = "Click 'Get Data' to fetch and display the pollen information for your selected location and date.",
      tab = "pollen-map"
    ),
    list(
      element = "#reset",
      intro = "Use the 'Reset' button to clear your inputs and return to the default view.",
      tab = "pollen-map"
    ),
    list(
      element = "#pollenMap",
      intro = "This interactive map visualizes pollen levels. Different colors represent varying pollen intensities. Use the buttons to switch between different pollen types (e.g., grass, tree, weed).",
      tab = "pollen-map"
    ),
    list(
      element = "#health_panel",
      intro = "The Health Recommendations panel provides personalized advice based on current pollen levels. Toggle this panel on/off using the 'Health Recommendations' switch.",
      tab = "pollen-map"
    ),
    list(
      element = "#plant_info",
      intro = "The Plant Information panel offers details about the primary pollen-producing plants in the selected area. Toggle this panel on/off using the 'Plant Info' switch.",
      tab = "pollen-map"
    ),
    list(
      element = "#save_data",
      intro = "After viewing pollen data, click 'Save to My Log' to record it along with your allergy symptoms. This feature helps you track patterns over time.",
      tab = "pollen-map"
    ),
    list(
      element = "#my-log-tab",
      intro = "The My Log tab is your personal allergy diary. It allows you to record and analyze your pollen exposure and symptoms over time.",
      tab = "my-log"
    )
  )
  
  log_data_steps <- list(
    list(
      element = "#pollen_log_or_message",
      intro = "This table displays your historical pollen and symptom data. You can sort, filter, and search through your entries to identify trends.",
      tab = "my-log"
    ),
    list(
      element = "#reset_log",
      intro = "Use the 'Reset Log' button to clear all your logged data. Be cautious, as this action cannot be undone.",
      tab = "my-log"
    ),
    list(
      element = "#download_csv",
      intro = "Export your log data as a CSV file for further analysis or to share with your healthcare provider.",
      tab = "my-log"
    ),
    list(
      element = "#download_pdf",
      intro = "Generate a PDF report of your pollen log, complete with visualizations and summaries. This is perfect for medical appointments or personal record-keeping.",
      tab = "my-log"
    ),
    list(
      element = "#bar_chart_or_message",
      intro = "This bar chart shows your average symptom intensity, helping you understand which symptoms are most affected by pollen.",
      tab = "my-log"
    ),
    list(
      element = "#multi_line_chart_or_message",
      intro = "This multi-line chart visualizes pollen levels and your symptoms over time, helping you identify correlations and seasonal patterns.",
      tab = "my-log"
    )
  )
  
  no_log_data_steps <- list(
    list(
      element = "#pollen_log_or_message",
      intro = "Your log is currently empty. Start by adding data in the Pollen Map tab to see your personal allergy diary here.",
      tab = "my-log"
    ),
    list(
      element = "#bar_chart_or_message",
      intro = "Once you have added data, you'll see a bar chart here showing your average symptom intensity.",
      tab = "my-log"
    ),
    list(
      element = "#multi_line_chart_or_message",
      intro = "After logging some data, a multi-line chart will appear here to help you visualize pollen levels and your symptoms over time.",
      tab = "my-log"
    )
  )
  
  remaining_steps <- list(
    list(
      element = "#plant-glossary-tab",
      intro = "The Plant Glossary is your comprehensive guide to pollen-producing plants and their characteristics.",
      tab = "plant-glossary"
    ),
    list(
      element = "#plant_select",
      intro = "Choose a plant from this dropdown to view detailed information. The search feature helps you quickly find specific plants.",
      tab = "plant-glossary"
    ),
    list(
      element = "#plant_image",
      intro = "Each plant entry includes an image to help you identify it in your environment.",
      tab = "plant-glossary"
    ),
    list(
      element = "#plant_gloss",
      intro = "This section provides in-depth information about the supported plants, including its scientific name, common names, and allergy-related details.",
      tab = "plant-glossary"
    ),
    list(
      element = "#about-tab",
      intro = "The About tab provides information about the app, its features, and how to use it effectively.",
      tab = "about"
    ),
    list(
      element = "#start_tour",
      intro = "You've completed the tour! Remember, you can always revisit this tour by clicking the 'Start Tour' button. Explore the app to make the most of its features in managing your allergies.",
      tab = "about"
    )
  )
  
  # Combine steps based on whether log data is available
  if (has_log_data) {
    steps <- c(base_steps, log_data_steps, remaining_steps)
  } else {
    steps <- c(base_steps, no_log_data_steps, remaining_steps)
  }
  
  return(steps)
}


setup_and_start_tour <- function(pollen_log_data) {
  # Check if the user has data in their log
  has_log_data <- nrow(pollen_log_data) > 0
  
  # Create tour steps based on the presence of log data
  tour_steps <- create_tour_steps(has_log_data)
  
  # JavaScript code for managing the tour
  tour_js <- sprintf('
  var tour = introJs().setOptions({
    steps: %s,
    showProgress: true,
    exitOnOverlayClick: false,
    exitOnEsc: false
  });

  var currentTab = "";

  function switchTabIfNeeded(targetTab) {
    if (targetTab !== currentTab) {
      console.log("Switching to tab:", targetTab);
      switchToTab(targetTab + "-tab");
      currentTab = targetTab;
      return true;
    }
    return false;
  }

  tour.onbeforechange(function(targetElement) {
    var nextStep = tour._currentStep;
    var nextTab = this._options.steps[nextStep].tab;
    console.log("Before change - Current step:", nextStep, "Target tab:", nextTab);
    if (switchTabIfNeeded(nextTab)) {
      setTimeout(function() {
        tour.refresh();
      }, 300);
    }
  });

  tour.onafterchange(function(targetElement) {
    var currentStep = tour._currentStep;
    var currentTab = this._options.steps[currentStep].tab;
    console.log("After change - Current step:", currentStep, "Current tab:", currentTab);
  });

  tour.oncomplete(function() {
    console.log("Tour completed");
    switchToTab("pollen-map-tab");
  });

  tour.onexit(function() {
    console.log("Tour exited");
  });

  console.log("Starting tour");
  tour.start();
  ', toJSON(tour_steps, auto_unbox = TRUE))
  
  shinyjs::runjs(tour_js)
}


# About Tab Content
create_about_tab_content <- function() {
  tagList(
    tags$head(
      tags$style(HTML("
        .custom-link {
          color: #A6AC8A !important;
          text-decoration: underline;
        }
        .custom-link:hover {
          color: #8A9170 !important;
        }
        .custom-panel {
          background-color: #EAE3D3; 
          border-radius: 5px;        
          padding: 20px;            
          box-shadow: 0 4px 12px rgba(0, 0, 0, 0.5)!important;
        }
      "))
    ),
    
    br(),
    div(class = "custom-panel", 
        p("Allergies affect a significant portion of the world's population, with numbers increasing each year. According to the World Allergy Organization, the prevalence of allergic diseases worldwide is rising dramatically in both developed and developing countries. Between 10-30% of the world's population is estimated to suffer from allergic rhinitis, with pollen being a major trigger for many.", style = "font-size: 18px;"),
        br(),
        h4("App Mission", style = "font-size: 20px; font-weight: bold;"),
        p("The Pollen App is designed to empower individuals with accurate, timely information about pollen levels in their area. By providing detailed forecasts and personalized recommendations, this app can help allergy sufferers better manage their symptoms and improve their quality of life.", style = "font-size: 18px;"),
        br(),
        h4("What the App Offers", style = "font-size: 20px; font-weight: bold;"),
        tags$ul(
          style = "font-size: 18px;",
          tags$li("Real-time and forecasted pollen data for locations worldwide"),
          tags$li("Personalized health recommendations based on current pollen levels"),
          tags$li("Detailed information about pollen-producing plants"),
          tags$li("A personal log to track your symptoms and identify patterns"),
          tags$li("Visualizations to help you understand your allergy triggers")
        ),
        br(),
        h4("Powered by Google Pollen API", style = "font-size: 20px; font-weight: bold;"),
        p("This app utilizes the robust Google Pollen API to provide you with accurate and detailed pollen information. Key features include:", style = "font-size: 18px;"),
        tags$ul(
          style = "font-size: 18px;",
          tags$li(strong("High-Resolution Forecasts: "), "Daily pollen index and categories calculated at 1 x 1 kilometer resolution."),
          tags$li(strong("Health Insights: "), "Tailored health recommendations based on current pollen levels for different plant types."),
          tags$li(strong("Comprehensive Plant Data: "), "Detailed information on various plant species, including type, family, season, and potential allergenic effects."),
          tags$li(strong("Global Coverage: "), "Pollen data available for over 65 countries worldwide.")
        ),
        p("For more information about the Google Pollen API, visit: ",
          tags$a(href = "https://developers.google.com/maps/documentation/pollen",
                 target = "_blank",
                 class = "custom-link",
                 "Google Pollen API Documentation"), style = "font-size: 18px;"),
        br(),
        
        h4("Geocoding with Nominatim", style = "font-size: 20px; font-weight: bold;"),
        p("The Pollen App uses the Nominatim geocoding service to convert location names into geographic coordinates. Nominatim is an open-source geocoder that uses OpenStreetMap data, providing worldwide coverage for location searches.", style = "font-size: 18px;"),
        p("Key features of Nominatim include:", style = "font-size: 18px;"),
        tags$ul(
          style = "font-size: 18px;",
          tags$li("Global coverage based on OpenStreetMap data"),
          tags$li("Support for various location types (cities, addresses, landmarks)"),
          tags$li("Open-source and free to use")
        ),
        p("For more information about Nominatim, visit: ",
          tags$a(href = "https://nominatim.org/",
                 target = "_blank",
                 class = "custom-link",
                 "Nominatim Documentation"), style = "font-size: 18px;"),
        br(),
        
        h4("Interactive Tutorial with Intro.js", style = "font-size: 20px; font-weight: bold;"),
        p("The Pollen App features an interactive tutorial implemented using Intro.js to help users navigate and understand its features. Intro.js is a lightweight, open-source JavaScript library that provides step-by-step guides and hints.", style = "font-size: 18px;"),
        p("Benefits of the Intro.js tutorial in this app:", style = "font-size: 18px;"),
        tags$ul(
          style = "font-size: 18px;",
          tags$li("User-friendly, step-by-step introduction to app features"),
          tags$li("Interactive elements to guide users through the app's functionality"),
          tags$li("Easily accessible at any time to refresh understanding of the app")
        ),
        p("To learn more about Intro.js, visit: ",
          tags$a(href = "https://introjs.com/",
                 target = "_blank",
                 class = "custom-link",
                 "Intro.js Official Website"), style = "font-size: 18px;"),
        br(),
        
        h4("Copyright Information", style = "font-size: 20px; font-weight: bold;"),
        p(
          "The background image used in this application is 'Pine Tree Path at Varengeville' by Claude Monet. This artwork is in the public domain and was sourced from:",
          tags$a(
            href = "https://www.wikiart.org/en/claude-monet/pine-tree-path-at-varengeville-1882", 
            target = "_blank",
            class = "custom-link",
            "WikiArt.org - Claude Monet"
          ),
          style = "font-size: 18px;"
        ),
        p(
          "The plant images used in the Plant Glossary section are sourced from: Wikimedia Commons:",
          tags$a(
            href = "https://commons.wikimedia.org/", 
            target = "_blank",
            class = "custom-link",
            "Wikimedia Commons"
          ),
          " and The New York Public Library Digital Collections:",
          tags$a(
            href = "https://digitalcollections.nypl.org/", 
            target = "_blank",
            class = "custom-link",
            "NYPL Digital Collections"
          ),
          style = "font-size: 18px;"
        ),
        p("These images are either in the public domain or licensed for free use with attribution. Specific attributions, where required, are provided alongside each image in the Plant Glossary.", style = "font-size: 18px;"),
        p("Usage of these images complies with applicable copyright laws. If you believe that any content in this application infringes upon your copyright, please contact the app developer immediately.", style = "font-size: 18px;"),
        br(),
        h4("Source Code", style = "font-size: 20px; font-weight: bold;"),
        p("The source code for the Pollen App is freely available on GitHub:", style = "font-size: 18px;"),
        div(class = "text-center",
            tags$a(href = "https://github.com/andabaka/pollen_app", 
                   target = "_blank",
                   icon("github"), 
                   "View Source Code on GitHub",
                   class = "btn",
                   style = "background-color: #A6AC8A; color: white; border: none;")
        )
    )
  )
}



# Load user data from MongoDB
# Retrieves the user's pollen log data from the database
load_user_data <- function(username, mongo_conn) {
  user_data <- mongo_conn$find(sprintf('{"username": "%s"}', username))
  if (!is.null(user_data$pollen_log)) {
    pollen_log <- fromJSON(toJSON(user_data$pollen_log, auto_unbox = TRUE))
    return(as.data.frame(pollen_log))
  }
  return(NULL)
}