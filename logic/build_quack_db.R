library(duckdb)


generate_nyflights_demo_db <- function(){
  con <- dbConnect(duckdb())
  duckdb_register(con, "flights", nycflights13::flights)

  return(con)
}


# Function to analyze flights data
analyze_flights_data <- function(dataset, destination_filter) {

  # Filter dataset based on the specified origin
  filtered_dataset <- dataset %>%
    filter(dest == destination_filter)

  # Average Departure Delay by Carrier
  avg_dep_delay_by_carrier <- filtered_dataset %>%
    group_by(carrier) %>%
    summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) |>
    collect()

  # Monthly Flight Count
  flight_count_by_month <- filtered_dataset %>%
    group_by(year, month) %>%
    summarize(flight_count = n()) |>
    collect()

  # Percentage of Delayed Flights by Origin and Destination
  delayed_flights_by_origin_dest <- filtered_dataset %>%
    filter(dep_delay > 0) %>%
    group_by(origin, dest) %>%
    summarize(delayed_count = n()) |> collect()

  total_flights_by_origin_dest <- filtered_dataset %>%
    group_by(origin, dest) %>%
    summarize(total_count = n()) |>
    collect()

  percentage_delayed_flights <- delayed_flights_by_origin_dest |>
    left_join(total_flights_by_origin_dest,
              join_by(origin, dest)
              ) %>%
    mutate(percentage_delayed = (delayed_count / total_count) * 100) |>
    collect()

  filtered_data_sql <- filtered_dataset |>
    sql_render(sql_options = sql_options(use_star = FALSE,
                                         cte = TRUE))

  # Return a named list
  result <- list(
    avg_dep_delay_by_carrier = avg_dep_delay_by_carrier,
    flight_count_by_month = flight_count_by_month,
    percentage_delayed_flights = percentage_delayed_flights,
    delayed_flights_by_origin_dest = delayed_flights_by_origin_dest,
    filtered_dataset = filtered_dataset,
    filtered_data_sql = filtered_data_sql
  )

  return(result)
}

# # Example usage:
# # Replace 'JFK' with the desired origin to filter by
# origin_to_filter <- 'JFK'
# result <- analyze_flights_data(nyflights_tbl, origin_to_filter)
#
# # Print the results
# print("Average Departure Delay by Carrier:")
# print(result$avg_dep_delay_by_carrier)
#
# cat("\nMonthly Flight Count:")
# print(result$flight_count_by_month)
#
# cat("\nPercentage of Delayed Flights by Origin and Destination:")
# print(result$percentage_delayed_flights)
#
#

visualize_avg_dep_delay_by_destination <- function(avg_dep_delay_by_carrier) {
  ggplot(avg_dep_delay_by_carrier, aes(x = carrier, y = avg_dep_delay)) +
    geom_bar(stat = "identity", fill = "skyblue", color = "black") +
    labs(title = "Average Departure Delay by Carrier",
         x = "Carrier",
         y = "Average Departure Delay") +
    theme_minimal()
}


visualize_monthly_flight_count <- function(flight_count_by_month) {
  ggplot(flight_count_by_month, aes(x = paste(year, month, sep = "-"), y = flight_count, group = 1)) +
    geom_line(color = "steelblue", size = 1) +
    labs(title = "Monthly Flight Count Over Time",
         x = "Time",
         y = "Flight Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


visualize_distribution_of_flights_by_delayed_status <- function(filtered_dataset) {

  # Create a new column 'delayed_status' based on the count
  delayed_counts <- filtered_dataset %>%
    mutate(
      delayed_status = if_else(dep_delay > 0, "Delayed", "Not Delayed")
    ) |>
    group_by(delayed_status) |>
    count() |>
    collect()

  # Create a pie chart
  ggplot(delayed_counts, aes(x = "", y = n, fill = delayed_status)) +
    geom_col(width = 1, color = "white") +
    geom_text(
      aes(label = n),
      position = position_stack(vjust = 0.5),
      color = "black",
      size = 4
    ) +
    coord_polar("y") +
    labs(title = "Distribution of Flights by Delayed Status",
         fill = "Delayed Status") +
    theme_void() +
    theme(legend.position = "bottom")
}

# Example usage:
# Assuming 'filtered_dataset' is your filtered dataset
#visualize_distribution_of_flights_by_delayed_status(filtered_dataset)


