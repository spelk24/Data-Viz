# ------------------------------ #
# --- Peloton APIx Functions --- #
# ------------------------------ #

peloton_api_login <- function(username, pw) {
  ###
  # Peloton API login using username & password
  # Returns: user_id
  ###
  
  # Login Endpoint
  login_url <- "https://api.onepeloton.com/auth/login"
  
  # Login Request
  res <- POST(
    login_url, 
    body = list(
      username_or_email = username,
      password = pw
    ),
    encode = "json"
  )
  
  # Login message
  message("Successfully logged in")
  
  # Extract & Return User ID
  user_id <- fromJSON(rawToChar(res$content))[["user_id"]]
  return(user_id)
}

pull_all_workouts <- function(username, pw) {
  ###
  # Pulls all Peloton workout data after
  # successful login
  ###
  
  # Call peloton_api_login()
  user_id <- peloton_api_login(username, pw)
  
  # Store start time
  code_start_time <- Sys.time()
  message(
    glue::glue("Pulling {username} workout info as of {code_start_time}")
  )
  
  # User workouts endpoint
  .workouts_url <- glue::glue(
    "https://api.onepeloton.com/api/user/{user_id}/workouts?joins=peloton.ride&limit=100&page=0&sort_by=-created"
  )
  
  # User workouts request
  res <- GET(.workouts_url)
  data <- fromJSON(rawToChar(res$content))
  all_workouts <- tibble() # used for storing all workouts
  
  for (w in 1:length(data$data)) {
    # Grab base Workout Info
    workout_row <- tibble(
      workout_id = data$data[[w]]$id,
      workout_start_time = as_datetime(data$data[[w]]$start_time, tz = "America/Chicago"),
      workout_date = as_date(workout_start_time),
      workout_type = data$data[[w]]$fitness_discipline,
      workout_display_name = data$data[[w]]$peloton$ride$fitness_discipline_display_name,
      workout_title = data$data[[w]]$peloton$ride$title,
      instructor_id = ifelse(
        is.null(data$data[[w]]$peloton$ride$instructor_id), 
        "None", 
        data$data[[w]]$peloton$ride$instructor_id
      ),
      personal_record = data$data[[w]]$is_total_work_personal_record,
      total_work = data$data[[w]]$total_work,
      duration_seconds = data$data[[w]]$peloton$ride$duration,
      difficulty_rating = data$data[[w]]$peloton$ride$difficulty_rating_avg
    )
    
    # Grab Cycling specific info
    if (workout_row$workout_type[1] == "cycling") {
      # Workout Endpoints
      workout_id <- workout_row$workout_id[1]
      .workout_url <- glue::glue("https://api.onepeloton.com/api/workout/{workout_id}")
      .workout_perf_url <- glue::glue("https://api.onepeloton.com/api/workout/{workout_id}/performance_graph")
      
      # Pull info from api/workout/{workout_id}
      res <- GET(.workout_url)
      wo_data <- fromJSON(rawToChar(res$content))
      
      workout_row <- workout_row %>% 
        mutate(
          leaderboard_rank = wo_data$leaderboard_rank,
          total_users = wo_data$total_leaderboard_users
        )
      
      # Pull info from api/workout/{workout_id}/performance_graph
      res <- GET(.workout_perf_url)
      perf_data <- fromJSON(rawToChar(res$content))
      workout_row <- workout_row %>% 
        mutate(
          avg_output = perf_data$average_summaries[[1]]$value,
          avg_cadence = perf_data$average_summaries[[2]]$value,
          avg_resistance = perf_data$average_summaries[[3]]$value,
          avg_speed = perf_data$average_summaries[[4]]$value,
          tot_distance = perf_data$summaries[[2]]$value,
          tot_calories = perf_data$summaries[[3]]$value
        )
    }
    
    # Append to full workouts
    all_workouts <- bind_rows(all_workouts, workout_row)
  }
  
  # End Time
  message(
    glue::glue("Code Run time: {round(Sys.time() - code_start_time, 1)} seconds for {nrow(all_workouts)} workouts")
  )
  
  # Return full df of workouts
  return(all_workouts)
}
