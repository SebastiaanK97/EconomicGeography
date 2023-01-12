
# function dplyr to matrix
function_tecnological_entry_period <- function(IPC, APP, geo_data) {

# define time frame
period <- c(0, 1, 2, 3, 4)
start <- c(1994, 1999, 2004, 2009, 2013)
end <- c(1998, 2003, 2008, 2013, 2018)
time_frame <- as.data.frame(cbind(period, start, end))

for (i in 1:nrow(time_frame["period"])) {
  
  # create variable name in for-loop for t
  name <-  paste("mat_RTA_t", i, sep = "")
  
  # query and clean data for periods
  IPC4_NUTS2 <- IPC %>%
    filter(prio_year >= time_frame[i, "start"] & prio_year <= time_frame[i, "end"]) %>%
    left_join(APP) %>%
    filter(NUTS2 %in% geo_data$geo) %>%
    select(NUTS2, IPC4) %>%
    mutate(count = 1) %>%
    filter(IPC4 != "")
  
  # count patents per region per technological field
  mat_count <- get.matrix(IPC4_NUTS2)
  # compute Revealed Technological Advantage (RTA) for period in for-loop
  mat_RTA <- location.quotient(mat_count, binary = TRUE)
  
  # assign RTA to the name with corresponding period in for-loop
  assign(name, mat_RTA)
   
}

# compute entry of technological fields for multiple periods
# note if RTA_t = 1 and RTA_t+1 = 1 then the technological entry results in NA
tecnological_entry_period <- entry.list(mat_RTA_t1, mat_RTA_t2, mat_RTA_t3, mat_RTA_t4, mat_RTA_t5)

# return result from function
return(tecnological_entry_period)

}
