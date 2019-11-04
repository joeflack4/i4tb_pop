num_years <- 20
num_countries <- 12
num_cohorts <- 11
num_years_cohort_active <- 10
year_start_cohort1 <- 2020

# TODO: Auto-calculate num_cohorts_active_in_year
# cohort_years_active = {}
# for (c in 1:num_cohorts) {
#   start_year = year_start_cohort1 + c - 1
#   end_year = start_year + num_years_cohort_active - 1
#   # I don't think this is valid R; this is how to do a list comprehension in Python:
#   cohort_years_active[c] = [x for x in range(start_year, end_year)]
# }
num_cohorts_by_year = {
  2020: 1,
  2021: 2,
  2022: 3,
  2023: 4,
  2024: 5,
  2025: 6,
  2026: 7,
  2027: 8,
  2028: 9,
  2029: 10,
  2030: 10,
  2031: 9,
  2032: 8,
  2033: 7,
  2034: 6,
  2035: 5,
  2036: 4,
  2037: 3,
  2038: 2,
  2039: 1,
}

caseaverted <- matrix(NA, nrow=num_countries, ncol=num_years)
for (k in 1:num_countries) {
  # caseaverted[k,1] <- l.case[[k]][1,1]
  # caseaverted[k,2] <- l.case[[k]][2,1]+l.case[[k]][1,2]
  # caseaverted[k,3] <- l.case[[k]][3,1]+l.case[[k]][2,2]+l.case[[k]][1,3]
  # caseaverted[k,4] <- l.case[[k]][4,1]+l.case[[k]][3,2]+l.case[[k]][2,3]+l.case[[k]][1,4]
  # caseaverted[k,5] <- l.case[[k]][5,1]+l.case[[k]][4,2]+l.case[[k]][3,3]+l.case[[k]][2,4]+l.case[[k]][1,5]  
  # caseaverted[k,6] <- l.case[[k]][6,1]+l.case[[k]][5,2]+l.case[[k]][4,3]+l.case[[k]][3,4]+l.case[[k]][2,5]+l.case[[k]][1,6]
  # caseaverted[k,7] <- l.case[[k]][7,1]+l.case[[k]][6,2]+l.case[[k]][5,3]+l.case[[k]][4,4]+l.case[[k]][3,5]+l.case[[k]][2,6]+l.case[[k]][1,7]
  # caseaverted[k,8] <- l.case[[k]][8,1]+l.case[[k]][7,2]+l.case[[k]][6,3]+l.case[[k]][5,4]+l.case[[k]][4,5]+l.case[[k]][3,6]+l.case[[k]][2,7]+l.case[[k]][1,8]
  # caseaverted[k,9] <- l.case[[k]][9,1]+l.case[[k]][8,2]+l.case[[k]][7,3]+l.case[[k]][6,4]+l.case[[k]][5,5]+l.case[[k]][4,6]+l.case[[k]][3,7]+l.case[[k]][2,8]+l.case[[k]][1,9]
  # # 2029+
  # caseaverted[k,10] <- l.case[[k]][10,1]+l.case[[k]][9,2]+l.case[[k]][8,3]+l.case[[k]][7,4]+l.case[[k]][6,5]+l.case[[k]][5,6]+l.case[[k]][4,7]+l.case[[k]][3,8]+l.case[[k]][2,9]+l.case[[k]][1,10]
  # caseaverted[k,11] <- l.case[[k]][10,2]+l.case[[k]][9,3]+l.case[[k]][8,4]+l.case[[k]][7,5]+l.case[[k]][6,6]+l.case[[k]][5,7]+l.case[[k]][4,8]+l.case[[k]][3,9]+l.case[[k]][2,10]+l.case[[k]][1,11]
  # caseaverted[k,12] <- l.case[[k]][10,3]+l.case[[k]][9,4]+l.case[[k]][8,5]+l.case[[k]][7,6]+l.case[[k]][6,7]+l.case[[k]][5,8]+l.case[[k]][4,9]+l.case[[k]][3,10]+l.case[[k]][2,11]
  # caseaverted[k,13] <- l.case[[k]][10,4]+l.case[[k]][9,5]+l.case[[k]][8,6]+l.case[[k]][7,7]+l.case[[k]][6,8]+l.case[[k]][5,9]+l.case[[k]][4,10]+l.case[[k]][3,11]
  # caseaverted[k,14] <- l.case[[k]][10,5]+l.case[[k]][9,6]+l.case[[k]][8,7]+l.case[[k]][7,8]+l.case[[k]][6,9]+l.case[[k]][5,10]+l.case[[k]][4,11]  
  # caseaverted[k,15] <- l.case[[k]][10,6]+l.case[[k]][9,7]+l.case[[k]][8,8]+l.case[[k]][7,9]+l.case[[k]][6,10]+l.case[[k]][5,11]
  # caseaverted[k,16] <- l.case[[k]][10,7]+l.case[[k]][9,8]+l.case[[k]][8,9]+l.case[[k]][7,10]+l.case[[k]][6,11]
  # caseaverted[k,17] <- l.case[[k]][10,8]+l.case[[k]][9,9]+l.case[[k]][8,10]+l.case[[k]][7,11]
  # caseaverted[k,18] <- l.case[[k]][10,9]+l.case[[k]][9,10]+l.case[[k]][8,11]
  # caseaverted[k,19] <- l.case[[k]][10,10]+l.case[[k]][9,11]
  # caseaverted[k,20] <- l.case[[k]][10,11]

  for (y in 1:num_years) {
    this_year = 2020 + y - 1
    num_cohorts_active <- num_cohorts_by_year[this_year]
    
    # For year before 2028, 'i' in [i,j] increments by 1. After that it is statically '10'
    # 'i' and 'j' have no meaning but are typical variables names used for iteration
    if (y <= 2028) {
      i <- 0 + y
      j <- 1
      caseaverted[k,y] <- l.case[[k]][i,j]
      for (n in num_cohorts_active)
        if (n > 1) {
          # I'm not sure about if this is valid R:
          caseaverted[k,y] <- caseaverted[k,y] + l.case[[k]][i-1,j+1]
        }
    } else {
      # 2029-2039
      # TODO: Haven't figured out this logic yet. Honestly though I'm sure there is a way 
      #  to do this algorithmically without needing to do it differently for 2020-2028 
      #  and 2029-3039
    }
  }
}
