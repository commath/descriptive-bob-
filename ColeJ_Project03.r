# Name: Joseph Cole
# Course: 44-149 Scientific Computing
# Assignment # (project 3)
# Due Date: 4/20/18
# Brief: I made a program that quiclky divides the country into new sections
# By submitting this, I pledge that the code in this file was written by the author indicated above,
#  and that all assistance from outside sources was correctly attributed in comments.  Additionally, I 
#  agree to abide by the rules expressed in the CSIS Academic Honesty Policy
N <- 8
census <- read.csv('us_census.csv')
#contiguous <- census
contiguous <- census[!census$state %in% c('AK', 'HI', 'PR'), ]
latidudes <- contiguous$latitude
longitudes <- contiguous$longitude
#plot(contiguous$longitude, contiguous$latitude, type='p', col=contiguous$state)

chosen_counties <- sample(1:nrow(contiguous), N)

#print(chosen_counties)

centers <- matrix(0, nrow = N, ncol = 2)

#for (i in 1:N) {
#    centers[i][1] = contiguous[chosen_counties[i],'latitude']
#    centers[i][2] = contiguous$longitude[chosen_counties[i]]
#}

centers[, 1] = contiguous$latitude[chosen_counties]
centers[, 2] = contiguous$longitude[chosen_counties]

#print(centers)
# as a data frame
centers_df = contiguous[chosen_counties, 3:4]

#print(contiguous[1,])
#print(centers[1,])

dist_sq <- function(county, center) {
  deltax <- latidudes[county] - center[1]
  deltay <- longitudes[county] - center[2]
  deltax ^ 2 + deltay ^ 2
}

#deltax <- contiguous[1, 'latitude'] - centers[1,1]
#deltay <- contiguous[1, 'longitude'] - centers[1,2]
#print(dist_sq(contiguous[1,], centers[1,]))

# belongs_to[i] means the ith county belongs_to the cluster at
# belongs_to[i]
belongs_to <- rep(0, nrow(contiguous))

# figure out closest cluster
for (county in 1:nrow(contiguous)) {
  closest_center <- 1
  closest_distance <- dist_sq(county, centers[1, ])
  
  for (cluster in 2:N) {
    d <- dist_sq(county, centers[cluster, ])
    if (d < closest_distance) {
      closest_distance <- d
      closest_center <- cluster
    }
  }
  belongs_to[county] <- closest_center
}
# print(belongs_to)
plot(contiguous$longitude,
     contiguous$latitude,
     type = 'p',
     col = belongs_to)
for (cl in 1:N) {
  clust_of_interest <- contiguous[belongs_to == cl, ]
  total_pop <- sum(clust_of_interest$population)
  # print(sum(clust_of_interest$population))
  new_latitude <-
    sum(clust_of_interest$latitude * clust_of_interest$population) / total_pop
  new_longitude <-
    sum(clust_of_interest$longitude * clust_of_interest$population) / total_pop
  centers[cl, 1] <- new_latitude
  centers[cl, 2] <- new_longitude
  plot(contiguous$longitude, contiguous$latitude, col=belongs_to)
}