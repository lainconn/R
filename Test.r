# create example vectors
vec1 <- as.vector(c(10, 20, 30))
vec2 <- as.vector(c(5, 25, 35))

# initialize third vector
vec3 <- vector(length = length(vec1))

# compare elements in vec1 and vec2 and assign values to vec3
for (i in seq_along(vec1)) {
  if (vec1[i] > vec2[i]) {
    vec3[i] <- "cash"
  } else {
    vec3[i] <- "dep"
  }
}

# print results
print(vec3)
