#ascending or descending order

exchange.sort.asc <- function(input_vector, decreasing = FALSE) {
  input_vector_clone <- input_vector
  vector_length <- length(input_vector)
  if (decreasing == FALSE) {
  
    for (i in 1:(vector_length - 1)) {
      for (j in (i + 1):vector_length) {
        if (input_vector[i] > input_vector[j]) {
          temp <- input_vector[i]
          input_vector[i] <- input_vector[j]
          input_vector[j] <- temp
       }
     }
   }
    return(input_vector)
  }else {
    for (i in 1:(vector_length - 1)) {
      for (j in (i + 1):vector_length) {
        if (input_vector[i] < input_vector[j]) {
          min <- input_vector[i]
          input_vector[i] <- input_vector[j]
          input_vector[j] <- min
        }
      }
    }
     return(input_vector)
   }
 }
 
unsorted_vector <- round(runif(10)*100)
exchange.sort.asc(unsorted_vector, decreasing = TRUE)




#calculating standard deviation

my.sd <- function(num_vector) {
  total <- 0
  for (i in num_vector) {
    total <- total + i
  }
  x_bar <- return(total / length(num_vector))
  my_sum <- 0
  len <- length(num_vector) - 1
  for (j in num_vector) {
    my_sum <- my_sum + (j - x_bar)^2
  }
  return(sqrt(my_sum / len))
}
input_num <- round(runif(10)*100)
my.sd(input_num)
