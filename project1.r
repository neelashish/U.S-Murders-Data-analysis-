library(dslabs)
data(murders)
murder_rate <- murders$total / murders$population * 100000

# assining murder_rate in murders creating a new column

murders$murder_rate <- murder_rate
cat(seq = "\n")
str(murders)

# printing it just to match the value

# print(murders$murder_rate) # nolint

# using order() to get the indices from murder_rate

highest <- order(murders$murder_rate, decreasing = TRUE)

top_states <- murders$state[murder_rate[1:10]]
cat(seq = "\n")
cat("top 10 states with highest murders rate \n")
print(top_states, seq = " \n ")
cat(seq = "\n")

# FINDING OUT THE SAFEST REGION WHICH HAS MURDERS RATE < 1

if (any(murder_rate < 1)) {
  pro <- murder_rate[murder_rate < 1]
  ind_pro <- order(pro)
}
my_data <- data.frame(
  state = murders$state[ind_pro],
  rate = pro
)
print("SAFEST REGION HAHAHAHAHAHAH")
print(format(my_data, justify = "centre"))
cat(seq = "\n")

# VISULATIZATION
options(scipen = 999)
par(mfrow = c(3, 1))
plot(murders$population, murders$total,
     main = "Population vs Total Murders",
     xlab = "Population", ylab = "Total Murders")
boxplot(murder_rate ~ region, data = murders,
        main = "Murder Rate by Region",
        ylab = "Murder Rate", col = "lightgreen")
hist(murder_rate,
     main = "murders rate",
     xlab = "murders rate")
par(mfrow = c(1, 1))

low_pop_state <- which.min(murders$population)
print("State with lowest population")
my_data1 <- data.frame(state = murders$state[low_pop_state],
                       population = murders$population[low_pop_state])
print(my_data1)

cat(seq = "\n")
cat("RANKING STATES ON BASIS OF MURDERS RATE", seq = "\n")

pop <- order(murders$population, decreasing = FALSE)
rate <- order(murders$murder_rate, decreasing = FALSE)
cat(seq = "\t", "RANK BY POPULATION", seq = "\n")

my_data2 <- data.frame(
  state = murders$state[pop],
  murders_rate = murders$murder_rate[pop],
  population = murders$population[pop]
)
print(format(my_data2, justify = "centre"))

print(seq = "\t", "RANK BY RATE ", seq = "\n")

my_data3 <- data.frame(
  state = murders$state[rate],
  murders_rate = murders$murder_rate[rate],
  population = murders$population[rate]
)
print(format(my_data3, justify = "centre"))

# Saving the data frame to a CSV file

write.csv(murders, "murders_summary.csv", row.names = FALSE)
