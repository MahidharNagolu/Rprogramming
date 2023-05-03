mom_numbers <- c(1, 2, 3, 4, 5, 6) # replace with your mom's numbers

lottery_numbers <- sample(1:50, 6, replace = FALSE)



lottery_numbers

if (identical(sort(mom_numbers), sort(lottery_numbers))) {

  print("Mom wins the lottery!")

} else {

  print("Mom did not win the lottery.")

}
#2

# Function to check if a number is an Armstrong number

isArmstrong <- function(num) {

  # Initialize variables

  sum <- 0

  n <- nchar(num)

  temp <- num

  

  # Use while loop to sum the digits raised to the power of n

  while (temp > 0) {

    digit <- temp %% 10

    sum <- sum + digit^n

    temp <- temp %/% 10

  }

  

  # Check if the number is an Armstrong number

  if (sum == num) {

    return(TRUE)

  } else {

    return(FALSE)

  }

}



# Test the function

num <- 153

if (isArmstrong(num)) {

  print(paste(num, "is an Armstrong number"))

} else {

  print(paste(num, "is not an Armstrong number"))

}
#3
kelvin_to_celsius <- function(temp_kelvin) {

  temp_celsius <- temp_kelvin - 273.15

  return(temp_celsius)

}

kelvin_to_celsius(300)
#4
# Define the class

account <- function(account_number, account_holder, balance) {

  structure(list(account_number = account_number,

                 account_holder = account_holder,

                 balance = balance),

            class = "account")

}



# Define a print method for the class

print.account <- function(x) {

  cat("Account number: ", x$account_number, "\n")

  cat("Account holder: ", x$account_holder, "\n")

  cat("Balance: ", x$balance, "\n")

}



# Define an amount method for the class

amount.account <- function(x) {

  return(x$balance)

}



# Apply getS3method to get the print and amount methods

print_method <- getS3method("print", "account")

amount_method <- getS3method("amount", "account")



# Create an account object

my_account <- account("123456789", "John Doe", 1000)



# Call the print method on the account object

print_method(my_account)



# Call the amount method on the account object and display the output

balance <- amount_method(my_account)

cat("Account balance: ", balance, "\n")
