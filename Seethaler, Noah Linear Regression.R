###BS803 Final Project
library(tidyverse)

proj <- function (z, y, x, instructions = T) {
  if (instructions){
    cat("How to use the multiple linear regression function:\n",
        "Make sure you have a complete data frame containing predictor and outcome variables.  At this time, please install and load the tidyverse package. For ease of use, remove variables that are not predictor or outcome variables from the data frame.\n",
        "I recommend reading in your dataset as data, then making a new dataset (z) with the outcome and predictor variables.\n",
          "z: a data frame with a selected outcome variable and selected predictors\n",
          "y: a single column in the z data frame that is selected as your outcome variable\n",
          "x: one to multiple columns in the z data frame that are selected as your predictor variable(s)\n",
        "z <- data %>% select(y, x1, x2, x3, x4)\n",
        "y <- z %>% select(y)\n",
        "x <- z %>% select(-y)\n",
        "Example:\n",
        "proj(z, y, x, instruction = F)\n",
        "If it runs without error messages, the model will print your beta coefficients, goodness of fit estimates, standard errors for the predictors, t and p values, F statistic with p value and degrees of freedom, an interpretation of your goodness of fit estimates, and a plot with actual and predicted values.\n",
        "The beta coefficients start with the intercept, then the coefficient for your first predictor, then the coefficient for the second predictor, etc.\n",
        "Have fun exploring linear regression!")
    return()
  }
  y_name <- colnames(y)
  if (!any(names(z) == y_name)) {
    stop("Warning: the specified outcome variable is not in the full dataset")
  }
  x_names <- colnames(x)
  if (any(!all_of(names(x_names)) %in% names(z))) {
    stop("Warning: one or more specified predictor variable(s) are not in the full dataset")
  }
  y <- as.matrix(y)
  x <- as.matrix(x)

  if (!is.numeric(x) || !is.numeric(y)) {
    stop("Warning: input contains non-numeric values")
  }
  if (nrow(y) != nrow(x)){
    stop("Warning: invalid dimensions! Number of rows of y and x should be equal.")
  }
    else if (ncol(y) == nrow(x)){
  }
if(sum(is.na(x)) > 0){
  stop("Warning: NA values detected")
}
if(sum(is.na(y)) > 0){
  stop("Warning: NA values detected")
}  
  
  x <- cbind(1, x)
  
  beta <- solve((t(x)%*%x)) %*% t(x)%*%y
  pred <- x %*% beta
  res <- y - pred
  RSS <- sum(res^2)
  SYY <- sum((y - mean(y))^2)
  SSR <- SYY - RSS
  R2 <- 1 - RSS / SYY
  n <- nrow(x)
  p <- nrow(beta) - 1
  R2.adjusted <- ((n-1)*R2 - p) / (n - p - 1)
  sigma2 <- sum(res**2) / (n- ncol(x))
  cov <- sigma2 * solve(t(x) %*% x)
  se <- sqrt(diag(cov))
  f <- (SSR / p) / (RSS / (n - p - 1))
  fp <- round(1 - pf(f, p, n - p - 1), 6)
  
df <- n - p - 1
t <- beta / se
pv <- ifelse(t < 0,
            2*(pt(t, df)),
               2* (1 - pt(t, df)))

results <- data.frame(beta, se, t, pv)
         
  colnames(results) <- c("Beta Coefficients", "Standard Errors", "t-values", "p-values")
  print(results)
  
  if (R2 > 0.5) {
    cat("The model explains a significant amount of variance in the outcome variable (R-squared =", round(R2,2),"). Adjusted R-squared =", round(R2.adjusted,2), "\n")
  } else {
    cat("The model explains a lesser amount of variance in the outcome variable (R-squared =", round(R2, 2),"). Adjusted R-squared =", round(R2.adjusted,2), "\n")
  }
  cat("F-statistic =", round(f, 2), ", p-value =", round(fp, 10), "on", p, "and", n-p-1, "df\n")
  
  plot(y, pred, xlab = "Observed Outcome Values", ylab = "Predicted Outcome Values", main = "Observed vs Predicted Values",
       pch = 10, col = "black")
  
  abline(a = 0, b = 1, col = "red")
  }

#test
data <- read.csv("C:/.csv")
z <- data %>% select(y, x1, x2, x3, x4)
y <- z %>% select(y)
x <- z %>% select(-y)

proj(z, y, x, instructions = F)


#compared to lm function
test <- lm(y ~ x1 + x2 + x3 + x4, data = data)
summary(test)

