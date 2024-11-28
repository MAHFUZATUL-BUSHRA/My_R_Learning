############################### HYPOTHESIS TESTING ####################################
##Z-test
#Problem

#Suppose the manufacturer claims that the mean lifetime of a light bulb is more than 10,000 hours. 
#In a sample of 30 light bulbs, it was found that they only last 9,900 hours on average. 
#Assume the population standard deviation is 120 hours. At .05 significance level, can we reject the claim by the manufacturer?

#Solution

#The null hypothesis is that mu0 >= 10000. We begin with computing the test statistic.

xbar = 9900            # sample mean 

mu0 = 10000            # hypothesized value 

sigma = 120            # population standard deviation 

n = 30                 # sample size 

z = (xbar-mu0)/(sigma/sqrt(n)) 

z                      # test statistic 

#We then compute the p-value

pnorm(z) 

#Answer

#The p-value is less than 0.05. Hence, at .05 significance level, we reject the claim that mean lifetime of a light bulb is above 10,000 hours.

#---------------------------------------------------------------------------------------------------------
#Upper Tail Test of Population Mean with Known Variance

#Problem

#Suppose the food label on a cookie bag states that there is at most 2 grams of saturated fat in a single cookie. 
#In a sample of 35 cookies, it is found that the mean amount of saturated fat per cookie is 2.1 grams.
#Assume that the population standard deviation is 0.25 grams. At .05 significance level, can we reject the claim on food label?


#Solution

#The null hypothesis is that mu0 <= 2. We begin with computing the test statistic.

xbar = 2.1             # sample mean 

mu0 = 2                # hypothesized value 

sigma = 0.25           # population standard deviation 

n = 35                 # sample size 

z = (xbar-mu0)/(sigma/sqrt(n)) 

z                      # test statistic 

#We then compute the p-value

pnorm(z, lower.tail = FALSE) 

#Answer

#The p-value is less than 0.05. Hence, at .05 significance level, we reject the claim that there is at most 2 grams of saturated fat in a cookie.

#_________________________________________________________________________________________________________
#Two-Tailed Test of Population Mean with Known Variance

#Problem

#Suppose the mean weight of King Penguins found in an Antarctic colony last year was 15.4 kg. 
#In a sample of 35 penguins same time this year in the same colony, the mean penguin weight is 14.6 kg. 
#Assume the population standard deviation is 2.5 kg. At .05 significance level, can we reject the null hypothesis that the mean penguin weight does not differ from last year?

#Solution

#The null hypothesis is that mu0 = 15.4. We begin with computing the test statistic.

xbar = 14.6            # sample mean 

mu0 = 15.4             # hypothesized value 

sigma = 2.5            # population standard deviation 

n = 35                 # sample size 

z = (xbar-mu0)/(sigma/sqrt(n)) 

z                      # test statistic 

#We then compute the p-value

2*pnorm(-abs(z)) 

#Answer

#The p-value is not less than 0.05. Hence, at .05 significance level, we do not reject the null hypothesis that the mean penguin weight does not differ from last year.

#_________________________________________________________________________________________________________
###T-test

#Lower Tail Test of Population Mean with Unknown Variance

#Problem

#Suppose the manufacturer claims that the mean lifetime of a light bulb is more than 10,000 hours. 
#In a sample of 30 light bulbs, it was found that they only last 9,900 hours on average. 
#Assume the sample standard deviation is 125 hours. At .05 significance level, can we reject the claim by the manufacturer?

#Solution

#The null hypothesis is that  mu0 >= 10000. We begin with computing the test statistic.

xbar = 9900            # sample mean 

mu0 = 10000            # hypothesized value 

s = 125                # sample standard deviation 

n = 30                 # sample size 

t = (xbar-mu0)/(s/sqrt(n)) 

t                      # test statistic 

#We then compute the p-value

pt(t, df = n-1) 

#Answer

#The p-value is less than 0.05. Hence, at .05 significance level, we can reject the claim that mean lifetime of a light bulb is above 10,000 hours.

#_________________________________________________________________________________________________________
#Upper Tail Test of Population Mean with Unknown Variance

#Problem

#Suppose the food label on a cookie bag states that there is at most 2 grams of saturated fat in a single cookie. 
#In a sample of 35 cookies, it is found that the mean amount of saturated fat per cookie is 2.1 grams. 
#Assume that the sample standard deviation is 0.3 gram. At .05 significance level, can we reject the claim on food label?

#Solution

#The null hypothesis is that  mu0 <= 2. We begin with computing the test statistic.

xbar = 2.1             # sample mean 

mu0 = 2                # hypothesized value 

s = 0.3                # sample standard deviation 

n = 35                 # sample size 

t = (xbar-mu0)/(s/sqrt(n)) 

t                      # test statistic 

#We then compute the p-value

pt(t, df = n-1, lower.tail = F) 

#Answer

#The p-value is less than 0.05. Hence, at .05 significance level, we can reject the claim that there is at most 2 grams of saturated fat in a cookie.
#pt(t, df= n-1,lower.tail = FALSE)

#__________________________________________________________________________________________________________
#Two-Tailed Test of Population Mean with Unknown Variance

#Problem

#Suppose the mean weight of King Penguins found in an Antarctic colony last year was 15.4 kg. 
#In a sample of 35 penguins same time this year in the same colony, the mean penguin weight is 14.6 kg. 
#Assume the sample standard deviation is 2.5 kg. At .05 significance level, can we reject the null hypothesis that the mean penguin weight does not differ from last year?

#Solution

#The null hypothesis is that  mu0 = 15.4. We begin with computing the test statistic.

xbar = 14.6            # sample mean 

mu0 = 15.4             # hypothesized value 

s = 2.5                # sample standard deviation 

n = 35                 # sample size 

t = (xbar-mu0)/(s/sqrt(n)) 

t                      # test statistic 

#We then compute the p-value

2*pt(-abs(t), df = n-1) 

#Answer

#The p-value is less than 0.05. Hence, at .05 significance level, we do not reject the null hypothesis that the mean penguin weight does not differ from last year.
