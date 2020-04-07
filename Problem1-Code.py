# Question:  Coins with values 1 through N (inclusive) are placed into a bag.
# All the coins from the bag are iteratively drawn (without replacement) at 
# random. For the first coin, you are paid the value of the coin. 
# For subsequent coins, you are paid the absolute difference between 
# the drawn coin and the previously drawn coin. 
# For example, if you drew 5,3,2,4,1, your payments would be 5,2,1,2,3 
# for a total payment of 13.


# Permutation function for a given list of N numbers
def permutation(args):
    if len(args) == 0: 
        return [] 
    
    if len(args) == 1: 
        return [args] 

    lst = [] 
    for i in range(len(args)): 
        m = args[i] 
        remargs = args[:i] + args[i+1:]
        for p in permutation(remargs): 
            lst.append([m] + p) 
    return lst 

# Total payments function associated with each permutation 
# Finds the Mean and Standard Deviation
def total_payment_mean_std(args):
    import statistics 
    list_payment = []
    total_payment = []
    for i in range(len(args)):
        list_payment_j = []
        #sumpay_j = []
        for j in range(len(args[i])):
            if j == 0:
                list_payment_j.append(args[i][j])
            else:
                list_payment_j.append(abs(args[i][j]-args[i][j-1]))
                total_payment_j = sum(list_payment_j)
        list_payment.append(list_payment_j)
        total_payment.append(total_payment_j)
    mean = statistics.mean(total_payment) 
    std = statistics.stdev(total_payment)
    return(total_payment,mean,std)


# Probability function
def prob(args):    
    count = 0
    for i in args:
        if i >= p0:
            count += 1 
    return count/len(args)



# Test 
N = 10     #N=20
p0 = 45    #p0=160
list_numbers_N = [i for i in range(1,N+1)]

# All permutation
permuted_list_numbers_N = permutation(list_numbers_N)

# All payments 
total_payment_mean_std = total_payment_mean_std(permuted_list_numbers_N)

# Probability
total_payment = total_payment_mean_std[0]
prob(total_payment)
