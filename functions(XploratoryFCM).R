library(tidyverse)

# unimizer builds adjacency matrix by randomly picking value for concept from provided range.
# x is input dataframe with four column the first two columns refer to connection and conveys source and target 
# for the connection while the column three and four refer two min and max weight of connection respectively.

unimizer <- function(x){
  
  # creating output data frame with the connection source and target info as well as weight of the connection.
  output <- data.frame(matrix(nrow=nrow(x),ncol = 3 , dimnames = list(NULL,c('Source','Target','Value'))))
  
  # Populating output data frame.
library(sfsmisc)


  output[,1:2] <- x[,1:2]
  
  # randomly pick weight for connection with regards to min and max value of the connection.
  output[,3] <- as.data.frame(mapply(function(x,y) runif(1,x,y), x[,3], x[,4]))
  
  #Building adjacency matrix
  
  # reshape the ouput dataframe into adjacency matrix. Row name is source, col name is target and cell value is Value.
  adj_m <- reshape2::dcast(output, Source ~ Target , sum, value.var = 'Value')
  adj_m <- as.matrix(adj_m %>% remove_rownames %>% column_to_rownames(var = 'Source'))
  
  # Gathering all the concepts from provided links.
  u_c <- unlist(unique(lapply(unlist(df[,c("Source","Target")]), as.character), use.names = FALSE))
  
  # Building zero matrix with m*m dimension where m is the size of concepts.
  adj_mat <- matrix(0, nrow = length(u_c), ncol = length(u_c), dimnames = list(u_c, u_c))
  
  # extracting the common row and column names between the adj_mat and adj_m.
  row_name <- u_c[u_c %in% row.names(adj_m)]
  col_name <- u_c[u_c %in% colnames(adj_m)]
  
  # populate adjacency matrix based on the randomized value.
  adj_mat[row_name, col_name] <- adj_m[row_name, col_name]
  
  # return adjacency matrix
  return(adj_mat)
}

# this function generates all the possible combination for the nodes that chosen to be triggered.
# i_inputs are refering to the nodes that are going to randomly ignited.

initial_v_mapper <- function(i_v, adj_mat,node_name){
  
  iv_mapped <- matrix(0, nrow = nrow(i_v), ncol = ncol(adj_mat), dimnames = list(NULL,colnames(adj_mat)))
  iv_mapped[,colnames(iv_mapped) %in% node_name] <- i_v
  
  return(iv_mapped)
}

# S_B function runs unimizer multiple time and runs FCM for each scenario for a given initial state. And it returns
# the average of all the scenario's results.

S_B <- function(x,n){
  
  scenarios <- list()
  scenarios <- replicate(n, unimizer(x), simplify = FALSE)
  
  return(scenarios)
}

# the FCM_base function runs FCM model and returns the results for all of scenarios (adjacency matrices). 
# The inputs are including:
# i_s: initial vector
# mat: adjacency matrix
# funct: squashing function - choices is either tanh or sigmoid.
# iter: the number of iteration that FCM model should run.
# eps: very small number which indicates the difference between two consecetive iteration results is significant if
#      it is greater than this value.
# clamp: the default state is TRUE and shows that chosen nodes while keep their initial states through analysis.

FCM <- function(i_s, mat, iter, eps, funct = 'tanh', clamp = TRUE){
  
  # this creates list to store each iterations results for all intial vectors.
  o_r <- list()
  
  # identify the column index for Exploratory innovation, Exploitative innovation, and 
  # Organizational ambidexterity to apply a given squashing function for organizational ambidextirty.
  
  x_index <- grep("Exploratory.Innovation", colnames(i_s))
  y_index <- grep("Exploitative.innovations", colnames(i_s))
  z_index <- grep("Organizational.ambidexterity", colnames(i_s))
  
  # this loop goes through all scenarios and runs FCM for with regards to each scenario(random adjacency matrix).
  for(j in 1:length(mat)){
    
    # initial vector will be stored as iteration zero result.
    result <- matrix(i_s, nrow=1)
    
    
    # FCM model will run multiple times either equal to determined iteration or 
    # will be stoped if epsilon condition will meet.
    for(i in 1:iter){
      
      # multiplying initial vector or previous iteration results to adjacency matrix.
      r_i <- result[i,] %*%  mat[[j]]
      
      # applying tanh squashing function.
      if(funct == 'tanh'){
        r_i <- sapply(r_i, function(x) (tanh(2*x)))
        
        # this replace the tanh squashing function with exponential function for organizational ambidextrity.
        if(r_i[79]<0 | r_i[87]<0){
          r_i[44]<-0
        }else{
          r_i[44] <- exp(-20 * exp (-10 * r_i[79] * r_i[87]))
        }
      }
      
      # applying tanh squashing function.
      if(funct == 'sigmoid'){
        r_i <- sapply(r_i, function(x) (1/(1 + exp(-4*x))))
      }
      
      # if clamp == TRUE this will return the chosen node to their initial state.
      if(clamp){
        
        r_i[which(i_s != 0)] <- result[i, which(i_s != 0)]
      }
      
      
      # this will aggregate all the result in each iteration for a given initial vector.
      result <- rbind(result, r_i)
      
      # this will check the epsilon condition. If the eps value is met exit the loop
      # if (mean(abs(result[i, ] - r_i)) <= eps)
      # break
    }
    
    # this will aggregate all the result in each iteration for all of initial vectors.
    # rownames(result) <- paste("r",0:(nrow(result)-1), sep = "_")
    colnames(result) <- colnames(mat[[1]])
    o_r[[j]] <- result
  }
  
  avg_res <- Reduce('+', o_r) / length(o_r)
  
  min_res <- pmin(o_r[[1]],o_r[[2]])
  max_res <- pmax(o_r[[1]],o_r[[2]])
  
  for(i in 3:length(o_r)){
    min_res <- pmin(min_res,o_r[[i]])
    max_res <- pmax(max_res,o_r[[i]])
  }
  
  robust_value <- 1 - ((max_res - min_res)/2)
  robust_op <- apply(robust_value, 2, min)
  
  op <- rbind(avg_res[1,],avg_res[nrow(avg_res),],robust_op)
  rownames(op) <- c("iv","r_8","ro")
  
return(op)
}

# FCM_avg function takes average of FCM output of all the scenarios.
#FCM_avg <- function(i_s, mat, iter, eps, funct , clamp){
  
#  FCM_res <- FCM_base(i_s, mat, iter, eps, funct, clamp)
#  avg_res <- Reduce('+', FCM_res) / length(FCM_res)
#  return(avg_res)
#}


# the FCM_robust function runs FCM model and returns the  output for N scenario matrices. The inputs are including:
# i_s: initial vector
# mat: adjacency matrix
# funct: squashing function - choices is either tanh or sigmoid.
# iter: the number of iteration that FCM model should run.
# eps: very small number which indicates the difference between two consecetive iteration results is significant if
#      it is greater than this value.
# clamp: the default state is TRUE and shows that chosen nodes while keep their initial states through analysis.

#FCM_robust <- function(i_s, mat, iter, eps, funct, clamp){
  
#  FCM_res <- FCM_base(i_s, mat, iter, eps, funct, clamp)
#  min_res <- pmin(FCM_res[[1]],FCM_res[[2]])
#  max_res <- pmax(FCM_res[[1]],FCM_res[[2]])
  
#  for(i in 3:length(FCM_res)){
#    min_res <- pmin(min_res,FCM_res[[i]])
#    max_res <- pmax(max_res,FCM_res[[i]])
#  }
  
#  robust_value <- 1 - ((max_res - min_res)/2)
#  robust_op <- as.matrix(apply(robust_value, 2, min))

#  return(robust_op)  
#}






