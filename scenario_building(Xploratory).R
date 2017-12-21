library(plotly)
library(ggplot2)
library(tibble)

# ---------Simulation----------------------------------------------

# Set the directory to the working directory, first one if using My Personal Laptop, second one HP Laptop

# setwd("E:/Partition E on Google Drive/05- Dissertation/0E-Simulations/Xploratory FCM 2.0")
 setwd("C:/Users/alizadeh/Google Drive/05- Dissertation/0E-Simulations/Xploratory FCM 2.0")

# Import required functions
source("functions(XploratoryFCM).R")

# Import data (connections and their minimum and maximum weights)
#df <- as.data.frame(read.csv("CollectiveFCM9-28-17_Xtreme.csv"))
df <- as.data.frame(read.csv("Ambidexterity CollectiveFCM Rev8-3.csv"))


# generating adjacency matrices and then save them as rds file.Variables; number of random adjacency matrices or exploratory weights

#adj_mat_raw <- S_B(df,10)
#saveRDS(adj_mat_raw, '10_adj_mat_raw_Rev8-3.rds')

# If the adjacency matries exist, read the rds file from the directory.

adj_mat_raw <- readRDS("10_adj_mat_raw_Rev8-3.rds")


# Import initial vectors-------

#--Use when IVs include all the concepts in a map--------------
#i_v <- as.matrix(read.csv("10_random_initial_vectors_4of365.csv"))

#--Use when IVs Does NOT include all the concepts in a map-but reads from additional input-------------

iv_unmapped <- as.matrix(read.csv("200000_random_initial_vectors_4of365.csv"))
concepts <- read.csv("Concepts to be included in IVs Rev8-3.csv", check.names = FALSE)
node_name <- subset(concepts,`Included in Initial Vectors`==1)
i_v <- initial_v_mapper(iv_unmapped,adj_mat_raw[[1]],node_name$id)
#colSums(i_v[,"Organizational ambidexterity",drop=FALSE])


#-End of -Use when IVs Does NOT include all the concepts in a map-but reads from additional input-------------


# run FCM. Variables; Number of initial vectors, condition for stop, global squashing function used, clamp option
start <- Sys.time()
res <- lapply(1:200000, function(x) FCM(i_v[x,], adj_mat_raw, 8, 0.001, funct = 'tanh', clamp = T))
duration <- Sys.time()-start



# save the last iteration results for all initial vectors before filtering.
agg_i_res <- do.call(rbind, res)
agg_i_res <- agg_i_res[row.names(agg_i_res)=="r_8",]
write.csv(agg_i_res, 'last_iteration_matrix.csv')



#--------------Filtering the scenarios------------------------------------



cond <- function(x){
  filter <- ifelse(x["r_8","Organizational ambidexterity"] >= 0.9 &
                   x["ro","Organizational ambidexterity"] >= 0.9   
                  #x["r_8","Exploratory Innovation"] >= 0.5 &
                 # x["ro","Exploratory Innovation"] >= 0.50 &
                 # x["r_8","Exploitative innovations"] >= 0.5 & 
                 # x["ro","Exploitative innovations"] >= 0.50
                    ,TRUE, FALSE)
  return(filter)
}

filtered_res <- res[sapply(res, function(x) cond(x))]

agg_res <- do.call(rbind,filtered_res)
write.csv(agg_res, 'filtered_scenarios.csv')



# -----------Prepare data for visualization---------------------------
#if reading from previous results

res<-read.csv('last_iteration_matrix.csv', check.names = FALSE)



#--------A-Only for expected behavius>   only for 1 initial vector--------


# n in res[[n]] determines the result of nth intial vector in the i_v input.
#t <- as.data.frame(res[[1]])

# colnames(t) <- u_c

#t <- tibble::rownames_to_column(t[,colSums(t) != 0], 'res_iter')
#t <- reshape2::melt(t,'res_iter')

#var_name <- unique(t$variable)


# Plot results for expected--------

# plot_ly(t, x = ~ res_iter, y = ~ value, color = ~ variable) %>% add_lines(line = list(shape = 'hv'))

#lapply(1:length(var_name),
#       function(x) plot_ly(t %>% dplyr::filter(variable == var_name[x]) ,
#                           x = ~ res_iter, y = ~ value, name = var_name[x]) %>% layout(yaxis = list(range = c(-1,1)))%>% 
#        add_lines(line = list(shape = 'hv'))) %>% subplot(nrows = 10, shareX = TRUE)
 

#-----------End of A--Only for expected behavius>   only for 1 initial vector--------



#-----------B-Code for showing all results of part to part ----------------

#lapply(11:15,
 #      function(x) plot_ly(t %>% dplyr::filter(variable == var_name[x]),
  #                         x = ~ res_iter, y = ~ value, name = var_name[x]) %>% add_lines(line = list(shape = 'hv'))) %>%
  #subplot(nrows = 10, shareX = TRUE)

#-----------End of B-Code for showing all results of part to part ----------------




# --------------C-Heat MAP-1------------------------------------------------------------------

# plotly 2
p <- plot_ly(x = agg_i_res[,"Exploitative innovation"], y = agg_i_res[,"Exploratory innovation"]) %>% 
  add_histogram2dcontour(contours = list(coloring = "fill",
                                         start = 0, end = 100, size = 1), histnorm = "percen")%>% 
  layout(xaxis = list(title = "Exploitative Innovation", range = c(-1,1)), 
         yaxis = list(title = "Exploratory Innovation", range = c(-1,1)),
         annotations = list(yref = "paper", xref = "paper",
                            y = 1.05, x = 1.2, 
                            text = "Percentage", showarrow = F))
p


# --------------C-Heat MAP-2------------------------------------------------------------------


p %>% add_histogram2d(zsmooth = "best")%>% 
    layout(xaxis = list(title = "Exploitative Innovation", range = c(-1,1)), 
         yaxis = list(title = "Exploratory Innovation", range = c(-1,1)),
         annotations = list(yref = "paper", xref = "paper",
                            y = 1.05, x = 1.2, 
                            text = "Frequancy", showarrow = F))
p

# -------------End of C-Heat MAP-1 and 2------------------------------------------------------------------

# --------------D-Black Hole MAP-------------------------------------------------------------------
# ggplot

p <- ggplot(as.data.frame(agg_i_res),aes(x=`Exploitative innovation`,y=`Exploratory innovation`))+
  stat_density2d(aes(alpha=..level..), geom="polygon") 
ggplotly(p)

# -------------End of D-Black Hole MAP-------------------------------------------------------------------

# ------------- E-Cluster Map -------------------------------------------------------------------

# ggplot & plotly

p <- ggplot(as.data.frame(agg_i_res),aes(x=`Exploitative innovation`,y=`Exploratory innovation`)) + 
  geom_point(alpha = 0.05) + 
  geom_density_2d() + 
  theme(panel.background = element_rect(fill = '#ffffff')) 
  #ggtitle("2D density plot with scatterplot overlay")

p <- ggplotly(p)
p

# ------------End of E-Cluster Map -------------------------------------------------------------------

# ------------- F-  Scatter Plot-----------------------------------------------------------------

# ggplot
p <- plot_ly(x = agg_i_res[,"Exploitative innovation"], y = agg_i_res[,"Exploratory innovation"]) 
p %>% add_markers(alpha = 0.05, marker = list(size = 20))%>% 
  layout(xaxis = list(title = "Exploitative Innovation"), 
         yaxis = list(title = "Exploratory Innovation"))

# ------------End of F-  Scatter Plot-----------------------------------------------------------------


#-------G- Topology Map---------------------
  assign_z <- function(x,y){
    if(x<0 | y<0){
      z <- 0
    }else{
      z <- exp(-20*exp(-10*x*y))
    }
    return(z)
  }

testPts <- expand.grid(x=seq(-1,1,0.01),y=seq(-1,1,0.01))
testPts$z <- mapply(function(x,y) assign_z(x,y),testPts$x,testPts$y)
pp <- ggplotly(ggplot(testPts,aes(x=x,y=y,z=z))+stat_contour(aes(color=factor(..level..)))+
                 guides(color = guide_legend(title = "OA Value")))%>% layout(legend = list(x = 1.02, y = 1))


p <- ggplot()+geom_point(data = as.data.frame(agg_i_res), aes(x = `Exploitative innovation`, y = `Exploratory innovation`))+
  stat_contour(data=testPts,aes(x=x,y=y,z=z, color=factor(..level..)))+
  guides(color = guide_legend(title = "OA Value"))+
  scale_x_continuous(limits = c(-1,1))+scale_y_continuous(limits = c(-1,1))
ggplotly(p)%>% 
  layout(xaxis = list(title = "Exploitative Innovation", range = c(-1,1)), 
         yaxis = list(title = "Exploratory Innovation", range = c(-1,1)),
         legend = list(x = 1.02, y = 1))

#------End of G- Topology Map---------------------
