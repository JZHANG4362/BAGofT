Generate data: Data_gen.R, Data_gen2.R

GOF test study: 

(The indices `100’, `200’, `800’ correspond to the sample size. 
The index `_1’ corresponds to  Figure 2, misspecified cases, 
and the index `_2’ corresponds to Figure 1, correctly specified cases. )
Setting 1: 
gamma = 1
sim1_100_1_fl.R, sim1_100_2_fl.R,
sim1_200_1_fl.R, sim1_200_2_fl.R,
sim1_800_1_fl.R, sim1_800_2_fl.R,

gamma = 0.5
sim4_100_1_fl.R, sim4_100_2_fl.R,
sim4_200_1_fl.R, sim4_200_2_fl.R,
sim4_800_1_fl.R, sim4_800_2_fl.R,

Setting 2: 
gamma = 1
sim2_100_1_fl.R, sim2_100_2_fl.R,
sim2_200_1_fl.R, sim2_200_2_fl.R,
sim2_800_1_fl.R, sim2_800_2_fl.R,

gamma = 0.5
sim5_100_1_fl.R, sim5_100_2_fl.R,
sim5_200_1_fl.R, sim5_200_2_fl.R,
sim5_800_1_fl.R, sim5_800_2_fl.R,

Setting 3: 
gamma = 1
sim3_100_1_fl.R, sim3_100_2_fl.R,
sim3_200_1_fl.R, sim3_200_2_fl.R,
sim3_800_1_fl.R, sim3_800_2_fl.R,

gamma = 0.5
sim6_100_1_fl.R, sim6_100_2_fl.R,
sim6_200_1_fl.R, sim6_200_2_fl.R,
sim6_800_1_fl.R, sim6_800_2_fl.R,

QQplot under H0: QQ_plots.R

Rejection rates under H1: Rates_H1.R

Trace plot that reflects the relation between number of splits and statistic variation: Mean_Trace_plot.R

Barplot for the counts of the covariates with the largest variable importance: Largest_VI_plot.R