tif_real_values <- tibble(theta=c(-0.90864, -0.45432,  0.00000,  0.45432), 
						  TI=c(5.07566, 4.56082, 4.74555, 4.78174))
# tmp2 <- 
tibble(lambda_a=c(1, .909), tau_a1=c(-1.527, -1.527), tau_a2=c(-.977, -.977), tau_a3=c(-.183, -.183), tau_a4=c(.606, .606), factorscore=c(.454,.454), var_theta=c(.826, 1), 
	   theta_ik=1-var_theta) %>%
	rowwise() %>%
	mutate(P_1 = pnorm((tau_a1 - lambda_a * factorscore) / sqrt(theta_ik)), 
		   P_2 = pnorm((tau_a2 - lambda_a * factorscore) / sqrt(theta_ik)) - P_1,
		   P_3 = pnorm((tau_a3 - lambda_a * factorscore) / sqrt(theta_ik)) - P_2,
		   P_4 = pnorm((tau_a4 - lambda_a * factorscore) / sqrt(theta_ik)) - P_3,
		   P_5 = 1 - P_4,
		   
		   Q_0 = 0,
		   Q_1 = sum(c(P_1)),
		   Q_2 = sum(c(P_1, P_2)),
		   Q_3 = sum(c(P_1, P_2, P_3)),
		   Q_4 = sum(c(P_1, P_2, P_3, P_4)), 
		   Q_5 = 1, 
		   
		   qpid_1 = (Q_1*(1 - Q_1) - Q_0 * (1 - Q_0))^2 / P_1,
		   qpid_2 = (Q_2*(1 - Q_2) - Q_1 * (1 - Q_1))^2 / P_2,
		   qpid_3 = (Q_3*(1 - Q_3) - Q_2 * (1 - Q_2))^2 / P_3,
		   qpid_4 = (Q_4*(1 - Q_4) - Q_3 * (1 - Q_3))^2 / P_4,
		   qpid_5 = (Q_5*(1 - Q_5) - Q_4 * (1 - Q_4))^2 / P_5,
		   
		   I_i = 3.29 * lambda_a^2 / theta_ik * sum(c(qpid_1, qpid_2, qpid_3, qpid_4, qpid_5))) %>%
	
	select(-matches("tau_|factorscore|var_theta|theta_ik"))
