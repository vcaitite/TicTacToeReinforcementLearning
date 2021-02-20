# Creating the model 
#install.packages("ReinforcementLearning")

#ReinforcementLearning is released under the MIT License
#Copyright (c) 2019 Nicolas Pröllochs & Stefan Feuerriegel
library(ReinforcementLearning)
control <- list(alpha = 0.2, gamma = 0.4, epsilon = 0.1)
modelottt <- ReinforcementLearning(tictactoe, s = "State", a = "Action", r = "Reward",
                                   s_new = "NextState", iter = 2, control = control)

