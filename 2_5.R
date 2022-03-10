#5a

data = read.table("awards.txt", header=TRUE)
head(data)

hist(data$num_awards)

#needed because otherwise programm would be numeric (now it is a factor)
data <- within(data, {prog <- factor(prog, levels=1:3, labels=c("General", "Academic", "Vocational"))})

awards=glm(num_awards~prog, family=poisson,data=data)

#Investigate whether the type of program influences the number of awards by 
#performing a Poisson regression, without taking variable math into account
summary(awards)
#general (1) has a p value<0.05 (0.005), academic (2) has a p value<0.05 (0.001)
#vocational has a p value>0.05 (0.072)

#Estimate the numbers of awards for all the three types of program. 
coef_awards_gen = exp(coef(awards)[1]); coef_awards_aca = exp(coef(awards)[1] + coef(awards)[2]);coef_awards_voc = exp(coef(awards)[1] + coef(awards)[3])
table_awards = table(data[1:2])
persons_1 = sum(table_awards[1:7]); persons_2 = sum(table_awards[8:14]); persons_3 = sum(table_awards[15:21])
persons_1*coef_awards_gen; persons_2*coef_awards_aca; persons_3*coef_awards_voc
#estimation number of awards per programm is program 1 (general): 26, 
#programm 2 (academic): 123, programm 3 (vocational): 45

#Which program type is the best for the number of awards for this model?
coef_awards_gen; coef_awards_aca; coef_awards_voc
#coeffecients per programm program 1 (general): 0.578, programm 2 (academic): 1.171 
#programm 3 (vocational): 0.9. So programm 2(academic)


####without mode####
#Also possible to estimate without the model does give the same answer
# number of awards per programm, but does not use model
awards_prog =xtabs(data[1:2])
awards_prog[1]; awards_prog[2]; awards_prog[3]

# average award per person for every programm
awards_prog[1]/persons_1; awards_prog[2]/persons_2; awards_prog[3]/persons_3
####without model####

#5b


#5c
#Now include predictor math into analysis and investigate the influence of the 
#explanatory variables prog and math (and their interaction) on the numbers of awards.

awards2=glm(num_awards~prog*math, family=poisson,data=data)
summary(awards2)
#the interactions have a p-vaue>0.05 and therefore it has an insignficant 
#influence on the amount of awards a person would get. So model without interaction

awards3=glm(num_awards~prog+math, family=poisson,data=data)
summary(awards3)
#the three programms have a p-value<0.05 so they are all significant 
#1 (p_general)=6.06e-07, 2 (p_aca)=0.0440, 3 (p_voc)=0.0232 and the math 
#variable is also significant p_value = 1.80e-10


#Which program type is the best for the number of awards? 

coef_awards3_gen = exp(coef(awards3)[1]); coef_awards3_aca = exp(coef(awards3)[1] + coef(awards3)[2]);coef_awards3_voc = exp(coef(awards3)[1] + coef(awards3)[3])
coef_awards3_gen; coef_awards3_aca; coef_awards3_voc

#coeffecients per programm program 1 (general): 0.093, programm 2 (academic): 0.147
#programm 3 (vocational): 0.164. So programm 3 (Vocational). This is different
#from when the math variable was not taken in account, since than programm 2
#(academic) had the largest coeffecient


#Estimate the numbers of awards for the vocational program and math score 55.

#when I use just the coefficients
coef_awards3_gen = exp(coef(awards3)[1]+coef(awards3)[3]+coef(awards3)[4]*55)
coef_awards3_gen 
#A person with prog = 3 (vocational), math = 55 is predicted to have 1.17 awards.

#when I use predict
newdata = data.frame(prog = 'Vocational', math = 55)
newdata

predict(awards3, newdata = newdata, type = "response")
#A person with prog = 1 (vocational), math = 55 is predicted to have 1.17 awards.

#both methods same answer so probably right

