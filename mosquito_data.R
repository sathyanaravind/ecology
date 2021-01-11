mosquito <- read.csv("mosquito_data.csv")
head(mosquito)
str(mosquito)
summary(mosquito)
mosquito$predator <- factor(mosquito$predator)
?factor

result1 <-glm(cbind(S,K) ~ density+predator+density:predator, data=mosquito,
              family = "quasibinomial")
summary(result1)

plot(S/(S+K)~density, data = mosquito,
     col= as.numeric(predator))

points(predict(result1,type = "response")~
         density,data = mosquito,
       col=as.numeric(predator),
       pch=19)

?lm
curve(exp(2.92553040 + (-0.02585535)*x)/(1+exp(2.92553040 + (-0.02585535)*x)), add=TRUE,lwd=2)

curve(exp(2.92553040-4.639482 + (-0.02585535+0.036193 )*x)/(1+exp(2.92553040-4.639482 + (-0.02585535+0.036193 )*x)),
      add=TRUE,lwd=2, col="red")

curve(exp(2.92553040-5.943534 + (-0.02585535+0.041391 )*x)/(1+exp(2.92553040-5.943534 + (-0.02585535+0.041391 )*x)),
      add=TRUE,lwd=2, col="green")




result2 <-glm(cbind(S,K) ~ density+predator, data=mosquito,
              family = "quasibinomial")
result2 <- update(result1, ~. - density:predator)
result2
anova(result2, result1, test="Chisq")

