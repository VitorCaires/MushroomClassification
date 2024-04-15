library(tidyverse)
library(caret)
library(randomForest)

mushrooms <- read.csv('mushrooms.csv')
str(mushrooms)
summary(mushrooms)

#Construção de gráfico de colunas para entender melhor a distribuição das variáveis
pivot <- pivot_longer(mushrooms, cols = !c("class"), names_to = "specifications") %>%
  count(class, specifications, value)
ggplot(data = pivot, mapping = aes(x=n, fill = value, y =specifications))+
  geom_col()+
  facet_wrap(~class)+
  theme_minimal()

#Limpeza e transformação dos dados
mushrooms$veil.type <- NULL
colSums(mushrooms=='?')
prop.table(table(mushrooms$stalk.root))*100
mushrooms[mushrooms=='?'] <- NA
mushrooms <- data.frame(lapply(mushrooms[,], as.factor))
mushrooms <- na.omit(mushrooms)

#Particionando a amostra para "treino" e teste
set.seed(123)
trainId <- sample(1:nrow(mushrooms), size=round(0.8*nrow(mushrooms)), replace = FALSE)
train <- mushrooms[trainId, ]
test <- mushrooms[-trainId, ]

#Criação do modelo de Random Forest e extração de informações importantes
rf <- randomForest(class~., data=train)
print(rf)
ptrain <- predict(rf, train)
confusionMatrix(ptrain, train$class)
ptest <- predict(rf, test)
confusionMatrix(ptest,test$class)
varImpPlot(rf, sort = T, n.var = 10, main = "Top 10 - Variable Importance")
importance(rf)
hist(treesize(rf))
varUsed(rf)
