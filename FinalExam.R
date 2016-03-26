library(pacman)
p_load(Hmisc, magrittr, caret, corrplot, rpart, rattle, MASS, ggbiplot, AppliedPredictiveModeling, pairsD3, conjoint)
#load data
Baseball <- read.csv("~/Data_Science/MarketingAnalytics/Baseball.csv", stringsAsFactors=FALSE)
#clean up salary, chr to numeric
Baseball$SALARY <- as.numeric(Baseball$SALARY)
#aggregate player year/salaries
clean_df <- aggregate(cbind(G,AB, R, H, X2B, X3B, HR, RBI, SB, CS, BB, SO, IBB, HBP, SH, SF, GIDP, SALARY) ~ playerID, sum, data = Baseball)
#Check out data set
str(clean_df)
#filter out observations with less than 240 games played
filtered_df <- clean_df[clean_df$G>240,] 
#Remove outlier jennide01 with highest salary and performance not commensurate with salary.
filtered_df <- filtered_df[filtered_df$SALARY<35000000,]
#Create Logistic Dataset
#Check out the data set
str(filtered_df)
#Plot hist to see distribution of games played
hist(filtered_df$G, main = "Histogram of Games Played")
#check plot of games vs salary(target)
filtered_df %>% with(plot(G, SALARY))

#Observe ea variable vs target to look for relationships
regVar <- c("G","AB", "R", "H", "X2B", "X3B", "HR", "RBI", "SB", "CS", "BB", "SO", "IBB", "HBP", "SH", "SF", "GIDP")
featurePlot(x = filtered_df[, regVar],
            y = filtered_df$SALARY,
            plot = "scatter",
            type = c("p", "smooth"),
            span = .5,
            layout = c(3, 6), main="Univariate Plots vs Salary")
#Check for non-zero variance
nzv <- nearZeroVar(filtered_df, saveMetrics= TRUE)#None observed
nzv#non-zero variance not a problem
comboInfo <- findLinearCombos(filtered_df[,-1])
comboInfo#none
#Correlation analysis
descrCor <-  cor(filtered_df[,-1])
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .8)
highlyCorDescr <- findCorrelation(descrCor, cutoff = .90)
#Correlation Plot
identifyPlot1 <- corrplot(descrCor, type="lower", tl.pos ="ld", tl.cex = .58)


#Looks like we could use some PCA to reduce variance
# #log transform to remove undue effects of large outlier values
# log.ball <- log(filtered_df[,2:19])
ball.pca <- prcomp(filtered_df[,2:18], center = TRUE, scale = TRUE)
print(ball.pca)
plot(ball.pca, type = "l")


summary(ball.pca)
princomp_ball <- predict(ball.pca, newdata = filtered_df[,2:19])
princomp_salry <- as.data.frame(cbind(princomp_ball[,1:10], SALARY =filtered_df$SALARY))

g <- ggbiplot(ball.pca, obs.scale = 2, var.scale = 2, ellipse = TRUE, circle = TRUE)
g+scale_color_discrete(name = '')
######################################

kmean.ball <- kmeans(filtered_df[,4:18], 4, nstart = 25)
plot(filtered_df[c("R", "RBI")], col = kmean.ball$cluster)

#Pairs Plot with class set to cluster
transparentTheme(trans = .6)
featurePlot(x = filtered_df[, regVar[1:5]],
            y = as.factor(kmean.ball$cluster),
            plot = "pairs",
            main="k-means pair plots",
            auto.key = list(columns = 4))
featurePlot(x = filtered_df[, regVar[6:11]],
            y = as.factor(kmean.ball$cluster),
            plot = "pairs",
            main="k-means pair plots",
            auto.key = list(columns = 4))
featurePlot(x = filtered_df[, regVar[12:18]],
            y = as.factor(kmean.ball$cluster),
            plot = "pairs",
            span = .5,
            main="k-means pair plots",
            auto.key = list(columns = 4))


pairsD3(filtered_df[2:6], group =kmean.ball$cluster, tooltip = )

points(kmean.ball$centers[,c("R", "RBI")], col=1:4, pch = 8, cex = 2)
kmean.ball$cluster
kmean.ball$centers
kmean.ball$totss
kmean.ball$iter
######################################
smp_size <- floor(0.7*nrow(filtered_df))
set.seed(143)
train_ind <- sample(seq_len(nrow(filtered_df)), size = smp_size)
train <- filtered_df[train_ind,]
test <- filtered_df[-train_ind,]
rpart.full <- rpart(SALARY~G+AB+R+H+X2B+X3B+HR+RBI+SB+CS+BB+SO+IBB+HBP+SH+SF+GIDP, data=train)
printcp(rpart.full)
fancyRpartPlot(rpart.full)


lm.ball <- lm(SALARY~., data = filtered_df[,2:19])
lm.ball2 <- lm(SALARY~R+RBI+BB+CS, data = filtered_df[,2:19])
summary(lm.ball)
summary(lm.ball2)
step <- stepAIC(lm.ball, direction = "backward")
step1 <- stepAIC(lm.ball, direction = "forward")
step2 <- stepAIC(lm.ball, direction = "both")
anova(step, step1, step2)
################################


logistic_data <- Baseball[,1:24]
logistic_data <- logistic_data[logistic_data$G>80,]
logistic_data <- cbind(logistic_data, HITTER = as.numeric(logistic_data$HR>10))
boxplot(logistic_data$HITTER, logistic_data$SALARY)
glm.ball <- glm(formula=HITTER~SALARY, family = binomial(link = "logit"), data = logistic_data)
summary(glm.ball)


########################

attribute <- list(
    Tenure=c("Veteran", "Rookie", "Journeyman"),
    HitStyle=c("Hits for Power", "Hits for Contact"),
    Mindedness=c("Offensive Minded", "Defensive Minded"),
    Passion=c("For the game", "For teammates", "For winning")
)
#develop profiles
profiles <- expand.grid(attribute)

design <- caFactorialDesign(data = profiles,type = "fractional", cards = 10)
nrow(design)
set.seed(45)

#synthesize respondent data matrix
matrix.new <- matrix(data = 0, nrow = 50, ncol = 10)
#iterate with sample data
for (i in 1:nrow(matrix.new)){
    
    x <- sample(1:10)
    matrix.new[i,] <- x
}
preferences <- matrix.new
#pre_vector <- as.vector(preferences)
profiles
attribute_levels <- as.vector(unlist(attribute))
#Run Conjoint function
Conjoint(preferences, design, attribute_levels)


