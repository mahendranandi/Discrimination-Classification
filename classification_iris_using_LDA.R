## MAHENDRA NANDI    RKMVERI     
## MVS PROJECT OF CLASSIFICATION EITH IRIS DATASET 
## INSTRUCTOR SUDIPTA DAS
## MAY 2021
 
# importing libraries
#####

library(MASS)
library(ggplot2)
library(dplyr)
#library(bivariate)
#library(devtools)
library(klaR)
library(corrplot)
#####
#----------------------------------------
                # loading dataset
#####

head(iris)   
x1=iris[,2]
x2=iris[,4]
y=iris[,5]

setosa=iris[iris[5]=="setosa",c(2,4)]
versicolor=iris[iris[5]=="versicolor",c(2,4)]
virginica=iris[iris[5]=="virginica",c(2,4)]

# description of the data
#   it has 150 total rows of three classes each of 50 data with 4 other column of 
#   petal length, petal width, sepal length and sepal width
#   [ Species:  setosa  versicolor  virginica ]
#   column names: "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"  "Species" 

#pairs(~Sepal.Width+ Petal.Width,
#      col = factor(iris$Species), pch = 19, data = iris,main="Scatter plots")

# iris%>%ggplot(aes(x=Sepal.Width,y=value,fill=Species))+
#   geom_violin(alpha=0.5)+geom_boxplot(width=0.1,fill="white")+
#   facet_wrap(~variable, scales = "free_y",nrow = 2)

# corrplot(cor(iris[,c(2,4)]), type = "upper", order = "hclust")#,
#        tl.col = "black", tl.srt = 90)

# library(psych)
# #install.packages("psych")
# corPlot(setosa[,1:2], cex = 1.2)
# corPlot(versicolor[,1:2])
# corPlot(setosa[,1:2], cex = 1.2)


# corrplot(cor(setosa[,1:2]),        # Correlation matrix
#          method = "shade", # Correlation plot method
#          type = "full",    # Correlation plot style (also "upper" and "lower")
#          diag = TRUE,      # If TRUE (default), adds the diagonal
#          tl.col = "black", # Labels color
#          bg = "white",     # Background color
#          title = "",       # Main title
#          col = NULL)

## separating the dataset according to 3 different species naming with the  Species

## NOTE THAT HERE THE COLUMN 2 AND 4 ARE OF OUR INTEREST ACCORDING TO THE QUESTION 

 
# cov_setosa=cov(setosa[,-3]);cov_setosa
# cov_versicolor=cov(versicolor[,-3]);cov_versicolor
# cov_virginica=cov(virginica[,-3]);cov_virginica
#####



#-----------------------------------------
                          ## a ## plotting the whole data #
#####

frquency_plot=iris%>%ggplot(aes(x=Species,fill=Species))+geom_bar()+
  labs(y="Frequency",title = "Frequencies of observations of different spcies")


Species=unique(iris[5])
plot_3_species <- ggplot(iris) +
  geom_point(aes(x=iris[,2],y=iris[,4], color = Species,shape=Species),size = 2) +
  labs(y="Sepal Width for all 3 Species",x="Petal Width for all 3 Species",
       title = "scatter plot of sepal width and petal width")


## Box plot of each two feature for each three classes ##
# try to add normal distribution curve
box_sepal=ggplot(iris)+
  geom_boxplot(aes(x=iris[,2],y=iris[,5]),color=Species,notch = TRUE,fill="yellow")+
  labs(y="Species",x="sepal Width ",
       title = "box plot of sepal width for three species")

box_petal=ggplot(iris)+
  geom_boxplot(aes(y=iris[,4],x=iris[,5]),color=Species,notch = TRUE,fill="magenta")+
  labs(x="Species",y="Petal Width ",
       title = "box plot of petal width for three species")

vio_sepal=ggplot(iris)+
  geom_violin(aes(x=iris[,2],y=iris[,5]),fill="yellow")+
  labs(y="Species",x="sepal Width ",
       title = "violin plot of sepal width for three species")

vio_petal=ggplot(iris)+
  geom_violin(aes(y=iris[,4],x=iris[,5]),fill="magenta")+
  labs(x="Species",y="Petal Width ",
       title = "violin plot of petal width for three species")

#####
frquency_plot
plot_3_species

box_sepal
vio_sepal

box_petal
vio_petal



#####



#-----------------------------------------
       ## plotting individual species and showing the bivariate normal shape

#####


## plotting individual species and showing the bivariate normal shape
# individua

#plot_setosa <- ggplot(setosa) +
#  geom_point(aes(x=setosa[,1],y=setosa[,2], color = "red"),size = 2) +
#  ylab("Sepal Width of setosa") + xlab("Petal Width of setosa")
#plot_setosa
setosa1=iris[iris[5]=="setosa",c(2,4,5)]
versicolor1=iris[iris[5]=="versicolor",c(2,4,5)]
virginica1=iris[iris[5]=="virginica",c(2,4,5)]


personal_theme <- function(){
  theme_bw() +
    theme(axis.title=element_text(size=rel(1.5)),
          plot.title=element_text(size=rel(1.5), hjust=0.5)) 
}
#
plot_setosa=ggplot(setosa1,aes(x=setosa[,1],y=setosa[,2],shape=Species))+
  geom_point(color = "red", size = 5)+stat_ellipse()+
  geom_point(aes(x=setosa[,1],y=setosa[,2]),size = 2)+
  geom_point(aes(x=versicolor[,1],y=versicolor[,2]),size = 1)+
  geom_point(aes(x=virginica[,1],y=virginica[,2]),size = 1)+
  ylab("Sepal Width for all 3 Species") +
  xlab("Petal Width for all 3 Species") +
  ggtitle("scatter plot fitted with ellipse") +
  personal_theme()

#
plot_versicolor=ggplot(versicolor1,aes(x=versicolor[,1],y=versicolor[,2],shape=Species))+
  geom_point(color = "green", size = 5)+stat_ellipse()+
  geom_point(aes(x=setosa[,1],y=setosa[,2]),size = 1)+
  geom_point(aes(x=versicolor[,1],y=versicolor[,2]),size = 1)+
  geom_point(aes(x=virginica[,1],y=virginica[,2]),size = 1)+
  # geom_point(aes(x=3.5,y=1.75),size = 8)+
  ylab("Sepal Width for all 3 Species") + 
  xlab("Petal Width for all 3 Species") +
  ggtitle("scatter plot fitted with ellipse") 

#
plot_virginica=ggplot(virginica1,aes(x=virginica[,1],y=virginica[,2],shape=Species))+
  geom_point(color = "blue", size = 5)+stat_ellipse()+
  geom_point(aes(x=setosa[,1],y=setosa[,2]),size = 1)+
  geom_point(aes(x=versicolor[,1],y=versicolor[,2]),size = 1)+
  geom_point(aes(x=virginica[,1],y=virginica[,2]),size = 1)+
  ylab("Sepal Width for all 3 Species") + 
  xlab("Petal Width for all 3 Species") +
  ggtitle("scatter plot fitted with ellipse") 


# results here(below one)
#####
# par(mfrow=c(1,1))
plot_setosa
cor_setosa=corrplot.mixed(cor(setosa[,1:2]),lower = "number",upper = "circle",tl.col = "black")
cor_setosa
cov_setosa=cov(setosa);cov_setosa

n_datadensity<-kde2d(setosa[,1],setosa[,2],n=1000)
hm_col_scale<-colorRampPalette(c("black","blue","green","orange","red"))(1000)
contour_setosa=image(n_datadensity$z,col=hm_col_scale,zlim=c(min(n_datadensity$z),
  max(n_datadensity$z)),xlab = "petal width",ylab="sepal width")
  text(0.57,0.6,"Most Frequent",col="black",cex=0.8)
  title("Contours of density for the setosa species")

#
plot_versicolor
cor_versicolor=corrplot.mixed(cor(versicolor[,1:2]),lower = "number",upper = "circle",tl.col = "black")
cor_versicolor
cov_versicolor=cov(versicolor);cov_versicolor

n_datadensity<-kde2d(versicolor[,1],versicolor[,2],n=1000)
hm_col_scale<-colorRampPalette(c("black","blue","green","orange","red"))(1000)
contour_versicolor=image(n_datadensity$z,col=hm_col_scale,zlim=c(min(n_datadensity$z),
  max(n_datadensity$z)),xlab = "petal width",ylab="sepal width")
  text(0.57,0.6,"Most Frequent",col="black",cex=0.8)
  title("Contours of density for the versicolor species")


#
plot_virginica
cor_virginica=corrplot.mixed(cor(virginica[,1:2]),lower = "number",upper = "circle",tl.col = "black")
cor_virginica
cov_virginica=cov(virginica);cov_virginica

n_datadensity<-kde2d(virginica[,1],virginica[,2],n=1000)
hm_col_scale<-colorRampPalette(c("black","blue","green","orange","red"))(1000)
image(n_datadensity$z,col=hm_col_scale,zlim=c(min(n_datadensity$z),
  max(n_datadensity$z)),xlab = "petal width",ylab="sepal width")
  text(0.57,0.6,"Most Frequent",col="black",cex=0.8)
  title("Contours of density for the virginica species")

#####



##################*************************************************************************
install.packages("ggpubr")
#install.packages("ggarrange")
library(ggpubr)
mahiA_D <- ggarrange(a,b,c,d,labels=c("hysrehydrjtydjfgjytjtdjbv",
                                      "hihh","ggu","jdsnfje","uknvr"),ncol=2,nrow=2);mahiA_D
ggarrange(plot_setosa,plot_versicolor)

#################$$*************************************************************************

#----------------------------------------
#=========== Classification through LDA with PLOTS ===================

#####
fit_1=lda(Species ~ iris[,2] + iris[,4],iris,prior = c(1,1,1)/3)
# Predict value of the data on first discriminant
fitvalues <- predict(fit_1,iris)
# Predict class of the data
fitclass <- predict(fit_1)$class
ct_1 <- table(iris$Species, fitvalues$class);ct_1
diag(prop.table(ct_1, 1))
# total percent correct
cat("accuaracy is :", sum(diag(prop.table(ct_1)))*100 ,"%" )
cat("error is : " , (1-sum(diag(prop.table(ct_1))))*100 , "%")



# Create a scatterplot of the discriminant function values
# LDA Axis 1
plot(fitvalues$x[,1], type="n", xlim=c(0,150), ylab=c("LDA Axis 1"),main="classification shown over LD1 values")
text(fitvalues$x[,1], row.names(iris),col=c(as.numeric(fitclass)))
abline(v=0, lty="dotted");abline(h=0, lty="dotted")
legend("topleft", legend=c("setosa","versicolor","virginica"),
       col=c("black","red","green"),pch="*")
# LDA Axis 2
plot(fitvalues$x[,2], type="n", xlim=c(0,150), ylab=c("LDA Axis 2"),main="classification shown over LD2 values")
text(fitvalues$x[,2], row.names(iris),col=c(as.numeric(fitclass)))
abline(v=0, lty="dotted");abline(h=0, lty="dotted")
# legend("topleft", legend=c("setosa","versicolor","virginica"),
#        col=c("black","red","green"),pch="*", xpd=FALSE)
# LDA Axis 1 and LDA Axis 2
plot(fitvalues$x[,1],fitvalues$x[,2],col=c(as.numeric(fitclass)),main="predicted classes shown in LD1 and LD2 space",
     ylab=c("LDA Axis 2"),xlab=c("LDA Axis 1"))
legend("bottomright", legend=c("setosa","versicular","virginica"),
       col=c("black","red","green"),pch="#")

plot(lda.iris, dimen = 1, type = "b",main="projection of cass distributions on LD1")


#
lda.pred <- predict(fit_1, iris[,5])
plot.df <- data.frame(lda.pred$x[,1:2], "Outcome" = iris[,5], 
                      "Prediction" = lda.pred$class)
ggplot(plot.df, aes(x = LD1, y = LD2, color = Outcome, shape = Prediction)) +
  geom_point()+labs(title="misclassification shown in LD1 and LD2 space")


# Plot of Predicted class 
plot(iris[,2],iris[,4],col=factor(fitclass,labels=c(1,2,3)),
     xlab = c("sepal width"),ylab = c("prtal width"),main = "Predicted species in feature space")
legend("topright", legend=c("setosa","versicular","virginica"),
       col=c("black","red","green"),pch="o")

ggplot(plot.df, aes(x = iris[,2], y = iris[,4], color = Outcome, shape = Prediction)) +
  geom_point()+labs(title="misclassification shown in feature space")+
  labs(x="sepal width",y="petal width")


#####




#=========== Classification through LDA with confusion matrix ========

#####

fit_2=lda(Species ~ iris[,2] + iris[,4],iris,prior = c(1,1,1)/3,CV=TRUE)
# Assess the accuracy of the prediction
# percent correct for each category of
ct_2 <- table(iris$Species, fit_2$class);ct_2
diag(prop.table(ct_2, 1))
# total percent correct
cat("accuaracy is :", sum(diag(prop.table(ct_2)))*100 ,"%" )
cat("error is : " , (1-sum(diag(prop.table(ct_2))))*100 , "%")

# Plot of Predicted class 
# plot.df <- data.frame(lda.pred$x[,1:2], "Outcome" = iris[,5], 
#                       "Prediction" = fit_2$class)
# ggplot(plot.df, aes(x = LD1, y = LD2, color = Outcome, shape = Prediction)) +
#   geom_point()+geom_point(aes(x=LD1[71],y=LD2[71]),color="blue")
# 
# fit_2

#####



#--------------------------------
    # mean posteriors and accuracies for individual species
#####
posteriors=fitvalues$posterior
classes <- colnames(posteriors)
res <- do.call(rbind, (lapply(classes, function(x) apply(posteriors[iris[,5] == x, ], 2, mean))))
rownames(res) <- classes
print(round(res, 3)) 

###
dims <- 1:2 # number of (canonical) variables to use
accuracies <- rep(NA, length(dims))
for (i in seq_along(dims)) {
  # print(i)
  lda.pred <- predict(fit, iris, dim = dims[i])
  # print(lda.pred)
  acc <- length(which(lda.pred$class == iris[,5]))/length(iris[,5])
  accuracies[i] <- acc
}
reduced.df <- data.frame("Dim_of_LD_space" = dims, "Accuracy" = round(accuracies, 2))
print(reduced.df)

###
lda.pred <- predict(fit_1, iris[,5])
plot.df <- data.frame(lda.pred$x[,1:2], "Outcome" = iris[,5], 
                      "Prediction" = lda.pred$class)
ggplot(plot.df, aes(x = LD1, y = LD2, color = Outcome, shape = Prediction)) +
  geom_point()#+geom_point(aes(x=ld1,y=ld2))



###

#####



#---------------------------------------
                     ## USING LDA AND QDA
#####
#Applying LDA
lda.iris <- lda(Species ~ iris[,2]+iris[,4], iris)
lda.iris
plot(lda.iris, col = as.integer(iris$Species))
plot(lda.iris, dimen = 1, type = "b")
# plot(lda.iris$, dimen = 0, type = "b")

#LDA Partition Plots

#install.packages("klaR")
colnames(iris)[c(2,4)]
partimat(Species ~ Sepal.Width+Petal.Width, data=iris, method="lda")
# legend("bottomright", legend=c("setosa","versicular","virginica"),
#        col=c("black","red","green"),pch=c("s","v","v"))
# LDA Predictions
lda.train <- predict(lda.iris)
predict_class <- lda.train$class
table(predict_class,iris$Species)

## 
qda.iris <- qda(Species ~ Sepal.Width+ Petal.Width, iris)
qda.iris
partimat(Species ~Sepal.Width+
           Petal.Width, data=iris, method="qda")#+plot(3.5,1.75)

# partimat(Species ~Sepal.Width+
#            Petal.Width, data=iris, method="qda")#+plot(3.5,1.75)

qda.train <- predict(qda.iris)
pred_classes <- qda.train$class
table(pred_classes,iris$Species)

#####



#---------------------------------------
              ## CLASSIFY the POINT (3.5,1.75)
#####
# from the model
test_point=data.frame(3.5,1.75)
colnames(test_point)=c(colnames(iris)[c(2,4)])
object=lda(iris[,c(2,4)],iris[,5])
predict(object, test_point)

object=qda(iris[,c(2,4)],iris[,5])
predict(object, test_point)$class



# ld1=(3.5*(-1.986964))+(1.75*5.477136)
# ld2=(3.5*(2.6800746))+(1.75*0.8169648)
# lda.pred <- predict(fit_1, iris[,5])
# plot.df <- data.frame(lda.pred$x[,1:2], "Outcome" = iris[,5], 
#                       "Prediction" = lda.pred$class)
# ggplot(plot.df, aes(x = LD1, y = LD2, color = Outcome, shape = Prediction)) +
#   geom_point()+geom_point(aes(x=ld1,y=ld2))

#
classify=ggplot(versicolor1,aes(x=versicolor[,1],y=versicolor[,2],shape=Species))+
  geom_point(color = "green", size = 5)+stat_ellipse()+
  geom_point(aes(x=setosa[,1],y=setosa[,2]),size = 1)+
  geom_point(aes(x=versicolor[,1],y=versicolor[,2]),size = 1)+
  geom_point(aes(x=virginica[,1],y=virginica[,2]),size = 1)+
  geom_point(aes(x=3.5,y=1.75),size = 8,color="blue")+
  ylab("Sepal Width for all 3 Species") + 
  xlab("Petal Width for all 3 Species") +
  ggtitle("scatter plot showing the NEW DATA with a larger size")
classify

#####





#---------------------------------------
                   ## answer from the questions
#####
#Not Dustbin

# linear dis. score
d_lin=rep(0,3)
S_pooled=(cov(setosa)+cov(versicolor)+cov(virginica))*(49/147)
X=c(3.5,1.75)
for(i in 1:3) {
  X_bar=apply(iris[((i-1)*50):((i-1)*50+50),c(2,4)], 2, mean)
  d_lin[i]=(t(X_bar)%*%solve(S_pooled)%*%X) - 0.5*(t(X_bar)%*%solve(S_pooled)%*%X_bar) 
  print(d_lin[i])
  if (i==3){
    indicator=which(d_lin==max(d_lin))
    cat("Data point belongs to species: ", as.character(iris[indicator*50,5]),"\n")
  }
}

# square distance differences
D_sqr=matrix(rep(0,9),3)
indicator=0
for (k in 1:3) {
  X_bar_k=apply(iris[((k-1)*50):((k-1)*50+50),c(2,4)], 2, mean)
  flag=0
  for (i in 1:3) {
    X_bar=apply(iris[((i-1)*50):((i-1)*50+50),c(2,4)], 2, mean)
    D_sqr[k,i]=t(X_bar_k-X_bar)%*%solve(S_pooled)%*%X-0.5*(t(X_bar_k-X_bar)%*%solve(S_pooled)%*%(X_bar_k+X_bar))
    if(D_sqr[k,i]<0)
      flag=1
  }
  if(flag==0){
    indicator=k
  }
  if (k==3){
    print(D_sqr)
    cat("Data point belongs to species: ", as.character(iris[indicator*50,5]),"\n")
  }
}

# quadratic dis. score
d_qr=rep(0,3)
for (i in 1:3) {
  X_bar=apply(iris[((i-1)*50):((i-1)*50+50),c(2,4)], 2, mean)
  S=cov(iris[((i-1)*50):((i-1)*50+50),c(2,4)])
  d_qr[i]=((-0.5)*log(det(S)))-(0.5*(t(X-X_bar)%*%solve(S)%*%(X-X_bar)))+log(1/3)
  print(d_qr[i])
}

# prediction of whole data with linear dis. score
predicted_cls=rep("",150)
S_pooled=(cov(setosa)+cov(versicolor)+cov(virginica))*(49/147)
for(k in 1:150) {
  X_point=c(iris[k,2],iris[k,4])
  d_lin_k=rep(0,3)
  max_d_in=0
  max_d=0
  for(i in 1:3) {
    X_bar=apply(iris[((i-1)*50):((i-1)*50+50),c(2,4)], 2, mean)
    d_lin_k[i]=(t(X_bar)%*%solve(S_pooled)%*%X_point) - 0.5*(t(X_bar)%*%solve(S_pooled)%*%X_bar) 
    if(d_lin_k[i]>max_d){
      max_d=d_lin_k[i]
      max_d_in=i
    }
  }
  predicted_cls[k]=as.character(iris[max_d_in*50,5])
  cat("Data point belongs to species: ", as.character(iris[max_d_in*50,5]),"\n")
  if(k==150)
    print(table(predicted_cls,iris[,5]))
}



E<-0
El<-c()
ind<-c()
classcol<-c()
for (i in 1:150){
  holdout<-iris[-i,c(2,4,5)]
  fit_h<-lda(Species~.,data = holdout,prior=c(1,1,1)/3)
  fitval_h<-predict(fit_h,holdout)
  fit_h_class<-fitval_h$class
  E<-E+1-(sum(fit_h_class==holdout$Species)/150)
  ind<-c(ind,i)
  classcol<-c(classcol,iris[,c(2,4)]$Species[i])
  El<-c(El,1-mean(fit_h_class==holdout$Species))
}
EAER<-E/150

cat("when classified by projected squared distance , APER is :   " , (1-sum(diag(prop.table(ct_3))))*100 , "%")
cat("using hold out process, EAER is :   " , EAER*100 , "%")
#####





#----------------------------------------
         #DUSTBIN
#####
library(bivariate)
#install.packages("bivariate")
f <- bmbvpdf (3.5, 40, 10, 10, 6.5, 4, 1, 1) #second component distribution

plot (f, axes = c (TRUE, FALSE), xat = c (3.5, 6.5) )

#########  quadratic d ##
qda.model=qda(Species ~ iris[,2] + iris[,4],iris,prior = c(1,1,1)/3)
#qda.model <- qda(iris, grouping = iris[,5])
qda.preds <- predict(qda.model, iris[,5])
acc <- length(which(qda.preds$class == iris[,5]))/length(iris[,5])
print(paste0("Accuracy of QDA is: " , round(acc, 2)))
predict(qda.model,iris[1,])


mat=matrix(c(-1.986964,2.68,5.47,0.8169),2);mat
vec=matrix(c(3.0,1.8))
mat%*%vec
(-1.986964*3.5)+(2.6800746*0.2)
(5.477136*3.5)+(0.8169648*0.2)

train=iris
lda.iris <- lda(Species ~ ., train)
lda.iris
mat=as.data.frame(matrix(c(2,1,3,1),2))
colnames(mat)=c(colnames(iris)[c(2,4)])

tr <- sample(1:50, 45)
train <- rbind(iris3[tr,,1], iris3[tr,,2], iris3[tr,,3])
test <- rbind(iris3[-tr,,1], iris3[-tr,,2], iris3[-tr,,3])
cl <- factor(c(rep("s",45), rep("c",45), rep("v",45)))
z <- lda(train, cl)
predict(z, test)$class

tr <- sample(1:50, 45)
train <- rbind(iris3[,c(2,4),1], iris3[,c(2,4),2], iris3[,c(2,4),3])
test <- rbind(iris3[-tr,c(2,4),1], iris3[-tr,c(2,4),2], iris3[-tr,c(2,4),3])
cl <- factor(c(rep("s",50), rep("c",50), rep("v",50)))
z <- lda(train, cl)
predict(z, test)$class
predict(z, mat)$class

test_point=test1
c2=

test1=test[1,]
test1[1]=3.5
test1[2]=1.25







pa<-ggplot(mapping = aes(x=1:150,y=El,color=classcol))+
  geom_point()+geom_hline(yintercept = EAER,color="magenta")+
  geom_text(aes(label=ind),vjust=2,size=3)+
  labs(x="Index",y="AER",title = "Errors using holdout method",
       color="Error value")+geom_hline(yintercept = Error,color="navyblue")+
  annotate("text", x = 15, y = Error, label = paste("AER=",signif(Error,4)),
           vjust=1.5,size=3.5,color="navyblue")+annotate("text", x = 15,
            y = EAER, label = paste("Expected AER=",signif(EAER,4)),
            vjust=1.5,size=3.5,color="magenta")


gridExtra::grid.arrange(
  pc,
  pa,
  nrow=1
)

#####

