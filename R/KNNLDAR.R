#' @title KNN method to predict test set using R
#' @description KNN
#' @param train_x the properties of the train set
#' @param train_y the labels of the train set
#' @param test_x the test set to be predicted
#' @param k the number of data used to predict test set
#' @return a label for test set
#' @examples
#' \dontrun{
#' data(data)
#' attach(data)
#' ypred <- myKNN(X, y,test_x = X, k = 5)
#' }
#' @export
myKNN <- function(train_x, train_y,test_x, k = 5){
  test_size = dim(test_x)[1]
  size = dim(train_x)[1]
  
  # 1.计算两个矩阵每个的距离
  l2_distance = -2 * test_x %*% t(train_x) +
    matrix(rep(rowSums(train_x^2), test_size), nrow = test_size, byrow = TRUE)+
    matrix(rep(rowSums(test_x^2), size), nrow = test_size)
  
  # 2.定义函数排序
  top_k <- function(test1,top_k = k){
    order_dist = order(test1, decreasing = F)[1:top_k]
    return(names(sort(table(train_y[order_dist]),decreasing = T)[1]))
  }
  
  # 3.预测结果
  yy_pred = as.vector(apply(l2_distance, MARGIN = 1, FUN = top_k))
  return(yy_pred)
}

#' @title LDA method to predict test set using R
#' @description LDA
#' @param train_X the properties of the train set
#' @param train_y the lables of the train set
#' @param test_x the test set to be predicted
#' @return a label for test set
#' @examples
#' \dontrun{
#' data(data)
#' attach(data)
#' ypred <- myLDA(X, y,test_x = X)
#' }
#' @export
myLDA <- function(train_X, train_y, test_x){
  y = train_y
  X1 = train_X[train_y==1, ]
  X2 = train_X[train_y==0, ]
  
  pi1 <- mean(y)
  n1 <- sum(y)
  n2 <- sum(y == 0)
  pi2 <- 1 - mean(y)
  
  mu1 <- apply(X1, 2, mean)
  
  mu2 <- apply(X2, 2, mean)
  mu_1and2 <- (mu1 + mu2) / 2
  
  x_mu1 <- sweep(X1 ,2, mu1)
  x_mu2 <- sweep(X2 ,2, mu2)
  
  sigma1 <- t(x_mu1) %*% x_mu1 / n1
  sigma2 <- t(x_mu2) %*% x_mu2 / n2
  sigma_merge <- (n1-1) * sigma1/(n1 + n2-2) + (n2-1) * sigma2/(n1 + n2 - 2)
  
  if(length(test_x) > 1){
    test_mu1andm2 <- sweep(test_x ,2, mu_1and2)
  }else{
    test_mu1andm2 <-test_x - mu_1and2
  }
  y_hat <- test_mu1andm2 %*% solve(sigma_merge) %*% (mu1 - mu2) + log(n1/n2)
  y_pred <- ifelse(y_hat > 0, 1, 0)
  return(list('yhat' = y_hat, 'ypred' = y_pred))
}