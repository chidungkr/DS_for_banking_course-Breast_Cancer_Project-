
#======================================================================================
#  Project Name: Breast Cancer (breast-cancer-wisconsin.data.txt)
#  Data Source + research papers: 
#  - https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+%28Diagnostic%29
#  - http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.56.707&rep=rep1&type=pdf
#======================================================================================


#---------------------------------
#  Initial Data Analysis (IDA)
#---------------------------------

# Đọc dữ liệu: 
rm(list = ls())
bc_data <- read.table("D:/Teaching/data_science_banking/breast_cancer/breast-cancer-wisconsin.data.txt", 
                      sep = ",")

# Đổi tên cho cột biến: 

colnames(bc_data) <- c("sample_code_number", 
                       "clump_thickness", 
                       "uniformity_of_cell_size", 
                       "uniformity_of_cell_shape", 
                       "marginal_adhesion", 
                       "single_epithelial_cell_size", 
                       "bare_nuclei", 
                       "bland_chromatin", 
                       "normal_nucleoli", 
                       "mitosis", 
                       "classes")

library(tidyverse)
library(magrittr)

# Xem qua dữ liệu: 
bc_data %>% head()
bc_data %>% str()

# Viết hàm đánh giá tỉ lệ thiếu của dữ liệu: 

ti_le_na <- function(x) {100*sum(is.na(x) / length(x))}

# Cách 1: 
bc_data %>% summarise_all(ti_le_na)

# Cách 2: 
sapply(bc_data, ti_le_na)

#-----------------------
#  Pre-processing Data
#-----------------------

# Dán lại nhãn cho biến classes: 

dan_nhan <- function(x) {
  case_when(x == 2 ~ "B", 
            x == 4 ~ "M")
}


# Thay thế "?" thành nhãn có tần suất cao nhất: 
bc_data$bare_nuclei %>% table()

replace_na <- function(x) {
  ELSE <- TRUE
  case_when(x == "?" ~ "1", 
            ELSE ~ x)
}


# Sử dụng các hàm trên: 

bc_data %<>% mutate(classes = dan_nhan(classes), 
                    bare_nuclei = as.character(bare_nuclei), 
                    bare_nuclei = replace_na(bare_nuclei))

bc_data %>% head()
bc_data$bare_nuclei %>% table()

# Chuyển hóa bare_nuclei về đúng bản chất số: 
bc_data %<>% mutate(bare_nuclei = as.numeric(bare_nuclei))
bc_data %>% head()

# Loại bỏ biến không cần thiết: 
bc_data %<>% select(-sample_code_number)

#------------------------------------
#  Exploratory Data Analysis (EDA)
#------------------------------------

# Tỉ lệ của các nhãn: 

bc_data$classes %>% table() / nrow(bc_data)

# Hình ảnh hóa tỉ lệ này: 

theme_set(theme_minimal())
bc_data %>% 
  group_by(classes) %>% 
  count() %>% 
  ggplot(aes(classes, n)) + 
  geom_col()


# Cách 1: 
bc_data %>% 
  gather(features, value, -classes) %>% 
  ggplot(aes(classes, value)) + 
  geom_boxplot() + 
  facet_wrap(~ features, scales = "free")

# Cách 2: 

bc_data %>% 
  ggplot(aes(bare_nuclei, fill = classes, color = classes)) + 
  geom_density(alpha = 0.3)

# Cách 3: 

bc_data %>% 
  gather(features, value, -classes) %>% 
  ggplot(aes(value, fill = classes, color = classes)) + 
  geom_density(alpha = 0.3) + 
  facet_wrap(~ features, scales = "free")

# Tương quan giữa các biến là không cao: 
library(corrplot)
bc_data %>% 
  select(-classes) %>% 
  cor() %>% 
  corrplot(method = "color", order = "hclust")

#---------------------------------
#     Support Vector Machine
#---------------------------------

# Để sử dụng SVM cho phân loại cần chuyển hóa biến đích về factor: 
bc_data %<>% mutate_if(is.character, as.factor)


# Chuẩn bị dữ liệu: 

set.seed(1)
train <- bc_data %>% 
  group_by(classes) %>% 
  sample_frac(0.8) %>% 
  ungroup()

test <- setdiff(bc_data, train)

# Chạy SVM với cost =  10  và gamma = 0.1:
library(e1071)
set.seed(29)
svm_radial <- svm(classes ~., 
                  data = train,
                  method = "C-classification", 
                  kernel = "radial",
                  cost = 10, 
                  gamma = 0.1)

# Sơ bộ mô hình: 
summary(svm_radial)


#-----------------------------------
#  Evaluating Model Performance
#-----------------------------------

# Sử dụng mô hình cho dự báo: 
pred1 <- predict(svm_radial, test, decision.values = TRUE)

# Đánh giá mô hình (cách 1): 
mean(pred1 == test$classes)

# Đánh giá mô hình (cách 2): 
library(caret) 
confusionMatrix(pred1, test$classes, positive = "M")

# SVM với một cặp tham số khác: 
set.seed(123)
tune_svm <- svm(classes ~., 
                data = train,
                cost = 10, 
                gamma = 0.01)


# Dự báo trên mô hình tốt nhất này: 
best_pred <- predict(tune_svm, test, decision.values = TRUE)
confusionMatrix(best_pred, test$classes, positive = "M")


#-------------------------------------------
#    Comparing with some alternatives
#-------------------------------------------

# Đánh giá mô hình: 

acc_model1 <- c()
acc_model2 <- c()

for (i in 1:100) {
  set.seed(i)
  test100 <- bc_data %>% 
    group_by(classes) %>% 
    sample_frac(0.3) %>% 
    ungroup()
  
  pred1 <- predict(svm_radial, test100, decision.values = TRUE)
  acc1 <- mean(pred1 == test100$classes)
  acc_model1 <- c(acc_model1, acc1)
  
  pred2 <- predict(tune_svm, test100, decision.values = TRUE)
  acc2 <- mean(pred2 == test100$classes)
  acc_model2 <- c(acc_model2, acc2)
}

# Các kết quả ở dạng data frame: 
all_df <- data.frame(Accuracy = c(acc_model1, acc_model2), 
                     Model = c(rep("Model1", 100), rep("Model2", 100)))

# Đánh giá sơ bộ hiệu quả phân loại của hai mô hình bằng hình ảnh: 
all_df %>% 
  ggplot(aes(Model, Accuracy)) + 
  geom_boxplot()

all_df %>% 
  ggplot(aes(Accuracy)) + geom_density(alpha = 0.3, color = "blue", fill = "blue") + 
  geom_histogram(aes(y = ..density..), alpha = 0.3, color = "red", fill = "red") + 
  facet_wrap(~ Model)


t.test(all_df$Accuracy ~ all_df$Model)


#---------------------------------
#  Improving Model Performance
#---------------------------------

# Viết hàm chuẩn hóa dữ liệu: 

scale_01 <- function(x) {
  y <- (x - min(x)) / (max(x) - min(x))
  return(y)
}

# Chuẩn hóa dữ liệu: 
bc_data_scaled <- bc_data %>% mutate_if(is.numeric, scale_01)


set.seed(1995)
train <- bc_data_scaled %>% 
  group_by(classes) %>% 
  sample_frac(0.8) %>% 
  ungroup()

test <- setdiff(bc_data_scaled, train)

# Chạy SVM với cost =  10  và gamma = 0.1:

set.seed(29)
svm_radial_scaled <- svm(classes ~.,
                         data = train,
                         method = "C-classification", 
                         kernel = "radial",
                         cost = 10, 
                         gamma = 0.1)


pred1 <- predict(svm_radial_scaled, test, decision.values = TRUE)
mean(pred1 == test$classes)


acc_model3 <- c()

for (i in 1:100) {
  set.seed(i)
  test100 <- bc_data_scaled %>% 
    group_by(classes) %>% 
    sample_frac(0.3) %>% 
    ungroup()
  
  pred3 <- predict(svm_radial_scaled, test100, decision.values = TRUE)
  acc3 <- mean(pred3 == test100$classes)
  acc_model3 <- c(acc_model3, acc3)
  
}

all_df3 <- data.frame(Accuracy = c(acc_model1, acc_model2, acc_model3), 
                      Model = c(rep("Model1", 100), 
                                rep("Model2", 100), 
                                rep("Model3", 100)))
all_df3 %>% 
  ggplot(aes(Model, Accuracy)) +
  geom_boxplot()

all_df3 %>% 
  group_by(Model) %>% 
  summarise_each(funs(mean, median, min, max, sd), Accuracy) %>% 
  arrange(-Accuracy_mean)


