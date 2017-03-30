library(RMySQL)
library(data.table)
library(tensorflow)
source("gameday_date_functions.R")

plimit.c <- 10000000
plimit.g <- 10000000
ump.mod.dir <- "models.umpire"

# Return a string with the SQL query for retrieving umpire training data.
# If an id is specified the query will be for that specific umpire, otherwise it will be for all umpires.

ump.training.data.query.tf <- function(id = -1,d.s = as.Date("2006-01-01"),d.e = as.Date("2006-01-02"),
				stand = "", incl.spring = FALSE, pitch.limit=plimit.c) {
        sqlString <- "  SELECT  p.gamedayPitchID,
                                concat(substr(a.gameName,5,4),'-',substr(a.gameName,10,2),'-',substr(a.gameName,13,2)) as date,
																u.id,
																a.batter,
																a.pitcher,
                                a.stand,
																a.p_throws,
                                a.b_height_in,
                                b.sz_top,
                                b.sz_bot,
                                (b.sz_top+b.sz_bot)/2 as sz_mid,
                                p.px,
                                p.pz,
                                p.des,
																p.start_speed,
																p.pfx_x,
																p.pfx_z
                        FROM    umpires u,
                                atbats a,
                                pitches p,
                                batter_sz_top_bot b,
                                games g
                        WHERE   u.gameName=p.gameName
                        AND     u.gameName=g.gameName
                        AND     p.gameName=a.gameName
                        AND     a.num=p.gameAtBatId
                        AND     a.batter=b.batter
                        AND     substr(a.gameName,5,4)=b.year
                        AND     u.position='home'
                        AND     p.des in ('Ball','Ball In Dirt','Called Strike')
                        AND     a.b_height_in is not null
                        AND     a.b_height_in!=0
                        AND     p.px is not null
                        AND     p.pz is not null"
        if(id != -1) { sqlString <- paste(sqlString,"AND u.id=",id) }
	if(stand %in% c('R','L')) { sqlString <- paste(sqlString," AND a.stand='",stand,"' ",sep="") }
	if(incl.spring) { sqlString <- paste(sqlString,"AND g.type IN ('R','S')") }
	else 		{ sqlString <- paste(sqlString,"AND g.type='R'") }
        sqlString <- paste(sqlString," AND STR_TO_DATE(concat(substr(a.gameName,5,4),'-',substr(a.gameName,10,2),'-',substr(a.gameName,13,2)), '%Y-%m-%d') < '",d.e,"'",sep="")
        sqlString <- paste(sqlString," AND STR_TO_DATE(concat(substr(a.gameName,5,4),'-',substr(a.gameName,10,2),'-',substr(a.gameName,13,2)), '%Y-%m-%d') >= '",d.s,"'",sep="")
        sqlString <- paste(sqlString,"ORDER BY concat(substr(a.gameName,5,4),'-',substr(a.gameName,10,2),'-',substr(a.gameName,13,2)) DESC, p.gamedayPitchID DESC LIMIT",as.integer(pitch.limit))
}

# Function to load data and train model for specified date range and umpire.
# Fields:
# 	id: GameDay umpire id (if unspecified it will use all umpires)
#	d.s: start of date range (i.e. get data on or after this date)
#	d.e: end of date range (i.e. get date before this date)
#	stand: batter hand (L or R) (if unspecified it will use data for both hands)
#	pitch.limit: max number of pitches to use for training; will use most recent pitches regardless. default is plimit.c
#	incl.spring: include spring training results in training data? default is "FALSE"
# Function returns "FALSE" if model fails to converge, returns nn model otherwise.
ump.train.model.tf <- function(id = -1,
															d.s = as.Date("2006-01-01"),
															d.e = as.Date("2006-01-02"),
			    										stand = "B",
															pitch.limit = plimit.c,
															incl.spring = FALSE,
															incl.pitchdata = FALSE) {
	sqlString <- ump.training.data.query.tf(id=id,
																			 d.s=d.s,
																			 d.e=d.e,
																			 stand=stand,
																			 pitch.limit=pitch.limit,
																			 incl.spring=incl.spring)

	mydb <- dbConnect(dbDriver("MySQL"),user="bbos",password="bbos",host="localhost",dbname="gameday")
	rs <- dbSendQuery(mydb,sqlString)
	dt <- fetch(rs,-1)
	dbDisconnect(mydb)

	dt$pz.ratio <- (dt$pz-dt$sz_mid)/((dt$sz_top-dt$sz_bot)/2)
	dt$s.f <- as.numeric(dt$des == "Called Strike")
	pcount <- nrow(dt)

	if(!incl.pitchdata) {
	} else {
	}

	return(m)
}

test_split <- function(df, cuts, prob, ...) {
  idx <- sample(seq(1, cuts), size = nrow(df), replace = TRUE, prob = prob, ...)
  z = list()
  for (i in 1:cuts)
    z[[i]] <- df[idx == i,]
  z
}

weight_variable <- function(shape, name=NULL) {
  initial <- tf$truncated_normal(shape, stddev = 0.1)
	if (is.null(name)) {
		tf$Variable(initial)
	} else {
		tf$Variable(initial, name=name)
	}
}

bias_variable <- function(shape, name=NULL) {
  initial <- tf$constant(0.1, shape = shape)
	if (is.null(name)) {
		tf$Variable(initial)
	} else {
		tf$Variable(initial, name=name)
	}
}

sqlString <- ump.training.data.query.tf(id=427058,
																		 d.s=as.Date("2016-03-01"),
																		 d.e=as.Date("2016-12-01"),
																		 stand="R")

mydb <- dbConnect(dbDriver("MySQL"),user="bbos",password="bbos",host="localhost",dbname="gameday")
rs <- dbSendQuery(mydb,sqlString)
dt <- fetch(rs,-1)
dbDisconnect(mydb)

dt$pz.ratio <- (dt$pz-dt$sz_mid)/((dt$sz_top-dt$sz_bot)/2)
dt$s.f <- as.numeric(dt$des == "Called Strike")

z <- test_split(dt, 3, c(0.6,0.2,0.2))

dt.train <- data.frame(z[1])
dt.test <- data.frame(z[2])
dt.validate <- data.frame(z[3])

m.data.train <- data.matrix(data.frame(dt.train$px, dt.train$pz.ratio))
colnames(m.data.train) <- NULL
m.label.train <- as.integer(data.frame(dt.train$s.f)[,1])

m.data.test <- data.matrix(data.frame(dt.test$px, dt.test$pz.ratio))
colnames(m.data.test) <- NULL
m.label.test <- as.integer(data.frame(dt.test$s.f)[,1])

m.data.validate <- data.matrix(data.frame(dt.validate$px, dt.validate$pz.ratio))
colnames(m.data.test) <- NULL
m.label.validate <- as.integer(data.frame(dt.validate$s.f)[,1])

sess <- tf$InteractiveSession()

# Model 1

x <- tf$placeholder(tf$float32, shape(NULL, 2L), name='x_0')

D_h <- 2L

W_0 <- weight_variable(shape(2L, 2L), name='W_0')
b_0 <- bias_variable(shape(1L, 2L), name='b_0')
y_0 <- tf$matmul(x, W_0) + b_0
a_0 <- tf$nn$sigmoid(y_0)

W_h1 <- weight_variable(shape(2L, D_h), name='W_h1')
b_h1 <- bias_variable(shape(1L, D_h), name='b_h1')
y_h1 <- tf$matmul(a_0, W_h1) + b_h1
a_h1 <- tf$nn$sigmoid(y_h1)

W_h2 <- weight_variable(shape(2L, D_h), name='W_h2')
b_h2 <- bias_variable(shape(1L, D_h), name='b_h2')
y_h2 <- tf$matmul(a_0, W_h2) + b_h2
a_h2 <- tf$nn$sigmoid(y_h2)

W_h3 <- weight_variable(shape(2L, D_h), name='W_h3')
b_h3 <- bias_variable(shape(1L, D_h), name='b_h3')
y_h3 <- tf$matmul(a_0, W_h3) + b_h3
a_h3 <- tf$nn$sigmoid(y_h3)

W_h4 <- weight_variable(shape(2L, D_h), name='W_h4')
b_h4 <- bias_variable(shape(1L, D_h), name='b_h4')
y_h4 <- tf$matmul(a_0, W_h4) + b_h4
a_h4 <- tf$nn$sigmoid(y_h4)

a_h_comb <- tf$concat(c(a_h1, a_h2, a_h3, a_h4), 1L)

W_out <- weight_variable(shape(4L * D_h, 2L), name='W_out')
b_out <- bias_variable(shape(1L, 2L), name='b_out')
y_out <- tf$matmul(a_h_comb, W_out) + b_out
a_out <- tf$nn$sigmoid(y_out)

y <- a_out
# y <- tf$nn$softmax(a_out)

y_ <- tf$placeholder(tf$float32, shape(NULL, 2L))

print("Define optimizer")
# loss <- tf$reduce_mean(-tf$reduce_sum(y_ * tf$log(y), reduction_indices=1L)) # cross-entropy
loss <- tf$reduce_mean(tf$losses$sigmoid_cross_entropy(logits=y, multi_class_labels=y_))
# loss <- tf$reduce_mean((y - y_) ^ 2, reduction_indices=1L)

optimizer <- tf$train$AdamOptimizer(1e-4)
# optimizer <- tf$train$GradientDescentOptimizer(0.5)
train_step <- optimizer$minimize(loss)

x_in <- m.data.train
y_in <- tf$one_hot(m.label.train, 2L)$eval()

correct_prediction <- tf$equal(tf$argmax(y, 1L), tf$argmax(y_, 1L))
accuracy <- tf$reduce_mean(tf$cast(correct_prediction, tf$float32))

a = tf$argmax(y, 1L)
b = tf$argmax(y_, 1L)
auc = tf$contrib$metrics$streaming_auc(a, b)

sess$run(tf$global_variables_initializer())

print("Train!!!")
for (i in 1:200) {
  sess$run(train_step,
           feed_dict = dict(x = x_in, y_ = y_in))
  if (i %% 20 == 0) {
		train_accuracy <- accuracy$eval(feed_dict = dict(x = x_in, y_ = y_in))
    cat(sprintf("step %d, training accuracy %g\n", i, train_accuracy))
	}
}

print("Evaluate!")
x_test <- m.data.test
y_test <- tf$one_hot(m.label.test, 2L)$eval()

result <- sess$run(accuracy, feed_dict = dict(x = x_test, y_ = y_test))
print(result)

sess$run(tf$local_variables_initializer())
result_auc <- sess$run(auc, feed_dict = dict(x = x_test, y_ = y_test))
print(result_auc)

# model 2

x2 <- tf$placeholder(tf$float32, shape(NULL, 2L), name='x2')

D_h <- 2L

W2 <- weight_variable(shape(2L, 10L), name='W2')
b2 <- bias_variable(shape(1L, 10L), name='b2')
y2_0 <- tf$matmul(x2, W2) + b2
a2 <- tf$nn$sigmoid(y2_0)

Wo <- weight_variable(shape(10L, 1L), name='Wo')
bo <- bias_variable(shape(1L, 1L), name='bo')
y2 <- tf$matmul(a2, Wo) + bo

y2_ <- tf$placeholder(tf$float32, shape(NULL, 1L))

loss2 <- tf$reduce_mean(tf$losses$sigmoid_cross_entropy(logits=y2, multi_class_labels=y2_))
# loss2 <- tf$reduce_mean((y - y_) ^ 2, reduction_indices=1L)

optimizer2 <- tf$train$AdamOptimizer(1e-4)
# optimizer2 <- tf$train$GradientDescentOptimizer(0.5)
train_step2 <- optimizer2$minimize(loss2)

x2_in <- m.data.train
y2_in <- as.matrix(m.label.train)

# correct_prediction2 <- tf$equal(tf$round(tf$cast(y2, tf$float32)), tf$round(tf$cast(y2_, tf$float32)))
correct_prediction2 <- abs(y2 - y2_) < 0.5
accuracy2 <- tf$reduce_mean(tf$cast(correct_prediction2, tf$float32))

a2 = y2
b2 = y2_
auc2 = tf$contrib$metrics$streaming_auc(a2, b2)

sess$run(tf$global_variables_initializer())

print("Train!!!")
for (i in 1:200) {
  sess$run(train_step2,
           feed_dict = dict(x2 = x2_in, y2_ = y2_in))
  if (i %% 20 == 0) {
		train_accuracy2 <- accuracy2$eval(feed_dict = dict(x2 = x2_in, y2_ = y2_in))
    cat(sprintf("step %d, training accuracy %g\n", i, train_accuracy2))
	}
}

print("Evaluate!")
x2_test <- m.data.test
y2_test <- as.matrix(m.label.test)

result2 <- sess$run(accuracy2, feed_dict = dict(x2 = x2_test, y2_ = y2_test))
print(result2)

sess$run(tf$local_variables_initializer())
result_auc2 <- sess$run(auc2, feed_dict = dict(x2 = x2_test, y2_ = y2_test))
print(result_auc2)
