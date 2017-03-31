library(RMySQL)
library(data.table)
library(tensorflow)
library(ggplot2)

source("gameday_date_functions.R")
source("umpire_graph_functions.R")

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

m.data.train <- data.matrix(data.frame(dt.train$px, dt.train$pz))
colnames(m.data.train) <- NULL
m.label.train <- as.integer(data.frame(dt.train$s.f)[,1])

m.data.test <- data.matrix(data.frame(dt.test$px, dt.test$pz))
colnames(m.data.test) <- NULL
m.label.test <- as.integer(data.frame(dt.test$s.f)[,1])

m.data.validate <- data.matrix(data.frame(dt.validate$px, dt.validate$pz))
colnames(m.data.test) <- NULL
m.label.validate <- as.integer(data.frame(dt.validate$s.f)[,1])

sess <- tf$InteractiveSession()

D_h <- 128L

x <- tf$placeholder(tf$float32, shape(NULL, 2L), name='x')

W_i <- weight_variable(shape(2L, D_h), name='W_i')
b_i <- bias_variable(shape(1L, D_h), name='b_i')
A <- tf$tanh(tf$matmul(x, W_i) + b_i)

W_o <- weight_variable(shape(D_h, 1L), name='W_o')
b_o <- bias_variable(shape(1L, 1L), name='b_o')
y <- tf$sigmoid(tf$matmul(A, W_o) + b_o)

y_ <- tf$placeholder(tf$float32, shape(NULL, 1L))

loss <- tf$nn$l2_loss(y - y_)
optimizer <- tf$train$AdamOptimizer(1e-2)
train_step <- optimizer$minimize(loss)

x_in <- m.data.train
y_in <- as.matrix(m.label.train)

correct_prediction <- abs(y - y_) < 0.5
accuracy <- tf$reduce_mean(tf$cast(correct_prediction, tf$float32))

a = y
b = y_
auc = tf$contrib$metrics$streaming_auc(a, b)

sess$run(tf$global_variables_initializer())

print("Train!!!")
for (i in 1:1000) {
  sess$run(train_step,
           feed_dict = dict(x = x_in, y_ = y_in))
  if (i %% 20 == 0) {
		train_accuracy <- accuracy$eval(feed_dict = dict(x = x_in, y_ = y_in))
    cat(sprintf("step %d, training accuracy %g\n", i, train_accuracy))
	}
}

print("Evaluate!")
x_test <- m.data.test
y_test <- as.matrix(m.label.test)

result <- sess$run(accuracy, feed_dict = dict(x = x_test, y_ = y_test))
print(result)

sess$run(tf$local_variables_initializer())
result_auc <- sess$run(auc, feed_dict = dict(x = x_test, y_ = y_test))
print(result_auc)

# Graph the model
x.plot <- seq(-2,2,by=0.025)
z.plot <- seq(1,5,by=0.025)

grid <- expand.grid(x=x.plot,y=z.plot)
m.plot <- as.matrix(cbind(grid[,1], grid[,2]))

plot_vals <- y$eval(feed_dict = dict(x = m.plot))

plot_tf <- function(x, y, p, filename="tf.png") {
	coords <- data.frame(x=x, y=y, p=p)
	g <- ggplot(coords,aes(x,y))
	g <- g + geom_tile(aes(fill=p)) + xlab("X") + ylab("Z")
	g <- g + scale_fill_gradient(low="white",high="black")
	g <- g + THT_Theme
	ggsave(g,file=paste("./umpire_graphs/",filename,sep=""),height=g.height,width=g.width)
}
