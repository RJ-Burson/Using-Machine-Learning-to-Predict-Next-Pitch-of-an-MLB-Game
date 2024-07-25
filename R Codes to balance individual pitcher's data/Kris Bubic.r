
#####  Kris Bubic

train_set= subset(dfvnew_train, dfvnew_train$pitcher=="Kris Bubic")[,-c(1,3)]
validation_set= subset(dfvnew_valid, dfvnew_valid$pitcher=="Kris Bubic")[,-c(1,3)]
final_test= subset(dfvnew_test, dfvnew_test$pitcher=="Kris Bubic")[,-c(1,3)]


ff_ch= subset(train_set, pitch_name==1 | pitch_name==2)
ff_cu= subset(train_set, pitch_name==1 | pitch_name==3)



BalancedData1 = ovun.sample(pitch_name~. , ff_ch, method="over", p=0.666, subset=options("subset")$subset, 
                            na.action=options("na.action")$na.action, seed)$data

BalancedData2 = ovun.sample(pitch_name~. , ff_cu, method="over", p=0.666, subset=options("subset")$subset, 
                            na.action=options("na.action")$na.action, seed)$data


#All of the pitches
baldata_train = do.call("rbind", list(BalancedData1, BalancedData2))

train_set=baldata_train

train_set = na.omit(train_set)
validation_set = na.omit(validation_set)
final_test = na.omit(final_test)


train_set$pitch_name <- factor(train_set$pitch_name, 
                               levels=c(1,2,3,4,5,6,7,8) )
validation_set$pitch_name <- factor(validation_set$pitch_name, 
                                    levels=c(1,2,3,4,5,6,7,8) )
final_test$pitch_name <- factor(final_test$pitch_name, 
                                levels=c(1,2,3,4,5,6,7,8) )


train_set = droplevels.data.frame(train_set)
validation_set = droplevels.data.frame(validation_set)
final_test = droplevels.data.frame(final_test)
