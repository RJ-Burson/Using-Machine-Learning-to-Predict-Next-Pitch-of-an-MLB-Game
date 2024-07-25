
#####  Jaime Barria

train_set= subset(dfvnew_train, dfvnew_train$pitcher=="Jaime Barria")[,-c(1,3)]
validation_set= subset(dfvnew_valid, dfvnew_valid$pitcher=="Jaime Barria")[,-c(1,3)]
final_test= subset(dfvnew_test, dfvnew_test$pitcher=="Jaime Barria")[,-c(1,3)]


sl_ff= subset(train_set, pitch_name==6 | pitch_name==1)
sl_ch= subset(train_set, pitch_name==6 | pitch_name==2)
sl_si= subset(train_set, pitch_name==6 | pitch_name==5)



BalancedData1 = ovun.sample(pitch_name~. , sl_ff, method="over", p=0.75, subset=options("subset")$subset, 
                            na.action=options("na.action")$na.action, seed)$data

BalancedData2 = ovun.sample(pitch_name~. , sl_ch, method="over", p=0.75, subset=options("subset")$subset, 
                            na.action=options("na.action")$na.action, seed)$data

BalancedData3 = ovun.sample(pitch_name~. , sl_si, method="over", p=0.75, subset=options("subset")$subset,
                            na.action=options("na.action")$na.action, seed)$data


## Combining all of the pitches into one balanced dataset
baldata_train = do.call("rbind", list(BalancedData1, BalancedData2, BalancedData3))

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
