# rather than repeating the 'off-label' plotting format in 10_1_modeL_fit_responses_mgcv.R
# that reports each m prediction one by one
# we follow Solomon Kurz' approach that pools the mean, within variance and between variance
# to do this we need to predict on separate set of new data to show temp change from 1-5 degrees
# can do this, just set all other variables to 0 or to median
# is there a way to convert ggpredict output to dataframe - no

# in that case create new dataframe with expand.grid and then use predict()
# https://stackoverflow.com/questions/66491927/how-can-i-use-predict-and-expand-grid-simultaneously-while-keeping-control-varia
# create new dataframe for every scenario i.e. precip change values, baseline temp change values
# and new predictions
# only pooling across m
