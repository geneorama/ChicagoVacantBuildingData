
# This script is supposed to create an Rds file called op.Rds
# op stands for "old par", and "par" is a list of your graphics
# parameters in R.
# 
# The idea is that you can use "op" to reset your graphics to 
# their default settings, since there is no "reset" feature in 
# R.
# 
# See more here: ?pa,r ?dev.copy

##------------------------------------------------------------------------------
## INITIALIZATION
##------------------------------------------------------------------------------

## Try to close a graphics device.
## If a graphics device, then we don't need one and we can save the 
## default parameters

CloseGrdevAttempt <- try(dev.off(), silent=TRUE)

##------------------------------------------------------------------------------
## CREATE op
##------------------------------------------------------------------------------
## "op" is a list of par() defaults.  Create by running: 
# op <- par(no.readonly = TRUE)

if("try-error" %in% class(CloseGrdevAttempt)){
    # dev.new()
    # dev.off()
    op <- par(no.readonly = TRUE)
}else{
    op <- par(no.readonly = TRUE)
}

##------------------------------------------------------------------------------
## SAVE op
##------------------------------------------------------------------------------
saveRDS(op, "data/op.Rds")



