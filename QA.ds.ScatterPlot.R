####################################################################
#     SCRIPT FOR CHECKING THE DATASHIELD SCATTER PLOT FUNCTION     #
####################################################################

# START UP VMs
# HOW MANY VMs TO USE 1, 2 or 3
use.VMs <- 3

# SET DEFAULT WORKING DIRECTORY
setwd("C:/Users/da15564/Dropbox/Demetris_BRISTOL/DataSHIELD Development/DataSHIELD_Programming")

# SOURCE CURRENT SET OF DEFAULT CLIENT FUNCTIONS THAT HAVE NOT BEEN ADDED TO dsBase.client
source("C:/Users/da15564/Dropbox/Demetris_BRISTOL/DataSHIELD Development/DataSHIELD_Programming/STARTUP.SCRIPTS/CURRENT.EXTRA.CLIENT.SOURCE.FILES.R")

# PACKAGES: devtools
library(devtools)

# PACKAGES: opal, opaladmin
library(opal)
library(opaladmin)

#When the current release is different to the most updated packages the
#following still work 
devtools::load_all("C:/Users/da15564/Dropbox/Demetris_BRISTOL/DataSHIELD Development/Github/CLONED.REPOS/dsBaseClient")
devtools::load_all("C:/Users/da15564/Dropbox/Demetris_BRISTOL/DataSHIELD Development/Github/CLONED.REPOS/dsStatsClient")
devtools::load_all("C:/Users/da15564/Dropbox/Demetris_BRISTOL/DataSHIELD Development/Github/CLONED.REPOS/dsGraphicsClient")
devtools::load_all("C:/Users/da15564/Dropbox/Demetris_BRISTOL/DataSHIELD Development/Github/CLONED.REPOS/dsModellingClient")
devtools::load_all("C:/Users/da15564/Dropbox/Demetris_BRISTOL/DataSHIELD Development/Github/CLONED.REPOS/dsBetaTestClient")

###########################################################
############# LOGIN PROCEDURE CNSIM.CNSIM DATA  ###########
###########################################################
if(use.VMs==1)
{
logindata.VMs.cm <- ds.createLogindata(111,table="CNSIM.CNSIM1")
opals.cm <- datashield.login(logins=logindata.VMs.cm,assign=TRUE,symbol="D")
}else
if(use.VMs==2)
{
logindata.VMs.cm <- ds.createLogindata(111,111,table=c("CNSIM.CNSIM1","CNSIM.CNSIM2"))
logindata.VMs.cm <- logindata.VMs.cm[1:use.VMs,]
opals.cm <- datashield.login(logins=logindata.VMs.cm,assign=TRUE,symbol="D")
}else
if(use.VMs==3)
{
logindata.VMs.cm <- ds.createLogindata(111,111,111,table=c("CNSIM.CNSIM1","CNSIM.CNSIM2","CNSIM.CNSIM3"))
logindata.VMs.cm <- logindata.VMs.cm[1:use.VMs,]
opals.cm <- datashield.login(logins=logindata.VMs.cm,assign=TRUE,symbol="D")
}
#######################


#ds.dim('D')
ds.ListClientsideFunctions()
ds.ListServersideFunctions()
ds.colnames("D")

##################################################################################

# CHECK THE SERVER-SIDE FUNCTION

# Example 1:
datashield.aggregate(opals.cm, paste0("centroids2dDS.b(", 'D$PM_BMI_CONTINUOUS', ",", 'D$LAB_GLUC_ADJUSTED', ",", '3',")"))

# Example 2: Fails because n=2 is less than the threshold
datashield.aggregate(opals.cm, paste0("centroids2dDS.b(", 'D$PM_BMI_CONTINUOUS', ",", 'D$LAB_GLUC_ADJUSTED', ",", '2',")"))

# CHECK THE CLIENT-SIDE FUNCTION

# Example 1: generate a combined scatter plot (the default behaviour)
ds.ScatterPlot.b(x='D$PM_BMI_CONTINUOUS', y='D$LAB_GLUC_ADJUSTED', n=5)

# Example 2: generate a scatter plot for each study separately
ds.ScatterPlot.b(x='D$PM_BMI_CONTINUOUS', y='D$LAB_GLUC_ADJUSTED', type="split")

# Example 3: if a variable is of type factor the scatter plot is not created
ds.ScatterPlot.b(x='D$PM_BMI_CATEGORICAL', y='D$LAB_GLUC_ADJUSTED')


