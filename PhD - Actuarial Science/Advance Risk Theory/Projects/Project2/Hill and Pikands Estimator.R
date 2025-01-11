library(fExtremes)
library(RobExtremes)
library(evmix)
# Fit the GEV model
gev_model <- gevFit(danishuni$Loss)
hillPlot(danishuni$Loss , plottype = "xi" , ci = 0.95)
grid()
fExtremes::mePlot(danishuni$Loss)

t = PickandsEstimator(danishuni$Loss, ParamFamily= GEVFamily())
pickandsplot(danishuni$Loss) 
evmix::hillplot(danishuni$Loss)


