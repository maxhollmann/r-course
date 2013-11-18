data(infert, package="datasets")
print(net.infert <- neuralnet(case~parity+induced+spontaneous, infert, hidden = 3,
                              lifesign = "full",
                              err.fct="ce", linear.output=FALSE, likelihood=TRUE))
# gwplot(net.infert, selected.covariate="parity")
# gwplot(net.infert, selected.covariate="induced")
# gwplot(net.infert, selected.covariate="spontaneous")
plot(net.infert)