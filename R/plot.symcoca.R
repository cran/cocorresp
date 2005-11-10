"plot.symcoca" <-
function(x, pages = NULL, axes = c(1:2), scaling = 1,
                         cex = c(0.8, 0.8), pch = c(par("pch"), 3),
                         ylab = NULL, xlab = NULL,
                         ann = par("ann"), lab.plot = TRUE, ...)
  {
    oldpar <- par(mfrow = c(1, 2))
    on.exit(par(oldpar))
    plt.dat <- scores(x, choices = axes, scaling,
                      display = c("species", "site"))
    ranx1 <- range(plt.dat$species$U1[,1], plt.dat$site$X1[,1])
    rany1 <- range(plt.dat$species$U1[,2], plt.dat$site$X1[,2])
    ranx2 <- range(plt.dat$species$U2[,1], plt.dat$site$X2[,1])
    rany2 <- range(plt.dat$species$U2[,2], plt.dat$site$X2[,2])
    ylabs <- substitute("Axis " * arg1 * " " * (lambda[arg1] == arg2),
                        list(arg1 = axes[2],
                             arg2 = round(x$lambda[axes[2]], 4)))
    xlabs <- substitute("Axis " * arg1 * " " * (lambda[arg1] == arg2),
                        list(arg1 = axes[1],
                             arg2 = round(x$lambda[axes[1]], 4)))
    plot(plt.dat$site$X1, xlim = ranx1, ylim = rany1, asp = 1,
         cex = cex[1], pch = pch[1], ylab = ylabs, xlab = xlabs)
    if(lab.plot)
      title(main = x$nam.dat$namY)
    points(plt.dat$species$U1, col = "red", pch = pch[2], cex = cex[2])
    plot(plt.dat$site$X2, xlim = ranx2, ylim = rany2, asp = 1,
         cex = cex[1], pch = pch[1], ylab = ylabs, xlab = xlabs)
    if(lab.plot)
      title(main = x$nam.dat$namX)
    points(plt.dat$species$U2, col = "red", pch = pch[2], cex = cex[2])
    invisible()
  }

