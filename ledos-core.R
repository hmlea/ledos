library(plotly)
library(wavefunction)

# monte carlo simulator, checks and takes points less than psi_lim
# from Henrique Fernandes Lobo et al.
# psi_lim default value is taken from Tulley et al.
monte_carlo = function(n, l, m, psi_lim=0.0026, r_lim=35, num_points=2048) {
  # init data vectors
  r_points = c()
  theta_points = c()
  phi_points = c()
  psi = c()
  
  # loop untill a sufficient number of points is collectd
  counter = 0
  # withProgress(message="Working...", value=0, {
  while (counter < num_points) {
    # generate a random radius, theta, and phi (spherical coords)
    r = runif(1, 0, r_lim)
    theta = runif(1, 0, pi)
    phi = runif(1, 0, 2*pi)
    
    # get the psi and |psi|^2, and a w
    psi_val = psi(n, l, m, r, theta, phi)
    psi2 = abs(psi_val)**2
    w = runif(1, 0, psi_lim)
    
    # if w is less than |psi|^2...
    if (w <= psi2) {
      # keep the point coordinates and psi value
      r_points = append(r_points, r)
      theta_points = append(theta_points, theta)
      phi_points = append(phi_points, phi)
      psi = append(psi, psi_val)
      
      # increment the counter and shiny progress bar
      counter = counter + 1
      setProgress(1, detail=paste(counter, "/", num_points))
    }
  }
  
  # output the data points
  output = data.frame("r"=r_points, "theta"=theta_points,
                      "phi"=phi_points, "psi"=psi)
}

# converts spherical coordinates to cartesian
convert_sph_coords = function(data) {
  # get spherical coordinates
  r = data[[1]]
  theta = data[[2]]
  phi = data[[3]]
  
  # convert to cartesian
  x = (r * sin(theta) * cos(phi))
  y = (r * sin(theta) * sin(phi))
  z = (r * cos(theta))
  
  # output the cartesian coordinates
  output = data.frame("x"=x, "y"=y, "z"=z, psi=data[[4]])
}

# get the name of the orbital in an HTML format
# add error handling and extended quantum numbers
get_html_name = function(n, l, m) {
  # get the angular letter of the orbital
  angular_list = c("s", "p", "d", "f")
  orb = paste0(n, angular_list[[l+1]])
  
  # get the axis of the orbital with number m
  if(l == 0) {
    output = orb
  } else {
    offset = m + l + 1
    axis_list = switch(l,
                       c("x", "z", "y"),
                       c("xy", "xz", "z<sup>2</sup>", "yz",
                         "x<sup>2</sup>-y<sup>2</sup>"),
                       c("x(x<sup>2</sup>-3y<sup>2</sup>)", "xyz",
                         "xz<sup>2</sup>", "z<sup>3</sup>", "yz<sup>2</sup>",
                         "z(x<sup>2</sup>-y<sup>2</sup>)",
                         "y(3x<sup>2</sup>-y<sup>2</sup>)"))
    axis = axis_list[[offset]]
    output = paste0(orb, "<sub>", axis, "</sub>")
  }
}

# does the same as the above function but just with a color gradient for psi
plot_data = function(coords, pos_col="#d13010", neg_col="#2d709f") {
  lim = max(max(coords$psi), abs(min(coords$psi)))
  # OR fig = plot_ly(fig,....)
  fig = plot_ly(type="scatter3d", mode="markers")
  fig = add_trace(fig, data=coords, x=~x, y=~y, z=~z, type="scatter3d",
                  mode="markers", size=1,
                  marker=list(size=1,
                              color=~psi,
                              colorscale=list(c(0, neg_col),
                                              c(0.5, "#ffffff"),
                                              c(1, pos_col)),
                              cauto=FALSE,
                              cmin=-lim,
                              cmaz=lim,
                              showscale=TRUE))
  
  # to add the axis lines in the middle
  # delete legend "trace 1" as well
  # fig = add_trace(fig, x=c(0, 0), y=c(0, 0), z=c(min(data$z), max(data$z)),
  #                 mode="lines", line=list(color="orange"))
  # fig = add_trace(fig, x=c(min(data$x), max(data$x)), y=c(0, 0), z=c(0, 0),
  #                 mode="lines", line=list(color="orange"))
  # fig = add_trace(fig, x=c(0, 0), y=c(min(data$y), max(data$y)), z=c(0, 0),
  #                 mode="lines", line=list(color="orange"))
  
  fig = layout(fig, paper_bgcolor="gray", showlegend=FALSE)
}

# simulates the data for the orbital - ready to plot
# psi_lim from Tully et al. - the maximum psi cutoff for the monte carlo sim
sim_data = function(n, l, m, psi_lim=0.0026, r_lim=35, num_points=2048) {
  # progress bar - might be able to be done in the server file
  withProgress(message="Simulating", value=1, {
    # simulate the data
    sph_coords = monte_carlo(n, l, m, psi_lim, r_lim, num_points)
    setProgress(value=1, message="Converting", detail="")
    # convert to cartesian coordinates
    cart_coords = convert_sph_coords(sph_coords)
    setProgress(value=1, message="Plotting", detail="")
  })
  # output - goes to plot right after
  output = cart_coords
}

