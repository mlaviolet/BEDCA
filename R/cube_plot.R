library(ggplot2)

df <- data.frame(
  A = c(-1, 1, -1, 1, -1, 1, -1, 1),
  B = c(-1, -1, 1, 1, -1, -1, 1, 1),
  C = c(-1, -1, -1, -1, 1, 1, 1, 1),
  Response = c(10, 15, 12, 20, 18, 25, 22, 30)
)

# Define 2D coordinates for the 3D cube visualization
# This requires mapping the 8 corners (x, y)
cube_coords <- data.frame(
  df,
  x = c(1, 2, 1, 2, 1.5, 2.5, 1.5, 2.5),
  y = c(1, 1, 2, 2, 1.5, 1.5, 2.5, 2.5)
)

# Plotting
ggplot(cube_coords, aes(x=x, y=y)) +
  # Draw lines connecting vertices (manual path ordering required)
  geom_path(data=cube_coords[c(1,2,4,3,1,5,6,8,7,5,6,8,4,2,6,5,7,8,4,3,7),]) +
  geom_point(size=5) +
  geom_text(aes(label=Response), vjust=-1) +
  theme_void() +
  labs(title="Factorial Cube Plot")
