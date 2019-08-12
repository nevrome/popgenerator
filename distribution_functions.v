// #### population size along temporal space ####
fn temporal_distribution(i int) int {
	x := f64(i)
	res := 20.0 + 0.5 * x
  return int(math.round(res))
}

// #### x structure along the social space ####
fn x_distribution(i int) int {
	x := f64(i)
	res := 5.0 * math.sin(0.01 * (x - 0.1)) + 5.0
  return int(math.round(res))
}

// #### y structure along the social space ####
fn y_distribution(i int) int {
	x := f64(i)
	res := 5.0 * math.sin(0.01 * (x - 0.1)) + 5.0
  return int(math.round(res))
}
