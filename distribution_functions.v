// #### population size along temporal space ####
fn temporal_distribution(i int) int {
	x := f64(i)
	res := 10.0 * math.sin(0.01 * (x - 0.1)) + 20.0
  return int(math.round(res))
}

// #### x structure along the social space ####
fn x_distribution(i int) int {
	x := f64(i)
	res := 10.0 * math.sin(0.01 * (x - 0.1)) + 10.0
  return int(math.round(res))
}

// #### y structure along the social space ####
fn y_distribution(i int) int {
	x := f64(i)
	res := 10.0 * math.sin(0.01 * (x - 0.1)) + 10.0
  return int(math.round(res))
}
