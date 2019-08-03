// #### population size along temporal space ####
fn temporal_distribution(t int) int {
	x := f64(t)
	res := 20.0 + 0.5 * x
  return int(math.round(res))
}

// #### social structure along the social space ####
fn social_distribution(s int) int {
	x := f64(s)
	res := 5.0 * math.sin(0.01 * (x - 0.1)) + 5.0
  return int(math.round(res))
}
