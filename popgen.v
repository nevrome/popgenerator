import rand

fn main() {
	
	//start_time := 0
	//end_time := 1000
	//entity_life_length := 50

	huhu := entity_generator(1, 2)
	
	huhu.print()
	
	println(cum_popsize_distribution(3, 10).str())

}

struct SocialWorld {
	entities []Entity
}

struct Entity {
	time int
	social int
}

fn (e Entity) print() {
	println(e.time.str() + ' ' + e.social.str()) 
} 

fn entity_generator(start_time int, end_time int) Entity {
	
	new_entity := Entity{time: 10, social: 15} 

	return new_entity

}

fn popsize_distribution(time int) int {
  return 2 * time + 20
}

fn cum_popsize_distribution(start_time int, end_time int) int {
	mut time := start_time
	mut cum_sum := 0
	for time <= end_time {
		cum_sum += popsize_distribution(time)
		time++
	}
	return(cum_sum)
}

fn random_integer(max int) int {
	rand.seed()
	return rand.next(max)
}




