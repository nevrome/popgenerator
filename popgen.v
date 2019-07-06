import rand

fn main() {

	rand.seed()
	
	// #### paramters ####
	end_time := 100
	entity_life_length := 50

	// #### logic ####
	cumsum_of_popsize_array := cum_popsize_distribution(end_time)

	total_popsize := cumsum_of_popsize_array[end_time]
	total_number_of_entities := total_popsize / entity_life_length

	mut entity_counter := 1
	mut entities_collector := []Entity
	for entity_counter <= total_number_of_entities {
		entity_time := where_in_cum_array(random_integer(total_popsize), cumsum_of_popsize_array)
		entities_collector << Entity{time: entity_time, social: 000}
		entity_counter++
	}

	for entity in entities_collector {
		entity.print()
	}

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

fn popsize_distribution(time int) int {
  return 2 * time + 20
}

fn cum_popsize_distribution(end_time int) []int {
	mut time := 1
	mut cum_sum := []int
	cum_sum << 0
	for time <= end_time {
		cum_sum << cum_sum[time - 1] + popsize_distribution(time)
		time++
	}
	return(cum_sum)
}

fn random_integer(max int) int {
	return rand.next(max)
}

fn where_in_cum_array(x int, numbers []int) int {
	mut i := 0
	for i < numbers.len {
		num_1 := numbers[i]
		num_2 := numbers[i + 1]
		if x >= num_1 && x <= num_2 {
			//println(num_1.str() + ' > ' + x.str() + ' < ' + num_2.str() + ' = ' + i.str())
			break
		}
		i++
	}
	return i
}



