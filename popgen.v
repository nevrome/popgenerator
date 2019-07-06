import rand
import math
import os

fn main() {

	rand.seed()
	
	// #### paramters ####
	end_time := 100
	end_social := 100
	generation_length := 25.0

	// #### logic ####
	
	// calculate population development
	cumsum_of_popsize_array := cum_popsize_distribution(end_social)
	total_popsize := cumsum_of_popsize_array[end_time]
	total_number_of_entities := int(math.round(total_popsize / generation_length))

	// calculate social structure
	cumsum_of_social_array := cum_social_distribution(end_time)
	total_social := cumsum_of_social_array[end_social]
	
	mut entities_collector := []Entity
	for entity_counter := 1; entity_counter <= total_number_of_entities; entity_counter++ {
		// temporal distribution
		entity_time := where_in_cum_array(f64(random_integer(int(total_popsize))), cumsum_of_popsize_array)
		// social distribution
		entity_social := where_in_cum_array(f64(random_integer(int(total_social))), cumsum_of_social_array)
		// construct entities
		entities_collector << Entity{time: entity_time, social: entity_social}
	}

	// #### save result ####
	output_file := os.create('./test.csv') or {
		println(error)
    return
	}
	output_file.write('time' + ',' + 'social' + '\n')
	for entity in entities_collector {
		output_file.write(entity.print())
	}
	output_file.close()

}

// #### SocialWorld ####
struct SocialWorld {
	entities []Entity
}

// #### Entity ####
struct Entity {
	time int
	social int
}

fn (e Entity) print() string {
	return e.time.str() + ',' + e.social.str() + '\n'
} 

// #### population size along temporal space ####
fn popsize_distribution(time int) f64 {
  return 1.25 * time + 20
}

fn cum_popsize_distribution(end_time int) []f64 {
	mut time := 1
	mut cum_sum := []f64
	cum_sum << 0
	for time <= end_time {
		cum_sum << cum_sum[time - 1] + popsize_distribution(time)
		time++
	}
	return(cum_sum)
}

// #### social structure along the social space ####
fn social_distribution(social int) f64 {
  return 0.5 * math.cos(0.314 * social + 3.14) + 0.5
}

fn cum_social_distribution(end_social int) []f64 {
	mut social := 1
	mut cum_sum := []f64
	cum_sum << 0.0
	for social <= end_social {
		cum_sum << cum_sum[social - 1] + social_distribution(social)
		social++
	}
	return(cum_sum)
}

// #### helper functions ####
fn random_integer(max int) int {
	return rand.next(max)
}

fn where_in_cum_array(x f64, numbers []f64) int {
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



