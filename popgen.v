import rand
import math
import os

fn main() {

	rand.seed(123)

	// #### paramters ####
	end_time := 100
	end_social := 100
	generation_length := 25.0

	// #### logic ####

	// calculate population development
	total_popsize := integral_popsize_distribution(end_time)
	total_number_of_entities := int(math.round(total_popsize / generation_length))

	// calculate social structure
	total_social := integral_social_distribution(end_social)

	mut entities_collector := []Entity
	for entity_counter := 1; entity_counter <= total_number_of_entities; entity_counter++ {
		// temporal distribution
		entity_time := inverse_integral_popsize_distribution(f64(random_integer(int(total_popsize))))
		// social distribution
		entity_social := inverse_integral_social_distribution(f64(random_integer(int(total_social))))
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
	time f64
	social f64
}

fn (e Entity) print() string {
	return e.time.str() + ',' + e.social.str() + '\n'
}

// #### population size along temporal space ####
fn popsize_distribution(x int) f64 {
  return 2 * x
}

fn integral_popsize_distribution(x int) f64 {
	return 2 * x * x
}

fn inverse_integral_popsize_distribution(y f64) f64 {
	return math.sqrt(y / 2)
}

// #### social structure along the social space ####
fn social_distribution(x int) f64 {
  return 2 * x
}

fn integral_social_distribution(x int) f64 {
	return 2 * x * x
}

fn inverse_integral_social_distribution(y f64) f64 {
	return math.sqrt(y / 2)
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
