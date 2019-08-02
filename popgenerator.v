import rand
import math
import os

fn main() {

	rand.seed(1463233)

	// #### read input args ####
	config_file_path := os.args[1]
	entities_output_file_path := os.args[2]
	relations_output_file_path := os.args[3]

	// #### default parameters ####
	mut end_time := 200
	mut	end_social := 200
	mut number_of_entities := 200
	mut neighbours_distance := 10

  // #### read config file ####
	config_file_content := os.read_file(config_file_path.trim_space()) or {
		println('failed to open $config_file_path')
		return
	}

	config_file_lines := config_file_content.split(';')
	config_number_lines := config_file_lines.len - 1

	for i := 0; i < config_number_lines; i++ {
		mut config_file_line := config_file_lines[i]
		mut config_values_array := config_file_line.split('=')
		mut name := config_values_array[0].trim_space()
		mut value := config_values_array[1].trim_space()

		println('$name = $value')

		if name == 'end_time' {
			end_time = value.int()
		}
		if name == 'end_social' {
			end_social = value.int()
		}
		if name == 'number_of_entities' {
			number_of_entities = value.int()
		}
		if name == 'neighbours_distance' {
			neighbours_distance = value.int()
		}
	}

	// #### create sequences####
	mut time_arr := [0; end_time]
	for t := 0; t < end_time; t++ {
		time_arr[t] = t
	}
	mut social_arr := [0; end_social]
	for s := 0; s < end_social; s++ {
		social_arr[s] = s
	}

	// #### calculate frequencies ####
	mut time_freq := [0; end_time]
	for t := 0; t < end_time; t++ {
		time_freq[t] = temporal_distribution(t)
	}
	mut social_freq := [0; end_social]
	for s := 0; s < end_social; s++ {
		social_freq[s] = social_distribution(s)
	}

	// #### create entities ####
	mut entities := []Entity
	for entity_counter := 0; entity_counter < number_of_entities; entity_counter++ {
		// draw temporal coordinate
		entity_time := random_integer_distribution(time_arr, time_freq, end_time)
		// draw social coordinate
		entity_social := random_integer_distribution(social_arr, social_freq, end_social)
		// construct entities
		entities << Entity{
			id: entity_counter,
			time: entity_time,
			social: entity_social
		}
	}

	// #### determine relations ####
	mut relations := []Relation
	for entity_a in entities {
		for entity_b in entities {
			if (entity_b.time <= entity_a.time + neighbours_distance) &&
				 (entity_b.time >= entity_a.time - neighbours_distance) &&
				 (entity_b.social <= entity_a.social + neighbours_distance) &&
				 (entity_b.social >= entity_a.social - neighbours_distance) {
				relations << Relation{
					id_a: entity_a.id,
					id_b: entity_b.id
				}
			}
		}
	}

	// #### save result ####
	entities_output_file := os.create(entities_output_file_path) or {
		println(error)
    return
	}
	entities_output_file.write('id' + ',' + 'time' + ',' + 'social' + '\n')
	for entity in entities {
		entities_output_file.write(entity.print())
	}
	entities_output_file.close()

	relations_output_file := os.create(relations_output_file_path) or {
		println(error)
		return
	}
	relations_output_file.write('id_a' + ',' + 'id_b' + '\n')
	for relation in relations {
		relations_output_file.write(relation.print())
	}
	relations_output_file.close()

}

// #### Entity ####
struct Entity {
	id int
	time int
	social int
}

fn (e Entity) print() string {
	return e.id.str() + ',' + e.time.str() + ',' + e.social.str() + '\n'
}

// #### Relation ####
struct Relation {
	id_a int
	id_b int
}

fn (r Relation) print() string {
	return r.id_a.str() + ',' + r.id_b.str() + '\n'
}

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

// #### random number generation ####
fn random_integer(max int) int {
	return rand.next(max)
}

// https://www.geeksforgeeks.org/random-number-generator-in-arbitrary-probability-distribution-fashion/
// returns a random number from arr[] according to
// distribution array defined by freq[]. n is size of arrays.
fn random_integer_distribution(arr[] int, freq[] int, n int) int {
		// Create and fill prefix array
    mut prefix := [0; n]
    prefix[0] = freq[0]
		for i := 1; i < n; i++ {
      prefix[i] = prefix[i - 1] + freq[i]
		}
    // prefix[n-1] is sum of all frequencies. Generate a random number
    // with value from 1 to this sum
    r := (random_integer(prefix[n - 1])) + 1
    // Find index of ceiling of r in prefix arrat
  	indexc := find_ceil(prefix, r, 0, n - 1)
    return arr[indexc]
}

// Utility function to find ceiling of r in arr[l..h]
fn find_ceil(arr[] int, r int, l int, h int) int {
		mut mid := 0
		for {
			mid = (l + h) / 2
			if r > arr[mid] { l = mid + 1 }  else { h = mid }
			if !(l < h) { break }
		}
		mut res := 0
		if arr[l] >= r { res = l } else { res = -1 }
    return res
}
