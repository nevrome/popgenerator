import rand
import math
import os

fn main() {

	rand.seed(1463233)

	// #### read input args ####
	input_config_file_path := os.args[1]
	output_pajek_file_path := os.args[2]

	// #### create parameter variables ####
	mut end_time := 0
	mut	end_x := 0
	mut	end_y := 0
	mut number_of_entities := 0
	mut neighbours_distance := 0

  // #### read config file ####
	input_config_file_content := os.read_file(input_config_file_path.trim_space()) or {
		println('failed to open $input_config_file_path')
		return
	}

	input_config_file_lines := input_config_file_content.split(';')
	config_number_lines := input_config_file_lines.len - 1

	for i := 0; i < config_number_lines; i++ {
		mut input_config_file_line := input_config_file_lines[i]
		mut config_values_array := input_config_file_line.split('=')
		mut name := config_values_array[0].trim_space()
		mut value := config_values_array[1].trim_space()

		println('$name = $value')

		if name == 'end_time' {
			end_time = value.int()
		}
		if name == 'end_x' {
			end_x = value.int()
		}
		if name == 'end_y' {
			end_y = value.int()
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
	for i := 0; i < end_time; i++ {
		time_arr[i] = i
	}
	mut x_arr := [0; end_x]
	for i := 0; i < end_x; i++ {
		x_arr[i] = i
	}
	mut y_arr := [0; end_y]
	for i := 0; i < end_y; i++ {
		y_arr[i] = i
	}

	// #### calculate frequencies ####
	mut time_freq := [0; end_time]
	for i := 0; i < end_time; i++ {
		time_freq[i] = temporal_distribution(i)
	}
	mut x_freq := [0; end_x]
	for i := 0; i < end_x; i++ {
		x_freq[i] = x_distribution(i)
	}
	mut y_freq := [0; end_y]
	for i := 0; i < end_y; i++ {
		y_freq[i] = y_distribution(i)
	}

	// #### create entities ####
	mut entities := []Entity
	for entity_counter := 0; entity_counter < number_of_entities; entity_counter++ {
		// draw temporal coordinate
		entity_time := random_integer_distribution(time_arr, time_freq, end_time)
		// draw social coordinates
		entity_x := random_integer_distribution(x_arr, x_freq, end_x)
		entity_y := random_integer_distribution(y_arr, y_freq, end_y)
		// construct entities
		entities << Entity{
			id: entity_counter,
			time: entity_time,
			x: entity_x,
			y: entity_y
		}
	}

	// create endless world
	entities_cube := cubify_entities(entities, end_time, end_x, end_y)

	// #### determine relations ####
	mut relations := []Relation
	for entity_a in entities {
		for entity_b in entities_cube {
			if (entity_a.id != entity_b.id) &&
				 (entity_b.time <= entity_a.time + neighbours_distance) &&
				 (entity_b.time >= entity_a.time - neighbours_distance) &&
				 (entity_b.x <= entity_a.x + neighbours_distance) &&
				 (entity_b.x >= entity_a.x - neighbours_distance) &&
				 (entity_b.y <= entity_a.y + neighbours_distance) &&
				 (entity_b.y >= entity_a.y - neighbours_distance) {
				distance := int(math.sqrt(
					math.pow(f64(entity_a.time - entity_b.time), 2.0) +
					math.pow(f64(entity_a.x - entity_b.x), 2.0) +
					math.pow(f64(entity_a.y - entity_b.y), 2.0)
				))
				relations << Relation{
					id_a: entity_a.id,
					id_b: entity_b.id,
					distance: distance
				}
			}
		}
	}

	// #### save result ####
	output_pajek_file := os.create(output_pajek_file_path) or {
		println(error)
    return
	}
	output_pajek_file.write('*Network popgenerator_network \n')
	output_pajek_file.write('*Vertices $number_of_entities \n')
	for entity in entities {
		output_pajek_file.write(entity.print())
	}
	output_pajek_file.write('*Edges \n')
	for relation in relations {
		output_pajek_file.write(relation.print())
	}
	output_pajek_file.close()

}
