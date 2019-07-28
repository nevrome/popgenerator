import rand
import math
import os

fn main() {

	rand.seed(1463233)

	// #### parameters ####
	end_time := 1000
	end_social := 1000
	number_of_entities := 1000

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

	// #### logic ####
	mut entities_collector := []Entity
	for entity_counter := 1; entity_counter <= number_of_entities; entity_counter++ {
		// draw temporal coordinate
		entity_time := random_integer_distribution(time_arr, time_freq, end_time)
		// draw social coordinate
		entity_social := random_integer_distribution(social_arr, social_freq, end_social)
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
