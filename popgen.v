import rand

fn main() {
	
	//start_time := 0
	//end_time := 1000

	huhu := entity_generator(1, 2)
	
	huhu.print()
	
}

struct Entity {
	time int
	social int
}

fn (e Entity) print() {
	println(e.time.str() + ' ' + e.social.str()) 
} 

fn entity_generator(start_time int, end_time int) Entity {
	
	//rand.seed(1234)
	//rand.next(10)
	new_entity := Entity{time: 10, social: 15} 

	return new_entity

}




