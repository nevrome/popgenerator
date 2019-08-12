struct Entity {
	id int
	time int
	x int
	y int
}

fn (e Entity) print() string {
	return e.id.str() + ' ' + e.time.str() + ' ' + e.x.str() + ' ' + e.y.str() + '\n'
}
