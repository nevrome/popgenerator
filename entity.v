struct Entity {
	id int
	time int
	social int
}

fn (e Entity) print() string {
	return e.id.str() + ' ' + e.time.str() + ' ' + e.social.str() + '\n'
}
