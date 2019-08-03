struct Relation {
	id_a int
	id_b int
	distance int
}

fn (r Relation) print() string {
	return r.id_a.str() + ' ' + r.id_b.str() + ' ' + r.distance.str() + '\n'
}
