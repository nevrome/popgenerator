struct Relation {
	id_a int
	id_b int
}

fn (r Relation) print() string {
	return r.id_a.str() + ',' + r.id_b.str() + '\n'
}
