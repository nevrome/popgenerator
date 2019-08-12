struct Entity {
	id int
	time int
	x int
	y int
}

fn (e Entity) print() string {
	return e.id.str() + ' ' + e.time.str() + ' ' + e.x.str() + ' ' + e.y.str() + '\n'
}

// cubification function to enable endless world
fn cubify_entities(entities []Entity, end_time int, end_x int, end_y int) []Entity {
	mut cubified_entities := []Entity
	for entity in entities {

		// top
		/*
		cubified_entities << Entity{
			id: entity.id,
			time: entity.time + end_time,
			x: entity.x,
			y: entity.y
		}
		cubified_entities << Entity{
			id: entity.id,
			time: entity.time + end_time,
			x: entity.x + end_x,
			y: entity.y
		}
		cubified_entities << Entity{
			id: entity.id,
			time: entity.time + end_time,
			x: entity.x - end_x,
			y: entity.y
		}
		cubified_entities << Entity{
			id: entity.id,
			time: entity.time + end_time,
			x: entity.x,
			y: entity.y + end_y
		}
		cubified_entities << Entity{
			id: entity.id,
			time: entity.time + end_time,
			x: entity.x,
			y: entity.y - end_y
		}
		cubified_entities << Entity{
			id: entity.id,
			time: entity.time + end_time,
			x: entity.x + end_x,
			y: entity.y + end_y
		}
		cubified_entities << Entity{
			id: entity.id,
			time: entity.time + end_time,
			x: entity.x - end_x,
			y: entity.y - end_y
		}
		cubified_entities << Entity{
			id: entity.id,
			time: entity.time + end_time,
			x: entity.x + end_x,
			y: entity.y - end_y
		}
		cubified_entities << Entity{
			id: entity.id,
			time: entity.time + end_time,
			x: entity.x - end_x,
			y: entity.y + end_y
		}
		*/

		// middle
		cubified_entities << Entity{
			id: entity.id,
			time: entity.time,
			x: entity.x,
			y: entity.y
		}
		cubified_entities << Entity{
			id: entity.id,
			time: entity.time,
			x: entity.x + end_x,
			y: entity.y
		}
		cubified_entities << Entity{
			id: entity.id,
			time: entity.time,
			x: entity.x - end_x,
			y: entity.y
		}
		cubified_entities << Entity{
			id: entity.id,
			time: entity.time,
			x: entity.x,
			y: entity.y + end_y
		}
		cubified_entities << Entity{
			id: entity.id,
			time: entity.time,
			x: entity.x,
			y: entity.y - end_y
		}
		cubified_entities << Entity{
			id: entity.id,
			time: entity.time,
			x: entity.x + end_x,
			y: entity.y + end_y
		}
		cubified_entities << Entity{
			id: entity.id,
			time: entity.time,
			x: entity.x - end_x,
			y: entity.y - end_y
		}
		cubified_entities << Entity{
			id: entity.id,
			time: entity.time,
			x: entity.x + end_x,
			y: entity.y - end_y
		}
		cubified_entities << Entity{
			id: entity.id,
			time: entity.time,
			x: entity.x - end_x,
			y: entity.y + end_y
		}

		// bottom
		/*
		cubified_entities << Entity{
			id: entity.id,
			time: entity.time - end_time,
			x: entity.x,
			y: entity.y
		}
		cubified_entities << Entity{
			id: entity.id,
			time: entity.time - end_time,
			x: entity.x + end_x,
			y: entity.y
		}
		cubified_entities << Entity{
			id: entity.id,
			time: entity.time - end_time,
			x: entity.x - end_x,
			y: entity.y
		}
		cubified_entities << Entity{
			id: entity.id,
			time: entity.time - end_time,
			x: entity.x,
			y: entity.y + end_y
		}
		cubified_entities << Entity{
			id: entity.id,
			time: entity.time - end_time,
			x: entity.x,
			y: entity.y - end_y
		}
		cubified_entities << Entity{
			id: entity.id,
			time: entity.time - end_time,
			x: entity.x + end_x,
			y: entity.y + end_y
		}
		cubified_entities << Entity{
			id: entity.id,
			time: entity.time - end_time,
			x: entity.x - end_x,
			y: entity.y - end_y
		}
		cubified_entities << Entity{
			id: entity.id,
			time: entity.time - end_time,
			x: entity.x + end_x,
			y: entity.y - end_y
		}
		cubified_entities << Entity{
			id: entity.id,
			time: entity.time - end_time,
			x: entity.x - end_x,
			y: entity.y + end_y
		}
		*/

	}
	return cubified_entities
}
