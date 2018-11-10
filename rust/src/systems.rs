use amethyst::{
    core::{bundle::Result as BundleResult, SystemBundle},
    ecs::{
        DispatcherBuilder, Entities, Join, Read, ReadExpect, ReadStorage, System, Write,
        WriteExpect, WriteStorage,
    },
    renderer::Material,
    shrev::EventChannel,
};
use components::{put_apple, Apple, Border, Cell, Snake};
use config::Config;
use events::{HitTarget, SnakeEvent};
use rand::StdRng;
use resources::{Renderers, SnakeStatus};

// TODO change all `unwrap`s to `expect`

pub struct PlaySystemBundle;

impl<'a, 'b> SystemBundle<'a, 'b> for PlaySystemBundle {
    fn build(self, builder: &mut DispatcherBuilder<'a, 'b>) -> BundleResult<()> {
        builder.add(MoveSnakeSystem, "move_snake_system", &[]);
        Ok(())
    }
}

struct MoveSnakeSystem;

impl<'s> System<'s> for MoveSnakeSystem {
    type SystemData = (
        Entities<'s>,
        WriteStorage<'s, Material>,
        WriteStorage<'s, Snake>,
        WriteStorage<'s, Apple>,
        WriteExpect<'s, StdRng>,
        Write<'s, SnakeStatus>,
        Write<'s, EventChannel<SnakeEvent>>,
        ReadStorage<'s, Cell>,
        ReadStorage<'s, Border>,
        ReadExpect<'s, Renderers>,
        Read<'s, Config>,
    );

    fn run(
        &mut self,
        (
            entities,
            mut mtls,
            mut snakes,
            mut apples,
            rng,
            mut snake_status,
            mut event_channel,
            cells,
            borders,
            renderers,
            config,
        ): Self::SystemData,
    ) {
        // get where the head will be next
        let mut next_head_coord: Option<(u8, u8)> = None;
        for (_, cell) in (&snakes, &cells).join().filter(|(snake, _)| snake.num == 0) {
            next_head_coord = Some(snake_status.direction.move_coord(cell.into()));
        }
        if next_head_coord.is_none() {
            return ();
        }
        let next_head_coord = next_head_coord.unwrap();

        // get the entity of where the next head will be
        let mut next_head_entity = None;
        for (entity, _) in (&*entities, &cells)
            .join()
            .filter(|(_, cell)| next_head_coord == (*cell).into())
        {
            next_head_entity = Some(entity);
        }

        // quit if the next head entity cannot be found
        if next_head_entity.is_none() {
            return ();
        }
        let next_head_entity = next_head_entity.unwrap();

        // check any collisions
        let mut if_ate = None;
        if apples.contains(next_head_entity) {
            // the snake will eat the apple
            event_channel.single_write(SnakeEvent::Eat);
            if_ate = Some(());
        } else if borders.contains(next_head_entity) {
            // the snake will collide into the border
            event_channel.single_write(SnakeEvent::Hit(HitTarget::Border));
            return ();
        } else if let Some(snake) = snakes.get(next_head_entity) {
            if snake.num < snake_status.length - 1 {
                // the snake will collide into itself
                event_channel.single_write(SnakeEvent::Hit(HitTarget::Itself));
                return ();
            }
        }
        let if_ate = match if_ate {
            Some(_) => true,
            None => false,
        };

        // increase snake numbers and check the cells to change
        let mut tail_to_remove = None;
        let mut old_head = None;
        for (entity, snake) in (&*entities, &mut snakes).join() {
            snake.num += 1;
            if snake.num == 1 {
                old_head = Some(entity);
            } else if !if_ate && snake.num == snake_status.length {
                tail_to_remove = Some(entity);
            }
        }

        // color and set the next head
        for (entity, _) in (&*entities, &cells)
            .join()
            .filter(|(_, cell)| next_head_coord == (*cell).into())
        {
            if if_ate {
                apples.remove(entity);
            }
            mtls.insert(entity, renderers.snake_head_mtl.clone())
                .unwrap();
            snakes.insert(entity, Snake::new(0)).unwrap();
        }

        // remove and recolor the last tail
        if let Some(entity) = tail_to_remove {
            snakes.remove(entity);
            mtls.insert(entity, renderers.background_mtl.clone())
                .unwrap();
        }

        // recolor the previous head to tail
        if let Some(entity) = old_head {
            mtls.insert(entity, renderers.snake_tail_mtl.clone())
                .unwrap();
        }

        // increment the snake length
        if if_ate {
            snake_status.length += 1;
            put_apple((
                entities, apples, mtls, rng, cells, snakes, renderers, config,
            ));
        }
    }
}
