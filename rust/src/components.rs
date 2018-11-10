use amethyst::{
    ecs::{
        Component, DenseVecStorage, Entities, Join, NullStorage, Read, ReadExpect, ReadStorage,
        Write, WriteExpect, WriteStorage,
    },
    renderer::Material,
};
use config::Config;
use rand::{Rng, StdRng};
use resources::{Renderers, SnakeStatus};

#[derive(Eq, PartialEq)]
pub struct Cell(pub u8, pub u8);

impl Component for Cell {
    type Storage = DenseVecStorage<Self>;
}

impl From<(u8, u8)> for Cell {
    fn from((x, y): (u8, u8)) -> Self {
        Cell(x, y)
    }
}

impl From<Cell> for (u8, u8) {
    fn from(Cell(x, y): Cell) -> Self {
        (x, y)
    }
}

impl From<&Cell> for (u8, u8) {
    fn from(Cell(x, y): &Cell) -> Self {
        (*x, *y)
    }
}

pub struct Snake {
    pub num: u8,
}

impl Component for Snake {
    type Storage = DenseVecStorage<Self>;
}

impl Snake {
    pub fn new(num: u8) -> Self {
        Snake { num }
    }
}

#[derive(Default)]
pub struct Apple;

impl Component for Apple {
    type Storage = NullStorage<Self>;
}

#[derive(Default)]
pub struct Border;

impl Component for Border {
    type Storage = NullStorage<Self>;
}

pub fn set_game(
    (
        entities,
        mut mtls,
        mut snakes,
        mut apples,
        mut snake_status,
        borders,
        cells,
        renderers,
        config,
    ): (
        Entities,
        WriteStorage<Material>,
        WriteStorage<Snake>,
        WriteStorage<Apple>,
        Write<SnakeStatus>,
        ReadStorage<Border>,
        ReadStorage<Cell>,
        ReadExpect<Renderers>,
        Read<Config>,
    ),
) {
    let size = config.field_size;

    snakes.clear();
    apples.clear();
    snake_status.length = SnakeStatus::default_length();
    snake_status.direction = SnakeStatus::default_direction();

    for (entity, cell) in (&*entities, &cells).join() {
        if borders.contains(entity) {
            continue;
        }

        let (x, y) = cell.into();

        if y == size / 2 + 2 && (2..=4).contains(&x) {
            snakes.insert(entity, Snake::new(4 - x)).unwrap();
            if x == 4 {
                mtls.insert(entity, renderers.snake_head_mtl.clone())
                    .unwrap();
            } else {
                mtls.insert(entity, renderers.snake_tail_mtl.clone())
                    .unwrap();
            }
        } else {
            mtls.insert(entity, renderers.background_mtl.clone())
                .unwrap();
        }
    }
}

pub fn put_apple(
    (entities, mut apples, mut mtls, mut rng, cells, snakes, renderers, config): (
        Entities,
        WriteStorage<Apple>,
        WriteStorage<Material>,
        WriteExpect<StdRng>,
        ReadStorage<Cell>,
        // this needs to be WriteStorage bc the function will be called in the MoveSnakeSystem
        WriteStorage<Snake>,
        ReadExpect<Renderers>,
        Read<Config>,
    ),
) {
    let mut coord = gen_coord(&mut rng, config.field_size);

    while 0
        < (&cells, &snakes)
            .join()
            .filter(|(cell, _)| coord == (*cell).into())
            .count()
    {
        coord = gen_coord(&mut rng, config.field_size);
    }

    for (entity, _) in (&*entities, &cells)
        .join()
        .filter(|(_, cell)| coord == (*cell).into())
    {
        apples
            .insert(entity, Apple)
            .expect("Failed to insert Apple component");
        mtls.insert(entity, renderers.apple_mtl.clone())
            .expect("Failed to insert Material for Apple");
    }
}

fn gen_coord(rng: &mut StdRng, field_size: u8) -> (u8, u8) {
    (
        rng.gen_range(0, field_size) + 2,
        rng.gen_range(0, field_size) + 2,
    )
}
