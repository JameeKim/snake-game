use amethyst::{
    assets::{AssetStorage, Handle, Loader},
    prelude::World,
    renderer::{Material, MaterialDefaults, MeshHandle, Texture},
    ui::UiPrefab,
    winit::VirtualKeyCode,
};
use config::{Color, ColorConfig};
use render::{make_mesh, make_mtl};

pub struct SnakeStatus {
    pub length: u8,
    pub direction: Direction,
}

impl Default for SnakeStatus {
    fn default() -> Self {
        SnakeStatus {
            length: Self::default_length(),
            direction: Self::default_direction(),
        }
    }
}

impl SnakeStatus {
    pub fn default_length() -> u8 {
        3
    }

    pub fn default_direction() -> Direction {
        Direction::Right
    }

    pub fn new_direction(&mut self, new: Direction) {
        if self.direction.opposite() != new {
            self.direction = new;
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    pub fn from_key_code(key: VirtualKeyCode) -> Result<Self, &'static str> {
        match key {
            VirtualKeyCode::Up => Ok(Direction::Up),
            VirtualKeyCode::Down => Ok(Direction::Down),
            VirtualKeyCode::Left => Ok(Direction::Left),
            VirtualKeyCode::Right => Ok(Direction::Right),
            _ => Err("Unable to convert the given key to a Direction"),
        }
    }

    pub fn opposite(&self) -> Direction {
        match self {
            Direction::Up => Direction::Down,
            Direction::Down => Direction::Up,
            Direction::Left => Direction::Right,
            Direction::Right => Direction::Left,
        }
    }

    pub fn move_coord(&self, (x, y): (u8, u8)) -> (u8, u8) {
        match self {
            Direction::Up => (x, y + 1),
            Direction::Down => (x, y - 1),
            Direction::Left => (x - 1, y),
            Direction::Right => (x + 1, y),
        }
    }
}

#[derive(Clone)]
pub struct Renderers {
    pub mesh: MeshHandle,
    pub border_mtl: Material,
    pub snake_head_mtl: Material,
    pub snake_tail_mtl: Material,
    pub apple_mtl: Material,
    pub background_mtl: Material,
}

impl Renderers {
    pub fn new(world: &World, color: &ColorConfig) -> Self {
        let mesh = make_mesh(world);

        let default = world.read_resource::<MaterialDefaults>().0.clone();
        let loader = world.read_resource::<Loader>();
        let storage = world.read_resource::<AssetStorage<Texture>>();

        let gen = |color: &Color| make_mtl(&default, &loader, &storage, color);

        Renderers {
            mesh,
            border_mtl: gen(&color.border),
            snake_head_mtl: gen(&color.snake_head),
            snake_tail_mtl: gen(&color.snake_tail),
            apple_mtl: gen(&color.apple),
            background_mtl: gen(&color.background),
        }
    }
}

#[derive(Default)]
pub struct UiData {
    pub title: Option<Handle<UiPrefab>>,
    pub press_space: Option<Handle<UiPrefab>>,
    pub score: Option<Handle<UiPrefab>>,
}
