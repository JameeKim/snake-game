use amethyst::{
    core::EventReader,
    ecs::{Read, Resources, SystemData},
    renderer::Event,
    shrev::{EventChannel, ReaderId},
    ui::UiEvent,
};

#[derive(Clone, Debug, EventReader)]
#[reader(GameEventReader)]
pub enum GameEvent {
    Window(Event),
    Ui(UiEvent),
    Snake(SnakeEvent),
}

#[derive(Clone, Debug)]
pub enum SnakeEvent {
    Hit(HitTarget),
    Eat,
}

#[derive(Clone, Debug)]
pub enum HitTarget {
    Border,
    Itself,
}
