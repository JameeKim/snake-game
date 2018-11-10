use amethyst::{
    assets::Handle,
    core::{timing::Time, transform::Transform},
    ecs::{Builder, Entities, Read, World, Write, WriteStorage},
    input::{get_key, is_close_requested, is_key_down},
    prelude::{State, StateData, Trans},
    ui::{UiFinder, UiLoader, UiPrefab, UiText},
    winit::{ElementState, VirtualKeyCode},
};
use components::{put_apple, set_game, Border, Cell};
use config::Config;
use data::Data as GameData;
use events::{GameEvent, SnakeEvent};
use render::make_camera;
use resources::{Direction, Renderers, SnakeStatus, UiData};
use std::{boxed::FnBox, time::Duration};

pub struct StartState;

impl<'a, 'b> State<GameData<'a, 'b>, GameEvent> for StartState {
    fn on_start(&mut self, StateData { world, .. }: StateData<GameData>) {
        world.exec(
            |(entities, mut ui_prefabs, mut uis, loader): (
                Entities,
                WriteStorage<Handle<UiPrefab>>,
                Write<UiData>,
                UiLoader,
            )| {
                uis.title = Some(loader.load("ui/title.ron", ()));
                uis.press_space = Some(loader.load("ui/press_space.ron", ()));
                uis.score = Some(loader.load("ui/score.ron", ()));
                entities
                    .build_entity()
                    .with(uis.title.as_ref().unwrap().clone(), &mut ui_prefabs)
                    .build();
                entities
                    .build_entity()
                    .with(uis.press_space.as_ref().unwrap().clone(), &mut ui_prefabs)
                    .build();
                entities
                    .build_entity()
                    .with(uis.score.as_ref().unwrap().clone(), &mut ui_prefabs)
                    .build();
            },
        );

        let (size, frame_duration) = {
            let config = world.read_resource::<Config>().clone();
            let renderers = Renderers::new(world, &config.colors);
            world.add_resource(renderers);
            (config.field_size, config.frame_duration)
        };

        init_board(world, size);
        make_camera(world);
        world.exec(set_game);

        let mut time = world.write_resource::<Time>();
        time.set_fixed_time(Duration::from_millis(frame_duration));
    }

    fn on_pause(&mut self, StateData { world, .. }: StateData<GameData>) {
        world.exec(|data: (WriteStorage<UiText>, UiFinder)| {
            let mut hide = ui_text_man_factory(data)(|text| {
                text.color[3] = 0.0;
            });
            vec![
                "game_title_1",
                "game_title_2",
                "game_title_3",
                "press_space",
                "score_label",
                "score_number",
            ]
            .into_iter()
            .map(|name| hide(name))
            .count();
        });
    }

    fn on_resume(&mut self, StateData { world, .. }: StateData<GameData>) {
        world.exec(|data: (WriteStorage<UiText>, UiFinder)| {
            let mut show = ui_text_man_factory(data)(|text| {
                text.color[3] = 1.0;
            });
            vec![
                "game_title_1",
                "game_title_2",
                "game_title_3",
                "press_space",
                "score_label",
                "score_number",
            ]
            .into_iter()
            .map(|name| show(name))
            .count();
        });
        world.exec(
            |(data, snake_status): ((WriteStorage<UiText>, UiFinder), Read<SnakeStatus>)| {
                ui_text_man_factory(data)(|text| {
                    text.text = (snake_status.length - 3).to_string();
                })("score_number");
            },
        );
    }

    fn handle_event(
        &mut self,
        _: StateData<GameData>,
        event: GameEvent,
    ) -> Trans<GameData<'a, 'b>, GameEvent> {
        match &event {
            GameEvent::Window(event) => {
                if is_close_requested(&event) {
                    Trans::Quit
                } else if is_key_down(&event, VirtualKeyCode::Space) {
                    Trans::Push(Box::new(PlayState))
                } else {
                    Trans::None
                }
            }
            _ => Trans::None,
        }
    }

    fn update(&mut self, data: StateData<GameData>) -> Trans<GameData<'a, 'b>, GameEvent> {
        data.data.update_core(&data.world);
        Trans::None
    }
}

impl StartState {
    pub fn new() -> Self {
        StartState
    }
}

struct PlayState;

impl<'a, 'b> State<GameData<'a, 'b>, GameEvent> for PlayState {
    fn on_start(&mut self, StateData { world, .. }: StateData<GameData>) {
        world.exec(set_game);
        world.exec(put_apple);
    }

    fn handle_event(
        &mut self,
        StateData { world, .. }: StateData<GameData>,
        event: GameEvent,
    ) -> Trans<GameData<'a, 'b>, GameEvent> {
        match &event {
            GameEvent::Window(event) => {
                if is_close_requested(&event) {
                    Trans::Quit
                } else if let Some((key, ElementState::Pressed)) = get_key(&event) {
                    match Direction::from_key_code(key) {
                        Ok(dir) => {
                            world.exec(|mut snake_status: Write<SnakeStatus>| {
                                snake_status.new_direction(dir);
                            });
                            Trans::None
                        }
                        Err(_) => {
                            if key == VirtualKeyCode::Escape {
                                Trans::Pop
                            } else {
                                Trans::None
                            }
                        }
                    }
                } else {
                    Trans::None
                }
            }
            GameEvent::Snake(SnakeEvent::Hit(_)) => Trans::Pop,
            _ => Trans::None,
        }
    }

    fn fixed_update(&mut self, data: StateData<GameData>) -> Trans<GameData<'a, 'b>, GameEvent> {
        data.data.update_play(&data.world);
        Trans::None
    }

    fn update(&mut self, data: StateData<GameData>) -> Trans<GameData<'a, 'b>, GameEvent> {
        data.data.update_core(&data.world);
        Trans::None
    }
}

fn init_board(world: &mut World, size: u8) {
    let renderers = world.read_resource::<Renderers>().clone();

    for i in 1..=size + 2 {
        for j in 1..=size + 2 {
            let mut transform = Transform::default();
            transform.translation.x = (i - 1) as f32;
            transform.translation.y = (j - 1) as f32;
            let mut entity_builder = world
                .create_entity()
                .with(Cell(i, j))
                .with(transform)
                .with(renderers.mesh.clone());

            if i == 1 || i == size + 2 || j == 1 || j == size + 2 {
                entity_builder = entity_builder
                    .with(Border)
                    .with(renderers.border_mtl.clone());
            } else {
                entity_builder = entity_builder.with(renderers.background_mtl.clone());
            }

            entity_builder.build();
        }
    }
}

fn ui_text_man_factory<'a, M>(
    (ui_text, finder): (WriteStorage<'a, UiText>, UiFinder<'a>),
) -> Box<FnBox(M) -> Box<FnMut(&'static str) + 'a> + 'a>
where
    M: Fn(&mut UiText) + 'a,
{
    Box::new(move |map_fn: M| ui_text_man(ui_text, finder, map_fn))
}

fn ui_text_man<'a, M>(
    mut ui_text: WriteStorage<'a, UiText>,
    finder: UiFinder<'a>,
    map_fn: M,
) -> Box<FnMut(&'static str) + 'a>
where
    M: Fn(&mut UiText) + 'a,
{
    Box::new(move |name: &'static str| {
        if let Some(entity) = finder.find(name) {
            if let Some(text) = ui_text.get_mut(entity) {
                map_fn(text);
            }
        }
    })
}
