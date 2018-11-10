#![feature(custom_attribute, fnbox, range_contains)]

mod components;
mod config;
mod data;
mod events;
mod render;
mod resources;
mod states;
mod systems;

#[macro_use]
extern crate amethyst;
extern crate rand;
#[macro_use]
extern crate serde_derive;

use amethyst::{
    core::transform::TransformBundle,
    prelude::{Config as AConfig, CoreApplication},
    renderer::{DisplayConfig, DrawFlat, Pipeline, PosTex, RenderBundle, Stage},
    shrev::EventChannel,
    ui::{DrawUi, UiBundle},
};
use config::Config;
use data::DataBuilder;
use events::{GameEvent, GameEventReader, SnakeEvent};
use rand::{FromEntropy, StdRng};
use states::StartState;
use systems::PlaySystemBundle;

pub fn run() -> amethyst::Result<()> {
    amethyst::start_logger(Default::default());

    let app_dir = amethyst::utils::application_root_dir();
    let res_dir = format!("{}/resources", &app_dir);
    let asset_dir = format!("{}/assets", &app_dir);

    let display_config = format!("{}/display.ron", &res_dir);
    let config_file = format!("{}/config.ron", &res_dir);

    let game_data = DataBuilder::default()
        .with_base_bundle(TransformBundle::new())?
        .with_base_bundle(RenderBundle::new(
            Pipeline::build().with_stage(
                Stage::with_backbuffer()
                    .clear_target([0.0, 0.0, 0.0, 1.0], 1.0)
                    .with_pass(DrawFlat::<PosTex>::new())
                    .with_pass(DrawUi::new()),
            ),
            Some(DisplayConfig::load(display_config)),
        ))?
        .with_base_bundle(UiBundle::<String, String>::new())?
        .with_play_bundle(PlaySystemBundle)?;
    let mut game =
        CoreApplication::<_, GameEvent, GameEventReader>::build(asset_dir, StartState::new())?
            .with_resource(Config::load(config_file))
            .with_resource(EventChannel::<SnakeEvent>::new())
            .with_resource(StdRng::from_entropy())
            .build(game_data)?;

    game.run();

    Ok(())
}
