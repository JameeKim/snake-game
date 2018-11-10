use amethyst::{
    core::{bundle::Result as BundleResult, ArcThreadPool, SystemBundle},
    ecs::{Dispatcher, DispatcherBuilder},
    prelude::{DataInit, World},
};

pub struct Data<'a, 'b> {
    core_dispatcher: Dispatcher<'a, 'b>,
    play_dispatcher: Dispatcher<'a, 'b>,
}

impl<'a, 'b> Data<'a, 'b> {
    pub fn update_core(&mut self, world: &World) {
        self.core_dispatcher.dispatch(&world.res);
    }

    pub fn update_play(&mut self, world: &World) {
        self.play_dispatcher.dispatch(&world.res);
    }
}

pub struct DataBuilder<'a, 'b> {
    core: DispatcherBuilder<'a, 'b>,
    play: DispatcherBuilder<'a, 'b>,
}

impl<'a, 'b> Default for DataBuilder<'a, 'b> {
    fn default() -> Self {
        DataBuilder::new()
    }
}

impl<'a, 'b> DataInit<Data<'a, 'b>> for DataBuilder<'a, 'b> {
    fn build(self, world: &mut World) -> Data<'a, 'b> {
        let pool = world.read_resource::<ArcThreadPool>().clone();

        let mut core_dispatcher = self.core.with_pool(pool.clone()).build();
        let mut play_dispatcher = self.play.with_pool(pool.clone()).build();

        core_dispatcher.setup(&mut world.res);
        play_dispatcher.setup(&mut world.res);

        Data {
            core_dispatcher,
            play_dispatcher,
        }
    }
}

impl<'a, 'b> DataBuilder<'a, 'b> {
    pub fn new() -> Self {
        DataBuilder {
            core: DispatcherBuilder::new(),
            play: DispatcherBuilder::new(),
        }
    }

    pub fn with_base_bundle<B>(mut self, bundle: B) -> BundleResult<Self>
    where
        B: SystemBundle<'a, 'b>,
    {
        bundle.build(&mut self.core)?;
        Ok(self)
    }

    pub fn with_play_bundle<B>(mut self, bundle: B) -> BundleResult<Self>
    where
        B: SystemBundle<'a, 'b>,
    {
        bundle.build(&mut self.play)?;
        Ok(self)
    }
}
