use amethyst::{
    assets::{AssetStorage, Loader},
    core::transform::Transform,
    prelude::{Builder, World},
    renderer::{Camera, Material, MeshHandle, PosTex, Projection, Texture},
};
use config::{Color, Config};

pub fn make_camera(world: &mut World) {
    let size = (world.read_resource::<Config>().field_size + 2) as f32;

    let mut transform = Transform::default();
    transform.translation.z = 1.0;

    world
        .create_entity()
        .with(transform)
        .with(Camera::from(Projection::orthographic(0.0, size, size, 0.0)))
        .build();
}

pub fn make_mtl(
    default: &Material,
    loader: &Loader,
    storage: &AssetStorage<Texture>,
    color: &Color,
) -> Material {
    Material {
        albedo: loader.load_from_data(color.to_arr().into(), (), storage),
        ..default.clone()
    }
}

pub fn make_mesh(world: &World) -> MeshHandle {
    make_mesh_from_ver(world, make_rect_ver(1.0, 1.0))
}

fn make_mesh_from_ver(world: &World, vertices: Vec<PosTex>) -> MeshHandle {
    world
        .read_resource::<Loader>()
        .load_from_data(vertices.into(), (), &world.read_resource())
}

fn make_rect_ver(width: f32, height: f32) -> Vec<PosTex> {
    vec![
        PosTex {
            position: [0.0, 0.0, 0.0],
            tex_coord: [0.0, 0.0],
        },
        PosTex {
            position: [width, 0.0, 0.0],
            tex_coord: [1.0, 0.0],
        },
        PosTex {
            position: [0.0, height, 0.0],
            tex_coord: [0.0, 1.0],
        },
        PosTex {
            position: [0.0, height, 0.0],
            tex_coord: [0.0, 1.0],
        },
        PosTex {
            position: [width, 0.0, 0.0],
            tex_coord: [1.0, 0.0],
        },
        PosTex {
            position: [width, height, 0.0],
            tex_coord: [1.0, 1.0],
        },
    ]
}
