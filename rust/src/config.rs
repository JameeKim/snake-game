#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Config {
    #[serde(default = "Config::default_field_size")]
    pub field_size: u8,
    #[serde(default)]
    pub colors: ColorConfig,
    #[serde(default = "Config::default_frame_duration")]
    pub frame_duration: u64,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            field_size: Self::default_field_size(),
            colors: Default::default(),
            frame_duration: Self::default_frame_duration(),
        }
    }
}

impl Config {
    fn default_field_size() -> u8 {
        15
    }

    fn default_frame_duration() -> u64 {
        100
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ColorConfig {
    #[serde(default)]
    pub border: Color,
    #[serde(default = "ColorConfig::default_snake_head")]
    pub snake_head: Color,
    #[serde(default = "ColorConfig::default_snake_tail")]
    pub snake_tail: Color,
    #[serde(default = "ColorConfig::default_apple")]
    pub apple: Color,
    #[serde(default = "ColorConfig::default_background")]
    pub background: Color,
}

impl Default for ColorConfig {
    fn default() -> Self {
        ColorConfig {
            border: Default::default(),
            snake_head: Self::default_snake_head(),
            snake_tail: Self::default_snake_tail(),
            apple: Self::default_apple(),
            background: Self::default_background(),
        }
    }
}

impl ColorConfig {
    fn default_snake_head() -> Color {
        Color(0.3, 1.0, 0.3, 1.0)
    }

    fn default_snake_tail() -> Color {
        Color(0.0, 1.0, 0.0, 1.0)
    }

    fn default_apple() -> Color {
        Color(1.0, 0.0, 0.0, 1.0)
    }

    fn default_background() -> Color {
        Color(0.7, 0.7, 0.7, 1.0)
    }
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
pub struct Color(f32, f32, f32, f32);

impl Default for Color {
    fn default() -> Self {
        Color(0.0, 0.0, 0.0, 1.0)
    }
}

impl Color {
    pub fn to_arr(&self) -> [f32; 4] {
        [self.0, self.1, self.2, self.3]
    }
}
