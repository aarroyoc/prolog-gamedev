use std::alloc::System;
use std::cell::RefCell;
use std::collections::HashMap;
use std::io::{BufRead, BufReader, BufWriter, Read, Write};
use std::net::{TcpListener, TcpStream};
use std::thread::spawn;
use std::process::Command;
use std::rc::Rc;
use std::str::FromStr;
use std::sync::{Arc, Mutex, RwLock};
use std::time::SystemTime;
use nom::branch::alt;
use nom::bytes::complete::take_while1;
use nom::character::complete::{alpha1, digit1, space1};
use nom::character::{is_alphabetic, is_digit};
use nom::bytes::complete::tag;
use nom::IResult;
use raylib::prelude::*;

/* TODO
 sleep to not have more than 60fps in Prolog
 dynamic list of sprites
 breakout
 */

struct Position {
    x: u32,
    y: u32
}

enum ProCommand {
    Init,
    AskInput,
    SetPosition(String, u32, Position)
}

struct State {
    loading: bool,
    sprites: HashMap<String, Sprite>,
}

struct Sprite {
    texture: Texture2D,
    position: Vec<Position>,
    visible: Vec<bool>
}

struct Input {
    up: bool,
    down: bool,
    left: bool,
    right: bool
}

fn main() {
    let (mut rl, thread) = raylib::init()
        .size(640, 480)
        .title("Prolog Gamedev")
        .build();

    let listener = TcpListener::bind("127.0.0.1:7878").unwrap();

    rl.set_target_fps(60);

    let state = Arc::new(RwLock::new(State {
        sprites: load_sprites(&mut rl, &thread),
        loading: true
    }));
    let input = Arc::new(RwLock::new(Input {
        up: false,
        down: false,
        left: false,
        right: false
    }));

    Command::new("/home/aarroyoc/dev/scryer-prolog/target/release/scryer-prolog")
        .arg("breakout/main.pl")
        .arg("-g")
        .arg("main")
        .spawn()
        .expect("Couldn't run Scryer Prolog");

    /*Command::new("swipl")
        .arg("breakout/swi.pl")
        .spawn()
        .expect("Couldn't run SWI-Prolog");*/

    let state_clone = state.clone();
    let input_clone = input.clone();
    spawn(move || {
        handle_client(listener, state_clone, input_clone);
    });

    while !rl.window_should_close() {
        {
            let mut input = input.write().unwrap();
            *input = get_input(&rl);
        }

        let mut d = rl.begin_drawing(&thread);

        d.clear_background(Color::WHITE);
        {
            let state = state.read().unwrap();
            if state.loading {
                d.draw_text("Loading...", 12, 12, 20, Color::BLACK);
            } else {
                d.draw_text("Welcome to Prolog Gamedev", 12, 12, 20, Color::BLUE);
                for sprite in state.sprites.iter().map(|(_x,y)| y) {
                    for (i, position) in sprite.position.iter().enumerate() {
                        if sprite.visible[i] {
                            let x = position.x;
                            let y = position.y;
                            d.draw_texture(&sprite.texture, x as i32, y as i32, Color::WHITE);
                        }
                    }
                }
            }
        }
    }
}

fn handle_client(listener: TcpListener, state: Arc<RwLock<State>>, input: Arc<RwLock<Input>>) {
    let (stream, _socket) = listener.accept().unwrap();
    let mut reader = BufReader::new(&stream);
    let mut writer = BufWriter::new(&stream);
    let timestamp = Rc::new(RefCell::new(SystemTime::now()));
    loop {
        let mut message = String::new();
        reader.read_line(&mut message).unwrap();
        if let Ok((_, command)) = parse_command(&message) {
            execute_command(command, &mut writer, state.clone(), input.clone(), timestamp.clone());
        }
    }
}

fn parse_command(input: &str) -> IResult<&str, ProCommand> {
    let (input, command) = alt((parse_init, parse_ask_input, parse_set_position))(input)?;

    Ok((input, command))
}

fn parse_init(input: &str) -> IResult<&str, ProCommand> {
    let (input, _) = tag("init")(input)?;

    Ok((input, ProCommand::Init))
}

fn parse_ask_input(input: &str) -> IResult<&str, ProCommand> {
    let (input, _) = tag("input?")(input)?;

    Ok((input, ProCommand::AskInput))
}

fn parse_set_position(input: &str) -> IResult<&str, ProCommand> {
    let (input, _) = tag("setpos")(input)?;
    let (input, _) = space1(input)?;
    let (input, sprite) = alpha1(input)?;
    let (input, _) = space1(input)?;
    let (input, index) = digit1(input)?;
    let (input, _) = space1(input)?;
    let (input, x) = digit1(input)?;
    let (input, _) = space1(input)?;
    let (input, y) = digit1(input)?;

    let index = u32::from_str(&String::from(index)).unwrap();
    let x = u32::from_str(&String::from(x)).unwrap();
    let y = u32::from_str(&String::from(y)).unwrap();
    
    Ok((input, ProCommand::SetPosition(String::from(sprite), index, Position { x, y })))

}

fn load_sprites(rl: &mut RaylibHandle, thread: &RaylibThread) -> HashMap<String, Sprite> {
    let mut sprites = HashMap::new();
    sprites.insert("ball".to_string(), Sprite {
        texture: rl.load_texture(&thread, "media/ballBlue.png").unwrap(),
        position: Vec::new(),
        visible: Vec::new()
    });
    sprites.insert("paddle".to_string(), Sprite {
        texture: rl.load_texture(&thread, "media/paddleRed.png").unwrap(),
        position: Vec::new(),
        visible: Vec::new()
    });
    sprites.insert("block".to_string(), Sprite {
        texture: rl.load_texture(&thread, "media/element_green_rectangle.png").unwrap(),
        position: Vec::new(),
        visible: Vec::new()
    });
    sprites
}

fn get_input(rl: &RaylibHandle) -> Input {
    Input {
        up: rl.is_key_down(raylib::ffi::KeyboardKey::KEY_UP),
        down: rl.is_key_down(raylib::ffi::KeyboardKey::KEY_DOWN),
        left: rl.is_key_down(raylib::ffi::KeyboardKey::KEY_LEFT),
        right: rl.is_key_down(raylib::ffi::KeyboardKey::KEY_RIGHT)
    }
}

fn execute_command(command: ProCommand, stream: &mut BufWriter<&TcpStream>, state: Arc<RwLock<State>>, input: Arc<RwLock<Input>>, timestamp: Rc<RefCell<SystemTime>>) {
    match command {
        ProCommand::Init => {
            let mut state = state.write().unwrap();
            state.loading = false
        },
        ProCommand::AskInput => {
            let delta = {
                let mut prev_timestamp = timestamp.borrow_mut();
                let curr_timestamp = SystemTime::now();
                let delta = curr_timestamp.duration_since(*prev_timestamp).unwrap().as_nanos();
                *prev_timestamp = curr_timestamp;
                delta
            };
            let input = input.read().unwrap();
            let input_str = format!("up={} down={} left={} right={} delta={}\n", input.up, input.down, input.left, input.right, delta);
            stream.write_all(input_str.as_bytes()).unwrap();
            stream.flush().unwrap();
        }
        ProCommand::SetPosition(sprite, index, position) => {
            let mut state = state.write().unwrap();
            if let Some(sprite) = state.sprites.get_mut(&sprite) {
                let index = index as usize;
                if index < sprite.position.len(){
                    sprite.position[index].x = position.x;
                    sprite.position[index].y = position.y;
                    sprite.visible[index] = true;
                } else if index == sprite.position.len(){
                    sprite.position.push(Position { x: position.x, y: position.y});
                    sprite.visible.push(true);
                }
            }
        }
    }
}