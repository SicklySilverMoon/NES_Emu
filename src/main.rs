mod nes;

extern crate sdl3;

use sdl3::pixels::Color;
use sdl3::event::Event;
use sdl3::keyboard::Keycode;
use std::time::Duration;

pub fn main() {
    let mut nes = nes::nes::Nes::new();
    nes.reset(); //todo: soon to be unneeded, init states set most up, but reset still loads ROM
    // while !nes.is_halted() { //temp
    //     nes.step();
    // }


    let sdl_context = sdl3::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();

    let window = video_subsystem.window("NES Emu", 256, 128)
        .position_centered()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas();
    let mut event_pump = sdl_context.event_pump().unwrap();
    'running: loop {
        canvas.set_draw_color(Color::RGB(0, 0, 0));
        canvas.clear();
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit {..} |
                Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
                    break 'running
                },
                Event::KeyDown { keycode: Some(Keycode::Space), .. } => {
                    println!("space pressed")
                }
                _ => {}
            }
        }
        if !nes.is_halted() {
            nes.step();
        }
        nes.draw_chrrom(&mut canvas);

        canvas.present();
        std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));
    }
}