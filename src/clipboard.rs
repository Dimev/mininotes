//! system clipboard if avaibable, dummy otherwise

// clipboard, only include on desktop platforms so it can compile on termux
#[cfg(any(
    target_os = "windows",
    target_os = "macos",
    target_os = "linux",
    target_os = "freebsd",
    target_os = "dragonfly",
    target_os = "openbsd",
    target_os = "netbsd"
))]
pub use arboard::Clipboard;

#[cfg(not(any(
    target_os = "windows",
    target_os = "macos",
    target_os = "linux",
    target_os = "freebsd",
    target_os = "dragonfly",
    target_os = "openbsd",
    target_os = "netbsd"
)))]
#[allow(non_snake_case)]
pub mod Clipboard {
    pub fn new() -> Result<DummyClip, ()> {
        Err(())
    }

    pub struct DummyClip(String);

    impl DummyClip {
        pub fn get_text(&mut self) -> Result<String, ()> {
            Ok(self.0.clone())
        }

        pub fn set_text(&mut self, string: String) {
            self.0 = string;
        }
    }
}
