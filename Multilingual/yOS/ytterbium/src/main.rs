// I am become the standard library.
#![no_std]
// No C nor Rust runtime here. The entrance is handled by the Ferrite
// bootloader.
#![no_main]

use core::panic::PanicInfo;

/// Custom panic handler because we out here in kernal land.
#[panic_handler]
fn crash_and_burn(_info: &PanicInfo) -> ! {
    loop {}
}

/// Kernal entrance. Called from the Ferrite bootloader.
#[no_mangle]
pub extern "C" fn _kernal_start_64() -> ! {
    loop {}
}
