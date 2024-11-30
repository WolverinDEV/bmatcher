use bmatcher::{
    pattern,
    BinaryMatcher,
};

fn main() {
    let data: &[u8] = &[0xE8, 0x00, 0x00, 0x00, 0x00, 0x48, 0x89, 0x04, 0x24, 0xE9];
    let pattern = pattern!(
        "
        /*
         * call my_function
         * $ = follow 4 byte relative jump
         * ' = save cursor position to the matched stack)
         */
        E8 $ { ' }
        
        /* mov QWORD PTR [rsp], rax */
        48 89 04 24 

        /* jmp somewhere */
        E9 [4]
    "
    );

    let mut matcher = BinaryMatcher::new(&pattern, &data);
    let Some(match_stack) = matcher.next_match() else {
        panic!("failed to find pattern");
    };

    println!("Matched at location 0x{:X}", match_stack[0]);
    println!("Target function located at 0x{:X}", match_stack[1]);
}
