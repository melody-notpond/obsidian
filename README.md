# Obsidian
The Obsidian programming language.

## Another language???? You're crazy!!!!
I am. Deal with it. :sunglasses:

## But why though?
Three reasons:
1. Rust is giving me pretty big executables for my OS, and I don't want core userspace programs taking up 100s of megabytes of memory.
2. `rustc` is a pain to host.
3. For fun!

## Okay but what is this language?
It's Obsidian! It's a systems language with the following features:
 - Function overloading
 - There is no module system. Everything is dumped into a global namespace no matter what, and collisions are resolved by name and signature.
 - Exceptions.
 
And, the neatest feature of all:
 - Frontends are separated into separate executables!
This means that a user can define their own frontend in their favourite language, pipe its result it into the core executable and backend executables, and have the same power of Obsidian with their favourite syntax!

This means!
 - Custom user syntax!
 - Custom user macro systems!
 - Custom user features that lower to the common IR!

## Cool beans! How do I sign up!
Run:
```sh
git clone https://github.com/jenra-uwu/obsidian && cd obsidian && cargo run
```

This will run the executable.

## Wait, this doesn't do any of the things you said, you liar!
Unfortunately, this project literally started like a few days ago, so you'd have to be patient with me as I implement all the features I promised. I'll get there eventually!

## I'm interested, can I talk to you about this?
Sure! Just dm me on matrix at [@jenra:chat.unix.lgbt](https://matrix.to/#/@jenra:chat.unix.lgbt)!
