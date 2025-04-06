# ulisp-docbrowser
Browse the ulisp documentation on the t-deck or m5cardputer, serves as a GUI demo

## Setup
There's two versions: one for the [lilygo t-deck](https://lilygo.cc/products/t-deck) and one for the [m5 cardputer](https://docs.m5stack.com/en/core/Cardputer). Setup ulisp on them following their respective instructions [t-deck](http://www.ulisp.com/show?4JAO) [cardputer](http://www.ulisp.com/show?52G4), then run the code from their respective folders in this repo. They both have somewhat modified main files so I included them in the repo instead of adding the extensions and lisplibrary files.

## Usage
 - `(doc-browser)` runs the program.
 - Move up and down the ulisp functions list with the trackball (t-deck) or arrow keys (cardputer).
 - The left and right keys on the cardputer move the documentation up and down since it doesn't always fit on the screen.
 - You can also type the function name you're looking for to shorten the list of functions shown.
 - The enter key returns the selected function name.
 - Thats basically it

## todo
- documentation for the code itself
- scroll bars
- more dynamic windows perhaps?

## Purpose
I wrote this to explore the viability of using "let over lambda" style objects to make a GUI framework as well as to explore their general functionality in ulisp
