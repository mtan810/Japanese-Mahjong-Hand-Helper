# Japanese Mahjong Hand Helper

Japanese Mahjong Hand Helper is a learning application that allows players to
practice forming a winning hand. A winning hand consists of a pair and four sets; a
set is either a triple or a sequence.

The application starts with a hand which consists of thirteen tiles. During each
turn, a player draws a tile and the application checks to see if a hand is a winning
hand. If it is, then congratulations to the player! If not, then the player chooses
which tile to discard and the turn ends. This process repeats until the hand is a
winning hand or there are no more tiles left to draw.

To check the demo video, please visit this link:
https://youtu.be/r2_-7uJg_aA

Please note that the tiles are rigged for demo purposes.

## Instructions

The instructions for running this application is fairly straightforward. The project
has been developed and tested using the CS50 appliance (version appliance50-
2014). As long as your Linux distro has ocaml installed, you will be able to run this
application.

There are three files in the project: hand.ml, main.ml, and Makefile. Using the
‘make’ command in the terminal with these three files in the same directory will
output a ‘main’ object which can then be run with ‘./main’ (excluding the quotes).
Once you run the command, the application will start.
