# Kings in the Corner

![](https://upload.wikimedia.org/wikipedia/commons/d/d6/Kings_in_the_Corner_card_game.jpg)

Developed by Aidan Denlinger, Rohin Garg, Amber Olsen, and Mayank Sharan (Group 42). This is a course project for [CSE 230 | Fall 2022 | UCSD](https://ucsd-cse230-np.github.io/web/).

## Background

The rules of Kings in the Corner can be found [here](https://en.wikipedia.org/wiki/Kings_in_the_Corner#Rules), and a video explanation can be found [here](https://www.youtube.com/watch?v=Rn37rjl-aNM).

### Goals

Our goal is to develop a 2-4 player networked game of “Kings In The Corner” using Haskell. The terminal interface should make it easy to learn the rules, display the controls and start a game. The game should be navigable with the arrow keys and enter button to select cards and piles. It should be easy to start and join a multiplayer game from the menu. If time permits, we will also develop an AI player to allow for single player local play.

#### Basic UX

- Welcome screen with toggle options for local / networked, number of players, rules of the game, controls
- Game screen with cards similar to the solitaire project linked below
- A completion screen with options to exit or start over

#### Libraries and references

- [brick](https://github.com/jtdaugherty/brick/) library for TUI
- [Hspec](https://hspec.github.io) for unit testing
- [microlens-platform](https://hackage.haskell.org/package/microlens-platform) library for game state handling
- [network](https://hackage.haskell.org/package/network-2.6.3.1/docs/Network.html) to handle networked gaming with UDP
- [Solitaire using brick](https://github.com/ambuc/solitaire) for referencing card game implementation

#### Tentative Timeline

| Task                | Duration         | Week         |
| :---                    | :----               | :---         |
| Basic Setup & Design         | 2 days               | Week 7       |
| Starter code            | 2 days               | Week 7          |
| Testing & Build Setup    | 2 days        | Week 8    |
| Local 2 player game        | 3 days        | Week 8    |
| Local 2 - 4 player game    | 2 days        | Week 9    |
| Networked game        | 3 days        | Week 9    |
| AI for local play        | 3 days        | Week 10    |
| Bug fixes, completion    | 2 days        | Week 10    |
| Demo / Presentation prep    | 2 days        | Week 10    |

## Acknowledgements
Image is from <https://commons.wikimedia.org/wiki/File:Kings_in_the_Corner_card_game.jpg>.
