# Team 69

## Team members

```
 - Bassier Jeanne Louise - 378371, jeanne.bassier@epfl.ch
 - Grillet-Aubert Alexis Théo André - 381587, alexis.grillet-aubert@epfl.ch
 - Kliment Jakub - 380660, jakub.kliment@epfl.ch
 - Lepin Guillaume Marie - 381189, guillaume.lepin@epfl.ch
```

# 6poker9

A state-driven, multiplayer poker game for up to five players. This app offers a streamlined poker experience with live updates, seamless transitions between game phases, and an intuitive interface for players to enjoy.

## Table of Contents
- [Start of the game](#start-of-the-game)
- [User's experience](#user's-experience)
- [Features](#features)
- [The rules of Poker](#the-rules-of-poker)

## Start of the Game
Players arrive at the ScalApp page. They chose 6poker9 and enter the userIds of the players, that can go from 2 to 5.
Then the game is launched.

## User's experience

### 1. Joining And Starting The Game
- Trigger: The player accesses the game lobby or is invited to a game session.
- Experience:
  - The player select his username
  - Display a screen with the title “6poker9”, the poker table and the 5 dealer's cards turned over. The pool amount is 0 and we see an emoji 💰.
  - Each player sees its cards, balance and current bet. He sees the same infos about the other players except their cards. 

### 2. Gameplay Interaction
- Trigger: It is the player's turn to act.
- Experience:
  - Highlight the player's turn with a clear indicator that is a green dot emoji next to his name.
  - Present context-sensitive action buttons: "Check", "Call", "Raise", and "Fold". Next to "Raise" is provided an input box to set the raise amount. Alerts are thrown if the action is invalid.
  - When the choice is made, the buttons disappear.

### 3. Observing Other Players
- Trigger: Another player takes their turn.
- Experience:
  - Show a message to inform what the player did (e.g., "Player1 folded", "Player2 raises by 50").
  - Update the other players' infos dynamically with the new balance, current bet and if the player didn't fold, his turned cards.

### 4. Viewing Dealer's Cards
- Trigger: The game progresses to the Flop, Turn, or River phases.
- Experience:
- Reveal dealer's cards one phase at a time.

### 5. Showdown and Results
- Trigger: The final betting round ends, and remaining players reveal their hands.
- Experience:
  - Show each player's hand with their username and balance.
  - A message is displayed to announce the winner of the round based on the regular rules.

### 6. Post-Round and Restart
- Trigger: The round ends, and the game is ready for the next hand.
- Experience:
  - Allow players to prepare for the next round by displaying a button to click when the player is ready.
  - When all the players clicked on ready, a new round is started and cards are distributed. The game phase returns to the GamePlay Interaction one.

## Features

### **What the game offers**:
- **Multiplayer**: Up to 5 players per game.
- **Turn-Based Gameplay**: Each player can select their actions (check, call, raise, fold) during their turn.
- **Live Updates**: The balances (scores) of each player, their currentBet, their cards and those of the dealer are displayed in real time. There is a green dot emoji that follows the current player.
- **Smooth Transitions**: The game progresses automatically through various stages:
  - The player get cards and is in game. He can raise,check/call or fold.
  - The player made a choice and it's the turn of the next player.
  - The dealer's cards are shown (flop, turn, river)
  - All the cards are shown, the winner is determined and there is a waiting for everybody to click on the beReady button to play another round
- **Intuitive Interface**: Contextual actions, clear turn indicators, and essential information displayed.


## The rules of poker

### **Hand ranking:**
1. **Royal Flush**: A, K, Q, J, 10 of the same suit.  
2. **Straight Flush**: 5 consecutive cards of the same suit.  
3. **Four of a Kind**: 4 cards of the same value.  
4. **Full (Full House)**: 3 cards of one value + 2 cards of another value.  
5. **Color (Flush)**: 5 cards of the same color.  
6. **Straight**: 5 consecutive cards of different colors.  
7. **Three of a Kind**: 3 cards of the same value.  
8. **Double Pair (Two Pair)**: 2 pairs of cards of different values.  
9. **One Pair**: 2 cards of the same value.  
10. **High Card**: If no combination is formed.

### **Phases of the game:**
1. **Pre-flop**: Each player receives 2 hole cards.  
2. **Flop**: 3 dealer's cards are revealed.  
3. **Turn**: A 4th dealer's card is revealed.  
4. **River**: A 5th dealer's card is revealed.  
5. **Showdown**: The remaining players reveal their cards to determine the winner.