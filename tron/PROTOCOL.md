AIPlay Tron Protocol
====================

master > player

    > TRON 1              # Game ID and version
    > SIZE M N
    > WALL N1 M1
    > WALL N2 M2
    > ..
    > WALL NK MK
    > PLAYER 1 N1 M1      # position player 1
    > PLAYER 2 N2 M2      # position player 2
    > ..
    > YOU 1 2

    > TURN
    < MOVE M N
    # Wait until all other players also sent it
    > PLAYER 1 N1 M1      # new position player 1
    > PLAYER 2 N2 M2      # new position player 2
    > PLAYER 3 N3 M3      # ...
    > YOU 1 2

    > TURN
    > MOVE 23 23
    < DEAD
    # disconnected
