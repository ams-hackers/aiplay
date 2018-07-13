interface Cell {
  x: number;
  y: number;
}

interface Turn {
  [index: number]: Cell;
}

export interface Game {
  width: number;
  height: number;
  walls: Cell[];
  turns: Turn[];
  players: number;
}
