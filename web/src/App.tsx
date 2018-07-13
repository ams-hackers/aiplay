import React from "react";
import Tron from "./Tron";

import { Game } from "./Model";
const WIDTH = 100;
const HEIGHT = 100;

function randomCell() {
  const x = Math.floor(Math.random() * WIDTH);
  const y = Math.floor(Math.random() * HEIGHT);
  return { x, y };
}

function walls() {
  const cells = [];
  for (let i = 0; i < Math.sqrt(10 * WIDTH * HEIGHT); i++) {
    cells.push(randomCell());
  }
  return cells;
}

const game: Game = {
  width: WIDTH,
  height: HEIGHT,
  players: 2,
  walls: walls(),
  turns: [
    [{ x: 30, y: 30 }, { x: 50, y: 30 }],
    [{ x: 29, y: 30 }, { x: 50, y: 31 }],
    [{ x: 28, y: 30 }, { x: 51, y: 31 }],
    [{ x: 27, y: 30 }, { x: 51, y: 32 }],
    [{ x: 26, y: 30 }, { x: 50, y: 32 }],
    [{ x: 25, y: 30 }, { x: 49, y: 32 }],
    [{ x: 25, y: 31 }, { x: 48, y: 32 }],
    [{ x: 25, y: 32 }, { x: 47, y: 32 }],
    [{ x: 25, y: 33 }, { x: 48, y: 32 }],
    [{ x: 25, y: 34 }, { x: 47, y: 32 }],
    [{ x: 25, y: 35 }, { x: 46, y: 32 }]
  ]
};

export default function App() {
  return (
    <div>
      <h1>Hello AI</h1>
      <Tron game={game} />
    </div>
  );
}
