import React, { Component } from "react";
import { drawTron } from "./TronCanvas";
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
  for (let i = 0; i < Math.sqrt(WIDTH * HEIGHT); i++) {
    cells.push(randomCell());
  }
  return cells;
}

const game: Game = {
  width: WIDTH,
  height: HEIGHT,
  players: 2,
  walls: walls(),
  turns: [[{ x: 30, y: 30 }, { x: 50, y: 30 }]]
};

interface State {
  error: boolean;
}

class Tron extends Component<{}, State> {
  canvas: HTMLCanvasElement | null = null;
  state = {
    error: false
  };

  registerCanvas = (element: HTMLCanvasElement) => {
    this.canvas = element;
  };

  componentDidMount() {
    if (!this.canvas) return;
    const ctx = this.canvas.getContext("2d");
    if (!ctx) {
      this.setState({ error: true });
    } else {
      drawTron(ctx, game);
    }
  }

  render() {
    if (this.state.error) {
      return <p>Could not render canvas.</p>;
    } else {
      return <canvas ref={this.registerCanvas} />;
    }
  }
}

export default Tron;
