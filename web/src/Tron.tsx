import React, { Component } from "react";
import { drawTron } from "./TronCanvas";
import { Game } from "./Model";

interface Props {
  game: Game;
}

interface State {
  turn: number;
  error: boolean;
}

class Tron extends Component<Props, State> {
  canvas: HTMLCanvasElement | null = null;
  ctx: CanvasRenderingContext2D | null = null;

  state = {
    turn: 1,
    error: false
  };

  registerCanvas = (element: HTMLCanvasElement) => {
    this.canvas = element;
  };

  step = (step: number) => {
    const { game } = this.props;
    this.setState(state => ({
      turn: Math.min(Math.max(1, state.turn + step), game.turns.length)
    }));
  };

  componentDidMount() {
    if (!this.canvas) return;
    this.ctx = this.canvas.getContext("2d");
    if (!this.ctx) {
      this.setState({ error: true });
    } else {
      this.draw();
    }
  }

  componentDidUpdate() {
    this.draw();
  }

  draw() {
    const { game } = this.props;
    const { turn } = this.state;
    if (this.ctx) {
      drawTron(this.ctx, game, { turn });
    }
  }

  render() {
    if (this.state.error) {
      return <p>Could not render canvas.</p>;
    } else {
      return (
        <div>
          <canvas ref={this.registerCanvas} />
          <button onClick={() => this.step(1)}>+</button>
          <button onClick={() => this.step(-1)}>-</button>
        </div>
      );
    }
  }
}

export default Tron;
