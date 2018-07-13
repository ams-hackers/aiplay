import React, { Component } from "react";
import { drawTron } from "./TronCanvas";
import { Game } from "./Model";
import styles from "./styles.css";

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

  registerCanvas = (element: HTMLCanvasElement) => {
    this.canvas = element;
  };

  step = (step: number, callback?: () => void) => {
    const { game } = this.props;
    this.setState(
      state => ({
        turn: Math.min(Math.max(1, state.turn + step), game.turns.length)
      }),
      callback
    );
  };

  draw = () => {
    const { game } = this.props;
    const { turn } = this.state;
    if (this.ctx) {
      drawTron(this.ctx, game, { turn });
    }
  };

  play = () => {
    this.draw();
    this.step(1, () => {
      const { turn } = this.state;
      const { game } = this.props;
      if (turn < game.turns.length) {
        requestAnimationFrame(this.play);
      } else {
        this.draw();
      }
    });
  };

  render() {
    if (this.state.error) {
      return <p>Could not render canvas.</p>;
    } else {
      console.log({ styles });
      return (
        <div>
          <canvas className={styles.canvas} ref={this.registerCanvas} />
          <button onClick={() => this.step(1)}>+</button>
          <button onClick={() => this.step(-1)}>-</button>
          <button onClick={this.play}>Play</button>
        </div>
      );
    }
  }
}

export default Tron;
