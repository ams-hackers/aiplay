import React, { Component } from "react";
import createTron from "./TronCanvas";

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
      createTron(ctx);
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
