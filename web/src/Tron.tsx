import React, { Component } from "react";

class Tron extends Component {
  canvas: HTMLCanvasElement | null = null;

  registerCanvas = (element: HTMLCanvasElement) => {
    this.canvas = element;
  };

  componentDidMount() {
    return;
  }

  render() {
    return <canvas ref={this.registerCanvas} />;
  }
}

export default Tron;
