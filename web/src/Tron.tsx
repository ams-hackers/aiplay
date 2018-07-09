import React, { Component } from "react";

class Tron extends Component {
  canvas: HTMLCanvasElement | null = null;

  registerCanvas = (element: HTMLCanvasElement) => {
    this.canvas = element;
  };

  componentDidMount() {}

  render() {
    return <canvas ref={this.registerCanvas} />;
  }
}

export default Tron;
