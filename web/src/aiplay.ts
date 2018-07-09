import React from "react";
import ReactDOM from "react-dom";
import App from "./App";

const container = document.querySelector(".app");

function renderApp() {
  ReactDOM.render(React.createElement(App), container);
}

renderApp();
