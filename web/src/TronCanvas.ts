import { Game } from "./Model";

const SCALE = 8;

const GRID_SIZE = 1;

interface StyleOptions {
  fillStyle: string;
}

function cell(
  ctx: CanvasRenderingContext2D,
  x: number,
  y: number,
  options: Partial<StyleOptions> = {}
) {
  if (options.fillStyle) {
    ctx.fillStyle = options.fillStyle;
  }
  ctx.beginPath();
  ctx.rect(x, y, 1, 1);
  ctx.fill();
  ctx.stroke();
}

function grid(ctx: CanvasRenderingContext2D, game: Game) {
  const { width, height } = game;
  // Grid
  ctx.beginPath();
  for (let x = 0; x < width; x += GRID_SIZE) {
    ctx.moveTo(x, 0);
    ctx.lineTo(x, height);
    ctx.lineWidth = 0.1;
  }
  for (let y = 0; y < height; y += GRID_SIZE) {
    ctx.moveTo(0, y);
    ctx.lineTo(width, y);
    ctx.lineWidth = 0.1;
  }
  ctx.stroke();
}

export function drawTron(ctx: CanvasRenderingContext2D, game: Game) {
  const { width, height } = game;
  const canvas = ctx.canvas;

  canvas.width = width * SCALE;
  canvas.height = height * SCALE;

  ctx.scale(SCALE, SCALE);

  const styles = ["blue", "green", "purple", "yellow"];

  // Background
  ctx.fillRect(0, 0, width, height);
  ctx.strokeStyle = "#333";

  grid(ctx, game);

  game.walls.forEach(wall => {
    cell(ctx, wall.x, wall.y, { fillStyle: "gray" });
  });

  for (let player = 0; player < game.players; player++) {
    game.turns.forEach(turn => {
      const { x, y } = turn[player];
      cell(ctx, x, y, { fillStyle: styles[player] });
    });
  }
}
