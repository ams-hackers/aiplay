const WIDTH = 100;
const HEIGHT = 100;

const SCALE = 8;

const GRID_SIZE = 1;

interface StyleOptions {
  strokeStyle: string;
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
  if (options.strokeStyle) {
    ctx.strokeStyle = options.strokeStyle;
  }
  ctx.fillRect(x, y, 1, 1);
  ctx.strokeRect(x, y, 1, 1);
}

export default function createTron(ctx: CanvasRenderingContext2D) {
  const canvas = ctx.canvas;
  canvas.width = WIDTH * SCALE;
  canvas.height = HEIGHT * SCALE;

  ctx.scale(SCALE, SCALE);

  // Background
  ctx.fillRect(0, 0, WIDTH, HEIGHT);

  // Grid
  for (let x = 0; x < WIDTH; x += GRID_SIZE) {
    ctx.beginPath();
    ctx.moveTo(x, 0);
    ctx.lineTo(x, HEIGHT);
    ctx.lineWidth = 0.1;
    ctx.strokeStyle = "#333";
    ctx.stroke();
  }
  for (let y = 0; y < HEIGHT; y += GRID_SIZE) {
    ctx.beginPath();
    ctx.moveTo(0, y);
    ctx.lineTo(WIDTH, y);
    ctx.lineWidth = 0.1;
    ctx.strokeStyle = "#333";
    ctx.stroke();
  }

  cell(ctx, 10, 10, { strokeStyle: "darkYellow", fillStyle: "yellow" });

  // Walls
  for (let i = 0; i < Math.sqrt(WIDTH * HEIGHT); i++) {
    const x = Math.round(WIDTH * Math.random());
    const y = Math.round(HEIGHT * Math.random());
    cell(ctx, x, y, { strokeStyle: "darkGray", fillStyle: "gray" });
  }
}
