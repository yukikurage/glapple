const canvas = document.getElementById('canvas');
canvas.width = 300.0;
canvas.height = 300.0;
const ctx = canvas.getContext('2d');

const off_canvas = document.createElement('canvas');
off_canvas.width = 300.0;
off_canvas.height = 300.0;
const off_ctx = off_canvas.getContext('2d');

const mainLoop = () => {
  off_ctx.save();

  off_ctx.clearRect(0.0, 0.0, 300.0, 300.0);
  off_ctx.fillRect (100.0, 100.0, 100.0, 100.0);

  off_ctx.restore();
  off_ctx.restore();

  ctx.clearRect(0.0, 0.0, 300.0, 300.0);
  ctx.drawImage(off_canvas, 0.0, 0.0);

  setTimeout(mainLoop, 1.0);
}

mainLoop();