require('esbuild').build({
  entryPoints: ['entry.js'],
  bundle: true,
  outfile: 'public/index.js',
  minify: true,
  sourcemap: false,
  target: ['chrome70', 'firefox57', 'safari11', 'edge16'],
  watch: true,
}).catch(() => process.exit(1))
