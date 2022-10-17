import { defineConfig } from 'vite';

export default defineConfig({
  root: 'src/',
  server: {
    port: 8080
  },
  css:  {
    preprocessorOptions: {
      plugins: [require("autoprefixer")],
    }
  },
  build: {
    outDir: '../inst/gallery-assets/public',
    assetsDir: '.',
    emptyOutDir: true,
    rollupOptions: {
      output: {
        assetFileNames: "rheroicons.[ext]",
        entryFileNames: "rheroicons.js"
      }
    }
  }
})