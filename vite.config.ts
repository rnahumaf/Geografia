import { defineConfig } from "vite";

export default defineConfig({
  build: {
    outDir: "dist-web",
  },
  server: {
    host: "127.0.0.1",
    port: 4173,
    strictPort: true,
    proxy: {
      "/api": "http://127.0.0.1:8765",
    },
  },
});
