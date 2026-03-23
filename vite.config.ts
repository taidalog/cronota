import { defineConfig } from 'vite'

// https://vitejs.dev/config/
export default defineConfig({
    base: '/cronota/',
    build: {
        outDir: 'docs',
        emptyOutDir: true
    },
    clearScreen: false,
    server: {
        port: 8080,
        watch: {
            ignored: [
                "**/*.fs" // Don't watch F# files
            ]
        }
    }
})