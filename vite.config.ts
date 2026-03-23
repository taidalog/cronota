import { defineConfig } from 'vite'

// https://vitejs.dev/config/
export default defineConfig({
    base: '/cronota/',
    build: {
        outDir: 'docs',
        emptyOutDir: true,
        rollupOptions: {
            input: {
                main: 'index.html',
                information_policy: 'information-policy.html'
            }
        }

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