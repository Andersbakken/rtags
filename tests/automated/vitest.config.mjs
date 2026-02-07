import { defineConfig } from "vitest/config";

export default defineConfig({
    test: {
        testTimeout: 60000,
        hookTimeout: 120000,
        fileParallelism: false,
        reporters: process.env.GITHUB_ACTIONS ? ["github-actions", "verbose", "junit"] : ["verbose"],
        outputFile: process.env.GITHUB_ACTIONS ? { junit: "test-results.xml" } : undefined
    }
});
