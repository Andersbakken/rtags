import { Server } from "@modelcontextprotocol/sdk/server/index.js";
import {
    CallToolRequestSchema,
    ListToolsRequestSchema,
} from "@modelcontextprotocol/sdk/types.js";
import { execRc, execRcJson, type RcOptions } from "./rc.js";

function loc(file: string, line: number, col: number): string {
    return `${file}:${line}:${col}`;
}

const locationParams = {
    file: { type: "string" as const, description: "Absolute path to the source file" },
    line: { type: "integer" as const, description: "Line number (1-based)" },
    col: { type: "integer" as const, description: "Column number (1-based)" },
};

const TOOLS = [
    {
        name: "rtags_symbol_info",
        description:
            "Get detailed information about the C/C++ symbol at a specific file location (type, kind, definition, size, alignment, etc.)",
        inputSchema: {
            type: "object" as const,
            properties: locationParams,
            required: ["file", "line", "col"],
        },
    },
    {
        name: "rtags_follow_location",
        description:
            "Jump to the definition or declaration of the C/C++ symbol at the given location",
        inputSchema: {
            type: "object" as const,
            properties: locationParams,
            required: ["file", "line", "col"],
        },
    },
    {
        name: "rtags_references",
        description:
            "Find all references to the C/C++ symbol at the given location",
        inputSchema: {
            type: "object" as const,
            properties: {
                ...locationParams,
                virtuals: {
                    type: "boolean" as const,
                    description: "Include virtual function overrides",
                },
                max: {
                    type: "integer" as const,
                    description: "Maximum number of results (default 100)",
                },
            },
            required: ["file", "line", "col"],
        },
    },
    {
        name: "rtags_references_by_name",
        description: "Find all references to a C/C++ symbol by name",
        inputSchema: {
            type: "object" as const,
            properties: {
                name: {
                    type: "string" as const,
                    description: "Symbol name to search for",
                },
                max: {
                    type: "integer" as const,
                    description: "Maximum number of results (default 100)",
                },
            },
            required: ["name"],
        },
    },
    {
        name: "rtags_find_symbols",
        description:
            "Find C/C++ symbol definitions matching a pattern",
        inputSchema: {
            type: "object" as const,
            properties: {
                pattern: {
                    type: "string" as const,
                    description: "Symbol pattern to search for",
                },
                regexp: {
                    type: "boolean" as const,
                    description: "Treat pattern as a regular expression",
                },
                max: {
                    type: "integer" as const,
                    description: "Maximum number of results (default 100)",
                },
            },
            required: ["pattern"],
        },
    },
    {
        name: "rtags_list_symbols",
        description: "List C/C++ symbol names matching a prefix",
        inputSchema: {
            type: "object" as const,
            properties: {
                pattern: {
                    type: "string" as const,
                    description: "Prefix to filter symbols by (optional)",
                },
                max: {
                    type: "integer" as const,
                    description: "Maximum number of results (default 100)",
                },
            },
        },
    },
    {
        name: "rtags_code_complete",
        description:
            "Get code completion suggestions at a location in a C/C++ file",
        inputSchema: {
            type: "object" as const,
            properties: locationParams,
            required: ["file", "line", "col"],
        },
    },
    {
        name: "rtags_class_hierarchy",
        description:
            "Show the class inheritance hierarchy for a C/C++ class or struct at the given location",
        inputSchema: {
            type: "object" as const,
            properties: locationParams,
            required: ["file", "line", "col"],
        },
    },
    {
        name: "rtags_diagnose",
        description:
            "Get compiler diagnostics (errors, warnings) for a C/C++ file",
        inputSchema: {
            type: "object" as const,
            properties: {
                file: {
                    type: "string" as const,
                    description: "Absolute path to the source file",
                },
            },
            required: ["file"],
        },
    },
];

export function registerTools(server: Server, options: RcOptions): void {
    server.setRequestHandler(ListToolsRequestSchema, async () => ({
        tools: TOOLS,
    }));

    server.setRequestHandler(CallToolRequestSchema, async (request) => {
        const { name, arguments: args } = request.params;

        try {
            const result = await dispatch(name, args ?? {}, options);
            const text =
                typeof result === "string"
                    ? result
                    : JSON.stringify(result, null, 2);
            return {
                content: [{ type: "text", text: text || "(no results)" }],
            };
        } catch (err: unknown) {
            const message =
                err instanceof Error ? err.message : String(err);
            return {
                content: [{ type: "text", text: message }],
                isError: true,
            };
        }
    });
}

async function dispatch(
    name: string,
    args: Record<string, unknown>,
    options: RcOptions,
): Promise<unknown> {
    switch (name) {
        case "rtags_symbol_info":
            return execRcJson(
                [
                    "--symbol-info",
                    loc(
                        args.file as string,
                        args.line as number,
                        args.col as number,
                    ),
                    "--absolute-path",
                ],
                options,
            );

        case "rtags_follow_location":
            return execRc(
                [
                    "--follow-location",
                    loc(
                        args.file as string,
                        args.line as number,
                        args.col as number,
                    ),
                    "--absolute-path",
                    "--json",
                ],
                options,
            );

        case "rtags_references": {
            const rcArgs = [
                "--references",
                loc(
                    args.file as string,
                    args.line as number,
                    args.col as number,
                ),
                "--absolute-path",
            ];
            if (args.virtuals) rcArgs.push("--find-virtuals");
            rcArgs.push("--max", String(args.max ?? 100));
            return execRcJson(rcArgs, options);
        }

        case "rtags_references_by_name": {
            const rcArgs = [
                "--references-name",
                args.name as string,
                "--absolute-path",
            ];
            rcArgs.push("--max", String(args.max ?? 100));
            return execRcJson(rcArgs, options);
        }

        case "rtags_find_symbols": {
            const rcArgs = [
                "--find-symbols",
                args.pattern as string,
                "--absolute-path",
            ];
            if (args.regexp) rcArgs.push("--match-regexp");
            rcArgs.push("--max", String(args.max ?? 100));
            return execRcJson(rcArgs, options);
        }

        case "rtags_list_symbols": {
            const rcArgs = ["--list-symbols"];
            if (args.pattern) rcArgs.push(args.pattern as string);
            rcArgs.push("--max", String(args.max ?? 100));
            return execRc(rcArgs, options);
        }

        case "rtags_code_complete":
            return execRcJson(
                [
                    "--code-complete-at",
                    loc(
                        args.file as string,
                        args.line as number,
                        args.col as number,
                    ),
                    "--synchronous-completions",
                    "--absolute-path",
                ],
                options,
            );

        case "rtags_class_hierarchy":
            return execRcJson(
                [
                    "--class-hierarchy",
                    loc(
                        args.file as string,
                        args.line as number,
                        args.col as number,
                    ),
                    "--absolute-path",
                ],
                options,
            );

        case "rtags_diagnose":
            return execRcJson(
                ["--diagnose", args.file as string, "--absolute-path"],
                options,
            );

        default:
            throw new Error(`Unknown tool: ${name}`);
    }
}
