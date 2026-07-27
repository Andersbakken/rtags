import { Server } from "@modelcontextprotocol/sdk/server/index.js";
import {
    CallToolRequestSchema,
    ListToolsRequestSchema,
} from "@modelcontextprotocol/sdk/types.js";
import { currentFileArgs, execRc, execRcJson, type RcOptions } from "./rc.js";

function loc(file: string, line: number, col: number): string {
    return `${file}:${line}:${col}`;
}

const projectParam = {
    project: {
        type: "string" as const,
        description:
            "Optional project to search: absolute path to the project root, or to any file inside it. Defaults to the server's working directory. Use this when several projects are indexed.",
    },
};

const locationParams = {
    file: {
        type: "string" as const,
        description: "Absolute path to the source file",
    },
    line: { type: "integer" as const, description: "Line number (1-based)" },
    col: { type: "integer" as const, description: "Column number (1-based)" },
};

const maxParam = {
    max: {
        type: "integer" as const,
        description: "Maximum number of results (default 100)",
    },
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
                ...maxParam,
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
                ...maxParam,
                ...projectParam,
            },
            required: ["name"],
        },
    },
    {
        name: "rtags_find_symbols",
        description: "Find C/C++ symbol definitions matching a pattern",
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
                wildcard: {
                    type: "boolean" as const,
                    description: "Expand '*' wildcards in the pattern",
                },
                ...maxParam,
                ...projectParam,
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
                regexp: {
                    type: "boolean" as const,
                    description: "Treat pattern as a regular expression",
                },
                wildcard: {
                    type: "boolean" as const,
                    description: "Expand '*' wildcards in the pattern",
                },
                ...maxParam,
                ...projectParam,
            },
        },
    },
    {
        name: "rtags_code_complete",
        description:
            "Get code completion suggestions at a location in a C/C++ file",
        inputSchema: {
            type: "object" as const,
            properties: { ...locationParams, ...maxParam },
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
            "Get compiler diagnostics (errors, warnings) for a C/C++ file. Returns nothing when the file compiles cleanly.",
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
                    : result === null || result === undefined
                      ? ""
                      : JSON.stringify(result, null, 2);
            return {
                content: [{ type: "text", text: text || "(no results)" }],
            };
        } catch (err: unknown) {
            const message = err instanceof Error ? err.message : String(err);
            return {
                content: [{ type: "text", text: message }],
                isError: true,
            };
        }
    });
}

/** Args shared by every location based query. */
function locationArgs(
    args: Record<string, unknown>,
    flag: string,
    options: RcOptions,
): string[] {
    const file = args.file as string;
    return [
        flag,
        loc(file, args.line as number, args.col as number),
        "--absolute-path",
        // rdm's notion of "current project" is sticky global state, so always
        // tell it which file we care about.
        ...currentFileArgs(options, file),
    ];
}

/** Args shared by every name based query. */
function nameArgs(
    args: Record<string, unknown>,
    flag: string,
    pattern: string | undefined,
    options: RcOptions,
): string[] {
    const rcArgs = [flag];
    if (pattern) {
        rcArgs.push(pattern);
    }
    rcArgs.push("--absolute-path");
    rcArgs.push(...currentFileArgs(options, args.project as string | undefined));
    if (args.regexp) rcArgs.push("--match-regexp");
    if (args.wildcard) rcArgs.push("--wildcard-symbol-names");
    rcArgs.push("--max", String(args.max ?? 100));
    return rcArgs;
}

async function dispatch(
    name: string,
    args: Record<string, unknown>,
    options: RcOptions,
): Promise<unknown> {
    switch (name) {
        case "rtags_symbol_info":
            return execRcJson(
                locationArgs(args, "--symbol-info", options),
                options,
            );

        case "rtags_follow_location":
            // rc ignores --json for --follow-location.
            return execRc(
                locationArgs(args, "--follow-location", options),
                options,
            );

        case "rtags_references": {
            const rcArgs = locationArgs(args, "--references", options);
            if (args.virtuals) rcArgs.push("--find-virtuals");
            rcArgs.push("--max", String(args.max ?? 100));
            return execRcJson(rcArgs, options);
        }

        case "rtags_references_by_name":
            return execRcJson(
                nameArgs(
                    args,
                    "--references-name",
                    args.name as string,
                    options,
                ),
                options,
            );

        case "rtags_find_symbols":
            // rc ignores --json for --find-symbols.
            return execRc(
                nameArgs(
                    args,
                    "--find-symbols",
                    args.pattern as string,
                    options,
                ),
                options,
            );

        case "rtags_list_symbols":
            // rc ignores --json for --list-symbols.
            return execRc(
                nameArgs(
                    args,
                    "--list-symbols",
                    args.pattern as string | undefined,
                    options,
                ),
                options,
            );

        case "rtags_code_complete": {
            const rcArgs = locationArgs(args, "--code-complete-at", options);
            rcArgs.push("--synchronous-completions");
            // Unbounded completions can exceed 2.5MB of JSON for a single
            // location, so always cap.
            rcArgs.push("--max", String(args.max ?? 50));
            return execRcJson(rcArgs, options);
        }

        case "rtags_class_hierarchy":
            // rc ignores --json for --class-hierarchy.
            return execRc(
                locationArgs(args, "--class-hierarchy", options),
                options,
            );

        case "rtags_diagnose": {
            const file = args.file as string;
            // --diagnose on its own is asynchronous: it only asks rdm to
            // re-send diagnostics to subscribed (-m) connections and prints
            // nothing. --synchronous-diagnostics makes rc wait and print.
            return execRcJson(
                [
                    "--diagnose",
                    file,
                    "--synchronous-diagnostics",
                    "--absolute-path",
                    ...currentFileArgs(options, file),
                ],
                options,
            );
        }

        default:
            throw new Error(`Unknown tool: ${name}`);
    }
}
