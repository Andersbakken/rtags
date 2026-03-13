import { Server } from "@modelcontextprotocol/sdk/server/index.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { loadRcOptions } from "./rc.js";
import { registerTools } from "./tools.js";

const server = new Server(
    { name: "rtags", version: "1.0.0" },
    { capabilities: { tools: {} } },
);

const rcOptions = loadRcOptions();
registerTools(server, rcOptions);

const transport = new StdioServerTransport();
await server.connect(transport);
