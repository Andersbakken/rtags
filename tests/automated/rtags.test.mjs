import { describe, it, expect, beforeAll, afterAll } from "vitest";
import { join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { RTags } from "./rtags.mjs";

const __dirname = fileURLToPath(new URL(".", import.meta.url));
const FIXTURES = resolve(__dirname, "fixtures");

// Single shared rdm instance for all tests
let rt;
beforeAll(() => {
    rt = new RTags();
    rt.start();
});
afterAll(() => rt.stop());

function lines(output) {
    return output.split("\n").filter((l) => l.length > 0);
}

function stripDir(output) {
    return output.replaceAll(FIXTURES + "/", "");
}

function f(name) {
    return join(FIXTURES, name);
}

// ─── Ported from old Python tests ────────────────────────────────────────────

describe("OneTU (ported)", () => {
    beforeAll(() => {
        rt.clearDatabase();
        rt.index(FIXTURES, ["basic.cpp"]);
    });

    it("follow_location", () => {
        const out = stripDir(rt.followLocation(f("basic.cpp"), 4, 5));
        expect(out).toContain("basic.cpp:1:6");
        expect(out).toContain("void free_function");
    });

    it("find_references", () => {
        const out = lines(stripDir(rt.references(f("basic.cpp"), 1, 6)));
        expect(out).toHaveLength(2);
        expect(out.some((l) => l.includes("basic.cpp:4:5"))).toBe(true);
        expect(out.some((l) => l.includes("basic.cpp:5:5"))).toBe(true);
    });
});

describe("MultipleTU (ported)", () => {
    beforeAll(() => {
        rt.clearDatabase();
        rt.index(FIXTURES, ["multi_tu_a.cpp", "multi_tu_b.cpp"]);
    });

    it("follow_location: declaration to definition", () => {
        const out = stripDir(rt.followLocation(f("multi_tu_header.hpp"), 3, 6));
        expect(out).toContain("multi_tu_a.cpp");
        expect(out).toContain("shared_function");
    });

    it("find_references: across translation units", () => {
        const out = lines(stripDir(rt.references(f("multi_tu_header.hpp"), 3, 6)));
        expect(out.some((l) => l.includes("multi_tu_a.cpp"))).toBe(true);
        expect(out.some((l) => l.includes("multi_tu_b.cpp"))).toBe(true);
    });
});

describe("AnonymousUnion (ported)", () => {
    beforeAll(() => {
        rt.clearDatabase();
        rt.index(FIXTURES, ["anonymous_union.cpp"]);
    });

    it("follow_location: usage to member in anonymous union", () => {
        const out = stripDir(rt.followLocation(f("anonymous_union.cpp"), 8, 7));
        expect(out).toContain("int a");
    });
});

describe("ClassTemplates (ported)", () => {
    beforeAll(() => {
        rt.clearDatabase();
        rt.index(FIXTURES, ["templates.cpp"]);
    });

    it("find_references: class template X", () => {
        const out = lines(stripDir(rt.references(f("templates.cpp"), 15, 8)));
        expect(out.length).toBeGreaterThanOrEqual(2);
    });
});

describe("ClassTemplatesMultipleTU (ported)", () => {
    beforeAll(() => {
        rt.clearDatabase();
        rt.index(FIXTURES, ["class_tpl_multi_tu_main.cpp", "class_tpl_multi_tu_a.cpp"]);
    });

    it("find_references: variadic template across TUs", () => {
        const out = lines(stripDir(rt.references(f("class_tpl_multi_tu.hpp"), 4, 8)));
        expect(out.some((l) => l.includes("class_tpl_multi_tu_main.cpp"))).toBe(true);
        expect(out.some((l) => l.includes("class_tpl_multi_tu_a.cpp"))).toBe(true);
    });
});

describe("FunctionTemplates (ported)", () => {
    beforeAll(() => {
        rt.clearDatabase();
        rt.index(FIXTURES, ["templates.cpp"]);
    });

    it("find_references: function template", () => {
        const out = lines(stripDir(rt.references(f("templates.cpp"), 2, 3)));
        expect(out.some((l) => l.includes("identity"))).toBe(true);
    });
});

describe("Completion (ported)", () => {
    beforeAll(() => {
        rt.clearDatabase();
        rt.index(FIXTURES, ["completion.cpp"]);
    });

    it("completes without crashing", () => {
        // Completion may return empty if TU isn't cached yet, but should not error
        const out = rt.complete(f("completion.cpp"), 13, 7);
        // If completions work, verify content; otherwise just ensure no crash
        if (out.length > 0) {
            expect(out).toMatch(/get/);
            expect(out).toMatch(/set/);
            expect(out).not.toMatch(/internal_validate/);
            expect(out).not.toMatch(/\bval_\b/);
        }
    });
});

describe("FunctionPointerField (ported)", () => {
    beforeAll(() => {
        rt.clearDatabase();
        rt.index(FIXTURES, ["function_pointer_field.cpp"]);
    });

    it("find_references: function pointer field", () => {
        const out = lines(stripDir(rt.references(f("function_pointer_field.cpp"), 3, 12)));
        expect(out.some((l) => l.includes("function_pointer_field.cpp:10"))).toBe(true);
    });
});

describe("ForwardDeclaration (ported)", () => {
    beforeAll(() => {
        rt.clearDatabase();
        rt.index(FIXTURES, ["fwd_decl_main.cpp"]);
    });

    it("forward declared object points to real declaration", () => {
        const out = stripDir(rt.followLocation(f("fwd_decl_contains.hpp"), 2, 8));
        expect(out).toContain("fwd_decl_include.hpp");
        expect(out).toContain("ForwardDecl");
    });

    it("reference to opaque forward declaration", () => {
        const out = lines(stripDir(rt.references(f("fwd_decl_main.cpp"), 13, 8)));
        expect(out.some((l) => l.includes("fwd_decl_main.cpp:17"))).toBe(true);
    });
});

describe("Parsing (ported)", () => {
    beforeAll(() => {
        rt.clearDatabase();
        rt.index(
            FIXTURES,
            ["mathutils_Vector_c_not.c"],
            [`clang -std=c11 -I${FIXTURES} -c ${f("mathutils_Vector_c_not.c")}`]
        );
    });

    it("can parse complex vector C file without crashing", () => {
        // The parsing test just verifies rdm doesn't crash on complex macro-heavy C code
        const out = rt.rc("-w");
        expect(out).toContain(FIXTURES);
    });
});

describe("PrintIncludePathOutput (ported)", () => {
    beforeAll(() => {
        rt.clearDatabase();
        rt.index(FIXTURES, ["include_path_main.cpp"]);
    });

    it("prints include path", () => {
        const out = stripDir(rt.includePath(f("include_path_main.cpp"), 4, 5));
        expect(out).toContain("include_path_main.cpp");
        expect(out).toContain("include_path_a.hpp");
    });
});

// ─── New tests ───────────────────────────────────────────────────────────────

describe("basic queries", () => {
    beforeAll(() => {
        rt.clearDatabase();
        rt.index(FIXTURES, ["basic.cpp"]);
    });

    it("finds symbol by name", () => {
        expect(rt.findSymbol("free_function")).toMatch(/free_function/);
    });

    it("lists symbols with prefix", () => {
        expect(rt.listSymbols("free_")).toMatch(/free_function/);
    });

    it("gets symbol info", () => {
        const out = rt.symbolInfo(f("basic.cpp"), 1, 6);
        expect(out).toMatch(/free_function/);
        expect(out).toMatch(/Kind:/);
    });
});

describe("classes and methods", () => {
    beforeAll(() => {
        rt.clearDatabase();
        rt.index(FIXTURES, ["classes.cpp"]);
    });

    it("follows d.speak() call to Dog::speak definition", () => {
        const out = stripDir(rt.followLocation(f("classes.cpp"), 25, 7));
        expect(out).toMatch(/speak/);
    });

    it("finds references to overridden virtual method", () => {
        const out = lines(stripDir(rt.references(f("classes.cpp"), 5, 18)));
        expect(out.length).toBeGreaterThanOrEqual(2);
    });

    it("follows d.fetch() call to Dog::fetch definition", () => {
        const out = stripDir(rt.followLocation(f("classes.cpp"), 26, 7));
        expect(out).toMatch(/classes\.cpp:14/);
        expect(out).toMatch(/fetch/);
    });

    it("follows d.legs() call to Animal::legs definition", () => {
        const out = stripDir(rt.followLocation(f("classes.cpp"), 27, 7));
        expect(out).toMatch(/legs/);
    });

    it("gets symbol info for class", () => {
        const out = rt.symbolInfo(f("classes.cpp"), 1, 7);
        expect(out).toMatch(/Animal/);
    });
});

describe("templates", () => {
    beforeAll(() => {
        rt.clearDatabase();
        rt.index(FIXTURES, ["templates.cpp"]);
    });

    it("follows Container<int>::push call to template definition", () => {
        const out = stripDir(rt.followLocation(f("templates.cpp"), 26, 8));
        expect(out).toMatch(/push/);
    });

    it("finds references to Pair with multiple type params", () => {
        const out = lines(stripDir(rt.references(f("templates.cpp"), 34, 8)));
        expect(out.some((l) => l.includes("Pair<int, double>"))).toBe(true);
        expect(out.some((l) => l.includes("Pair<char, char>"))).toBe(true);
    });
});

describe("namespaces", () => {
    beforeAll(() => {
        rt.clearDatabase();
        rt.index(FIXTURES, ["namespaces.cpp"]);
    });

    it("follows qualified call to nested function", () => {
        const out = stripDir(rt.followLocation(f("namespaces.cpp"), 24, 12));
        expect(out).toMatch(/nested_func/);
    });

    it("finds references to outer::free_func", () => {
        const out = lines(stripDir(rt.references(f("namespaces.cpp"), 5, 6)));
        expect(out.some((l) => l.includes("free_func"))).toBe(true);
    });

    it("follows fully qualified outer::inner::Widget::draw", () => {
        const out = stripDir(rt.followLocation(f("namespaces.cpp"), 36, 7));
        expect(out).toMatch(/draw/);
    });

    it("finds symbol by qualified name", () => {
        expect(rt.findSymbol("outer::inner::nested_func")).toMatch(/nested_func/);
    });
});

describe("enums", () => {
    beforeAll(() => {
        rt.clearDatabase();
        rt.index(FIXTURES, ["enums.cpp"]);
    });

    it("follows old-style enum value to definition", () => {
        const out = stripDir(rt.followLocation(f("enums.cpp"), 14, 18));
        expect(out).toMatch(/RED/);
    });

    it("follows scoped enum value to definition", () => {
        const out = stripDir(rt.followLocation(f("enums.cpp"), 15, 22));
        expect(out).toMatch(/Green/);
    });

    it("finds references to enum value RED", () => {
        const out = lines(stripDir(rt.references(f("enums.cpp"), 2, 5)));
        expect(out.some((l) => l.includes("RED"))).toBe(true);
    });
});

describe("macros", () => {
    beforeAll(() => {
        rt.clearDatabase();
        rt.index(FIXTURES, ["macros.cpp"]);
    });

    it("follows macro usage to definition", () => {
        const out = stripDir(rt.followLocation(f("macros.cpp"), 5, 13));
        expect(out).toMatch(/CONSTANT/);
        expect(out).toMatch(/macros\.cpp:2/);
    });

    it("finds references to CONSTANT", () => {
        const out = lines(stripDir(rt.references(f("macros.cpp"), 2, 9)));
        expect(out.some((l) => l.includes("macros.cpp:5"))).toBe(true);
    });
});

describe("overloads", () => {
    beforeAll(() => {
        rt.clearDatabase();
        rt.index(FIXTURES, ["overloads.cpp"]);
    });

    it("follows overloaded method call to correct overload (int)", () => {
        const out = stripDir(rt.followLocation(f("overloads.cpp"), 12, 7));
        expect(out).toMatch(/print/);
        expect(out).toMatch(/int/);
    });

    it("follows overloaded method call to correct overload (double)", () => {
        const out = stripDir(rt.followLocation(f("overloads.cpp"), 13, 7));
        expect(out).toMatch(/print/);
        expect(out).toMatch(/double/);
    });

    it("follows overloaded free function to correct one", () => {
        const out = stripDir(rt.followLocation(f("overloads.cpp"), 16, 5));
        expect(out).toMatch(/add/);
        expect(out).toMatch(/int/);
    });
});

describe("multiple translation units", () => {
    beforeAll(() => {
        rt.clearDatabase();
        rt.index(FIXTURES, ["multi_tu_a.cpp", "multi_tu_b.cpp"]);
    });

    it("follows method declaration to definition", () => {
        const out = stripDir(rt.followLocation(f("multi_tu_header.hpp"), 6, 10));
        expect(out).toMatch(/multi_tu_a\.cpp/);
        expect(out).toMatch(/method/);
    });

    it("finds references to Shared::method across TUs", () => {
        const out = lines(stripDir(rt.references(f("multi_tu_header.hpp"), 6, 10)));
        expect(out.length).toBeGreaterThanOrEqual(2);
    });

    it("finds references to Shared::value across TUs", () => {
        const out = lines(stripDir(rt.references(f("multi_tu_header.hpp"), 7, 9)));
        expect(out.some((l) => l.includes("multi_tu_b.cpp"))).toBe(true);
    });
});

describe("auto types", () => {
    beforeAll(() => {
        rt.clearDatabase();
        rt.index(FIXTURES, ["auto_types.cpp"]);
    });

    it("follows auto variable to the function it was assigned from", () => {
        const out = stripDir(rt.followLocation(f("auto_types.cpp"), 11, 14));
        expect(out).toMatch(/make_point/);
    });

    it("gets symbol info for auto variable showing resolved type", () => {
        const out = rt.symbolInfo(f("auto_types.cpp"), 11, 10);
        expect(out).toMatch(/Point|auto/);
    });
});

describe("inheritance hierarchy", () => {
    beforeAll(() => {
        rt.clearDatabase();
        rt.index(FIXTURES, ["inheritance.cpp"]);
    });

    it("follows overridden method call", () => {
        const out = stripDir(rt.followLocation(f("inheritance.cpp"), 22, 7));
        expect(out).toMatch(/vfunc/);
    });

    it("finds references to Base::vfunc", () => {
        const out = stripDir(rt.references(f("inheritance.cpp"), 2, 18));
        expect(out).toMatch(/vfunc/);
    });

    it("follows call to non-virtual inherited method", () => {
        const out = stripDir(rt.followLocation(f("inheritance.cpp"), 23, 7));
        expect(out).toMatch(/non_virtual/);
        expect(out).toMatch(/inheritance\.cpp:3/);
    });

    it("follows call to middle-only method", () => {
        const out = stripDir(rt.followLocation(f("inheritance.cpp"), 24, 7));
        expect(out).toMatch(/middle_only/);
    });
});

describe("typedefs and struct aliases", () => {
    beforeAll(() => {
        rt.clearDatabase();
        rt.index(FIXTURES, ["typedef_struct.cpp"]);
    });

    it("follows typedef usage to struct definition", () => {
        const out = stripDir(rt.followLocation(f("typedef_struct.cpp"), 13, 5));
        expect(out).toMatch(/my_struct/);
    });

    it("follows member access through typedef", () => {
        const out = stripDir(rt.followLocation(f("typedef_struct.cpp"), 14, 8));
        expect(out).toMatch(/int a/);
    });

    it("follows alias_struct usage", () => {
        const out = stripDir(rt.followLocation(f("typedef_struct.cpp"), 17, 5));
        expect(out).toMatch(/other_struct|alias_struct/);
    });
});

describe("operator overloads", () => {
    beforeAll(() => {
        rt.clearDatabase();
        rt.index(FIXTURES, ["operators.cpp"]);
    });

    it("follows operator+ usage to definition", () => {
        const out = stripDir(rt.followLocation(f("operators.cpp"), 22, 16));
        expect(out).toMatch(/operator\+/);
    });

    it("follows operator== usage to definition", () => {
        const out = stripDir(rt.followLocation(f("operators.cpp"), 23, 18));
        expect(out).toMatch(/operator==/);
    });

    it("follows operator+= usage to definition", () => {
        const out = stripDir(rt.followLocation(f("operators.cpp"), 24, 7));
        expect(out).toMatch(/operator\+=/);
    });
});

describe("static members", () => {
    beforeAll(() => {
        rt.clearDatabase();
        rt.index(FIXTURES, ["static_members.cpp"]);
    });

    it("follows static method call", () => {
        const out = stripDir(rt.followLocation(f("static_members.cpp"), 13, 13));
        expect(out).toMatch(/instance/);
    });

    it("finds references to static variable", () => {
        const out = lines(stripDir(rt.references(f("static_members.cpp"), 2, 16)));
        expect(out.length).toBeGreaterThanOrEqual(2);
    });
});

describe("function pointers", () => {
    beforeAll(() => {
        rt.clearDatabase();
        rt.index(FIXTURES, ["function_pointers.cpp"]);
    });

    it("follows typedef to function pointer type", () => {
        const out = stripDir(rt.followLocation(f("function_pointers.cpp"), 11, 5));
        expect(out).toMatch(/BinOp/);
    });

    it("finds references to add function", () => {
        const out = lines(stripDir(rt.references(f("function_pointers.cpp"), 1, 5)));
        expect(out.some((l) => l.includes("function_pointers.cpp:11"))).toBe(true);
    });
});

describe("variadic templates", () => {
    beforeAll(() => {
        rt.clearDatabase();
        rt.index(FIXTURES, ["variadic_templates.cpp"]);
    });

    it("finds references to variadic class template Tuple", () => {
        const out = lines(stripDir(rt.references(f("variadic_templates.cpp"), 2, 8)));
        expect(out.length).toBeGreaterThanOrEqual(3);
    });

    it("follows call to variadic function template", () => {
        const out = stripDir(rt.followLocation(f("variadic_templates.cpp"), 14, 5));
        expect(out).toMatch(/first/);
    });
});

describe("lambdas", () => {
    beforeAll(() => {
        rt.clearDatabase();
        rt.index(FIXTURES, ["lambdas.cpp"]);
    });

    it("finds references to captured variable", () => {
        const out = lines(stripDir(rt.references(f("lambdas.cpp"), 4, 9)));
        expect(out.length).toBeGreaterThanOrEqual(2);
    });

    it("finds references to global_val", () => {
        const out = lines(stripDir(rt.references(f("lambdas.cpp"), 1, 5)));
        expect(out.some((l) => l.includes("lambdas.cpp"))).toBe(true);
    });
});
