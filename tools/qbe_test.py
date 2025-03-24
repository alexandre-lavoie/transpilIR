import dataclasses
import glob
import multiprocessing
import os
import os.path
import re
import subprocess
import sys
import tempfile

TARGETS = []
TESTS = []

COLOR = sys.stdout.isatty()
if COLOR:
    GREEN = "\033[32m"
    RED = "\033[31m"
    BLUE = "\033[34m"
    GRAY = "\033[90m"
    RESET = "\033[0m"
else:
    GREEN = ""
    RED = ""
    BLUE = ""
    GRAY = ""
    RESET = ""

PROCESS_COUNT = 8

GCC = os.environ.get("GCC") or "gcc"
QBE = os.environ.get("QBE") or "qbe"
TRANSPILIR = os.environ.get("TRANSPILIR") or "transpilir"

SECTION_RE = re.compile(r"^# >>> (.*?)$(.*?)^# <<<$", re.MULTILINE | re.DOTALL)
LINE_RE = re.compile(r"(^# |#$)", re.MULTILINE | re.DOTALL)

@dataclasses.dataclass
class Response:
    scope: str = dataclasses.field(default="")
    message: str = dataclasses.field(default="")
    error: bool = dataclasses.field(default=False)
    children: list["Response"] = dataclasses.field(default_factory=list)

def cli() -> None:
    global TESTS, TARGETS

    TARGETS = [
        "native"
    ]
    
    TESTS = [
        # test_qbe,
        test_gcc,
    ]

    test_dir = os.path.join(os.path.dirname(os.path.realpath(__file__)), "qbe", "test")
    test_files = list(sorted(glob.glob("[!_]*.ssa", root_dir=test_dir)))

    if not test_files:
        print(f"{RED}No tests{RESET}")
        return

    test_data = []

    for file in test_files:
        path = os.path.join(test_dir, file)
    
        test_data.append(path)

    pool = multiprocessing.Pool(PROCESS_COUNT)

    responses = pool.map(test, test_data)

    ok_count = 0
    for res in responses:
        ok_count += int(not res.error)

        print_response(res)

    print(f"\n{BLUE}result{RESET} {ok_count} / {len(test_files)} ({ok_count / len(test_files) * 100:.02f}%)")

def print_response(res: Response, depth: int = 0) -> None:
    prefix = "  " * depth

    print(f"{prefix}{GRAY}{res.scope}{RESET} ", end="")

    if not res.error:
        print(f"{GREEN}OK{RESET}")
        return

    if not res.message:
        print(f"{RED}ERR{RESET}")
    else:
        msg = res.message.strip()

        if "\n" in msg:
            print("")

            for line in msg.split("\n"):
                if line.strip():
                    print(f"{prefix}  {line}")
                else:
                    print("")
        else:
            print(msg)

    for c in res.children:
        print_response(c, depth + 1)

def test(ir_path: str) -> Response:
    res = Response(scope=ir_path)

    with open(ir_path) as f:
        code = f.read()

    sections = {}

    for match in SECTION_RE.finditer(code):
        sections[match.group(1)] = LINE_RE.sub("", match.group(2)).strip()

    with tempfile.TemporaryDirectory() as tmp_dir:
        for arch in TARGETS:
            res.children.append(
                test_arch(
                    ir_path=ir_path,
                    root_dir=tmp_dir,
                    arch=arch,
                    sections=sections,
                )
            )

    res.error = any(r.error for r in res.children)

    return res

def test_arch(ir_path: str, root_dir: str, arch: str, sections: dict[str, str]) -> Response:
    res = Response(scope=arch)

    arch_folder = os.path.join(root_dir, arch)
    os.mkdir(arch_folder)

    if "driver" in sections:
        driver_path = os.path.join(arch_folder, "driver.o")

        out = gcc_compile(
            arch=arch,
            lang="c",
            code=sections["driver"],
            out_path=driver_path,
        )
        out.scope = "driver"
        res.children.append(out)

        if out.error:
            res.error = True
            return res
    else:
        driver_path = None

    expected = (0, sections.get("output", ""), "")

    res.children += [
        t(
            ir_path,
            driver_path=driver_path,
            arch_folder=arch_folder,
            arch=arch,
            expected=expected
        )
        for t in TESTS
    ]

    res.error = any(r.error for r in res.children)

    return res

def test_gcc(ir_path: str, driver_path: str | None, arch_folder: str, arch: str, expected: tuple[int, str, str]) -> Response:
    res = Response(scope="gcc", error=True)

    out, asm = transpilir_assemble(
        ir="c",
        arch=arch,
        compiler="gcc",
        ir_path=ir_path,
    )
    if out.error:
        res.children.append(out)

        return res

    obj = os.path.join(arch_folder, "gcc.o")
    out = gcc_compile(
        arch=arch,
        lang="assembler",
        code=asm,
        out_path=obj,
    )
    if out.error:
        res.children.append(out)

        return res

    paths = []

    paths.append(obj)

    if driver_path:
        paths.append(driver_path)

    exe_path = os.path.join(arch_folder, "test_gcc")
    out = gcc_link(
        arch=arch,
        paths=paths,
        out_path=exe_path
    )
    if out.error:
        res.children.append(out)

        return res

    res.children.append(
        test_result(
            expected,
            run_test(arch=arch, exe_path=exe_path)
        )
    )

    res.error = any(r.error for r in res.children)

    return res

def test_qbe(ir_path: str, driver_path: str | None, arch_folder: str, arch: str, expected: tuple[int, str, str]) -> Response:
    res = Response(scope="qbe", error=True)

    out, asm = transpilir_assemble(
        ir="qbe",
        arch=arch,
        compiler="qbe",
        ir_path=ir_path,
    )
    if out.error:
        res.children.append(out)

        return res

    obj = os.path.join(arch_folder, "qbe.o")
    out = gcc_compile(
        arch=arch,
        lang="assembler",
        code=asm,
        out_path=obj,
    )
    if out.error:
        res.children.append(out)

        return res

    paths = []

    paths.append(obj)

    if driver_path:
        paths.append(driver_path)

    exe_path = os.path.join(arch_folder, "test_qbe")
    out = gcc_link(
        arch=arch,
        paths=paths,
        out_path=exe_path
    )
    if out.error:
        res.children.append(out)

        return res

    res.children.append(
        test_result(
            expected,
            run_test(arch=arch, exe_path=exe_path)
        )
    )

    res.error = any(r.error for r in res.children)

    return res

def test_result(expected: tuple[int, str, str], actual: tuple[int, bytes, bytes]) -> Response:
    expected = (expected[0], expected[1].encode(), expected[2].encode())

    return Response(
        scope="result",
        error=actual != expected,
        children=[
            Response(
                scope="expected",
                error=True,
                children=[
                    Response(
                        scope="status",
                        message=str(expected[0]),
                        error=expected[0] != actual[0]
                    ),
                    Response(
                        scope="out",
                        message=str(expected[1]) or "<empty>",
                        error=expected[1] != actual[1]
                    )
                ]
            ),
            Response(
                scope="actual",
                error=True,
                children=[
                    Response(
                        scope="status",
                        message=str(actual[0]),
                        error=expected[0] != actual[0]
                    ),
                    Response(
                        scope="out",
                        message=str(actual[1]) or "<empty>",
                        error=expected[1] != actual[1]
                    ),
                    Response(
                        scope="err",
                        message=str(actual[2]) or "<empty>",
                        error=bool(actual[2])
                    )
                ]
            ),
        ]
    )

def run_test(arch: str, exe_path: str) -> tuple[int, bytes, bytes]:
    proc = subprocess.Popen(
        [
            exe_path,
            "a",
            "b",
            "c"
        ],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )

    stdout, stderr = proc.communicate(b"")

    return proc.returncode, stdout.strip(), stderr

def transpilir_assemble(ir: str, arch: str, compiler: str, ir_path: str) -> tuple[Response, str]:
    res = Response(scope="transpilir", error=True)

    proc = subprocess.Popen(
        [
            TRANSPILIR,
            "-s", "qbe",
            "-r", ir,
            "-c", compiler,
            "-t", arch,
            "-d", "ir",
            *([] if COLOR else ["-b"]),
            ir_path
        ],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )

    stdout, stderr = proc.communicate(b"")

    if not stdout or b"error:" in stderr:
        res.children.append(
            Response(scope="error", message=stderr.decode(), error=True)
        )

        return res, ""

    res.error = False

    return res, stdout.decode()

def gcc_compile(arch: str, lang: str, code: str, out_path: str) -> Response:
    res = Response(scope="gcc_compile", error=True)

    proc = subprocess.Popen(
        [
            GCC,
            "-Wno-int-conversion",
            "-Wno-incompatible-pointer-types"
            f"-march={arch}",
            "-o", out_path,
            "-x", lang,
            "-c",
            "-"
        ],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )

    _, stderr = proc.communicate(code.encode())

    if b"error:" in stderr:
        res.message = "compiler error"

        res.children.append(
            Response(scope="error", message=stderr.decode(), error=True)
        )

        return res

    if not os.path.exists(out_path):
        res.message = "file did not output"

        return res

    res.error = False

    return res

def gcc_link(arch: str, paths: list[str], out_path: str) -> Response:
    res = Response(scope="gcc_link", error=True)

    proc = subprocess.Popen(
        [
            GCC,
            f"-march={arch}",
            "-o", out_path,
            *paths,
        ],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )

    _, stderr = proc.communicate(b"")

    if b"error:" in stderr:
        res.children.append(
            Response(scope="error", message=stderr.decode(), error=True)
        )

        return res

    if not os.path.exists(out_path):
        res.message = "not output"

        return res

    res.error = False

    return res

if __name__ == "__main__":
    cli()
