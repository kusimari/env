"""Run a git subcommand across every repo under cwd, in parallel.

Not a standalone script. This file is packaged by
`pkgs.writers.writePython3Bin` in ./gittree-module.nix, which bakes in a
python3 interpreter with `toolz` already on sys.path. Invoking the file
directly with the system python will fail at `import toolz` unless that
interpreter has toolz installed separately.
"""

from __future__ import annotations

import os
import re
import shutil
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass

from toolz import concat, curry, pipe
from toolz.curried import map as tmap


ANSI_RE = re.compile(r"\x1b\[[0-9;]*[A-Za-z]")
INDENT = "    "
TERM_WIDTH = shutil.get_terminal_size(fallback=(120, 24)).columns
USE_COLOR = sys.stdout.isatty()

ANSI = {
    "reset": "\033[0m",
    "bold": "\033[1m",
    "red": "\033[31m",
    "green": "\033[32m",
    "yellow": "\033[33m",
    "cyan": "\033[36m",
    "dim": "\033[2m",
}


def paint(code: str, s: str) -> str:
    return f"{ANSI[code]}{s}{ANSI['reset']}" if USE_COLOR else s


def find_repos(root: str):
    for dirpath, dirnames, filenames in os.walk(root, followlinks=False):
        if ".git" in dirnames or ".git" in filenames:
            yield dirpath
            dirnames[:] = []


@dataclass
class Result:
    path: str
    args: list
    rc: int
    out: str


@curry
def run_git(args: list, path: str) -> Result:
    color_cfg = ["-c", "color.ui=always"] if USE_COLOR else []
    proc = subprocess.run(
        ["git", *color_cfg, "-C", path, *args],
        capture_output=True,
        text=True,
    )
    combined = proc.stdout + (proc.stderr if proc.returncode != 0 else "")
    return Result(path=path, args=args, rc=proc.returncode,
                  out=combined.rstrip("\n"))


def status_verdict(r: Result) -> str:
    if r.rc != 0:
        return "red"
    lines = [ANSI_RE.sub("", ln) for ln in r.out.splitlines()]
    header = lines[0] if lines else ""
    if any(not ln.startswith("##") for ln in lines):
        return "red"
    if any(marker in header for marker in ("[ahead", "[behind", "[gone]")):
        return "yellow"
    return "green"


def header_color(r: Result) -> str:
    if not r.args or r.args[0] == "status":
        return status_verdict(r)
    return "green" if r.rc == 0 else "red"


def wrap_line(width: int, line: str):
    """Hard-wrap a line to `width` visible columns, preserving ANSI codes."""
    if width <= 0:
        yield line
        return
    buf, visible, i = [], 0, 0
    while i < len(line):
        m = ANSI_RE.match(line, i)
        if m:
            buf.append(m.group(0))
            i = m.end()
            continue
        buf.append(line[i])
        visible += 1
        i += 1
        if visible >= width:
            yield "".join(buf)
            buf, visible = [], 0
    if buf:
        yield "".join(buf)


@curry
def format_result(root: str, r: Result) -> str:
    rel = os.path.relpath(r.path, root) or "."
    color = header_color(r)
    marker = "OK " if r.rc == 0 else "ERR"
    head = paint("bold", paint(color, f"{marker} {rel}"))
    if not r.out:
        return head + paint("dim", "  (no output)")
    width = max(10, TERM_WIDTH - len(INDENT)) if USE_COLOR else 10 ** 9
    wrapped = pipe(
        r.out.splitlines(),
        tmap(curry(wrap_line)(width)),
        concat,
        tmap(lambda piece: INDENT + piece),
    )
    return head + "\n" + "\n".join(wrapped)


def main(argv: list) -> int:
    args = argv[1:] or ["status", "-sb"]
    root = os.getcwd()
    repos = list(find_repos(root))
    if not repos:
        print(paint("yellow", f"no git repos under {root}"), file=sys.stderr)
        return 1

    print(paint("cyan",
                f"# {len(repos)} repo(s) under {root} — git {' '.join(args)}"))

    with ThreadPoolExecutor(max_workers=min(16, len(repos))) as pool:
        results = list(pool.map(run_git(args), repos))

    pipe(results, tmap(format_result(root)), tmap(print), list)
    return 1 if any(r.rc for r in results) else 0


if __name__ == "__main__":
    try:
        sys.exit(main(sys.argv))
    except KeyboardInterrupt:
        sys.exit(130)
