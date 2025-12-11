#!/usr/bin/env python3
"""
Small HTTP service to accept OCaml source (mli + ml), write them into src/lib,
run `make build` in the project root and return the compiler output (warnings filtered out).

Endpoints:
 - POST /compile  JSON body: {"name": "modname", "mli": "...", "ml": "..."}
 - GET  /health   simple health check

This implementation uses only the Python stdlib (ThreadingHTTPServer) so no extra deps are required.
"""

import io
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from threading import Lock


# Root of the project (assume this file is at project root)
PROJECT_ROOT = Path(__file__).resolve().parent
SRC_LIB_DIR = PROJECT_ROOT / "src" / "lib"

# Keep a lock per module name to avoid race when two requests write same files
name_locks = {}
name_locks_lock = Lock()


def get_name_lock(name: str) -> Lock:
	with name_locks_lock:
		if name not in name_locks:
			name_locks[name] = Lock()
		return name_locks[name]


def safe_name(name: str) -> bool:
	# allow only letters, digits and underscore and hyphen; no path separators
	return bool(re.fullmatch(r"[A-Za-z0-9_\-]+", name))


def atomic_write(path: Path, data: str) -> None:
	path.parent.mkdir(parents=True, exist_ok=True)
	# write to temp file then replace
	fd, tmp = tempfile.mkstemp(dir=str(path.parent))
	try:
		with os.fdopen(fd, "w", encoding="utf-8") as f:
			f.write(data)
		# atomic replace
		shutil.move(tmp, path)
	finally:
		if os.path.exists(tmp):
			try:
				os.remove(tmp)
			except Exception:
				pass

def remove_file(path: Path) -> None:
    try:
        if path.exists():
            path.unlink()
    except Exception:
        pass


class Handler(BaseHTTPRequestHandler):
	server_version = "IEC-Compile-Server/0.1"

	def _send_json(self, code: int, obj):
		data = json.dumps(obj, ensure_ascii=False).encode("utf-8")
		self.send_response(code)
		self.send_header("Content-Type", "application/json; charset=utf-8")
		self.send_header("Content-Length", str(len(data)))
		self.end_headers()
		self.wfile.write(data)

	def do_GET(self):
		if self.path == "/health":
			self._send_json(200, {"status": "ok"})
		else:
			self._send_json(404, {"error": "not found"})

	def do_POST(self):
		if self.path != "/compile":
			self._send_json(404, {"error": "not found"})
			return

		content_length = int(self.headers.get("Content-Length", 0))
		if content_length <= 0:
			self._send_json(400, {"error": "empty body"})
			return

		body = self.rfile.read(content_length)
		try:
			payload = json.loads(body.decode("utf-8"))
		except Exception:
			self._send_json(400, {"error": "invalid json"})
			return

		name = payload.get("name")
		mli = payload.get("mli")
		ml = payload.get("ml")

		if not (isinstance(name, str) and isinstance(mli, str) and isinstance(ml, str)):
			self._send_json(400, {"error": "fields 'name','mli','ml' must be strings"})
			return

		if not safe_name(name):
			self._send_json(400, {"error": "invalid name; allowed: A-Z a-z 0-9 _ -"})
			return

		# Acquire per-name lock while writing and building to avoid clobbering
		lock = get_name_lock(name)
		with lock:
			try:
				mli_path = SRC_LIB_DIR / f"{name}.mli"
				ml_path = SRC_LIB_DIR / f"{name}.ml"

				atomic_write(mli_path, mli)
				atomic_write(ml_path, ml)

				# run make build in project root
				proc = subprocess.run(
					["make", "build"],
					cwd=str(PROJECT_ROOT),
					stdout=subprocess.PIPE,
					stderr=subprocess.PIPE,
					text=True,
				)

				combined = proc.stdout + ("\n" + proc.stderr if proc.stderr else "")
				filtered_lines = [
					line for line in combined.strip().splitlines()
				][1:]
				out_text = "\n".join(filtered_lines).strip()
				remove_file(mli_path)
				remove_file(ml_path)

				result = {
					"returncode": proc.returncode,
					"output": out_text,
				}
				self._send_json(200, result)
			except Exception as e:
				self._send_json(500, {"error": str(e)})


def run(addr: str = "0.0.0.0", port: int = 8000):
	server = ThreadingHTTPServer((addr, port), Handler)
	print(f"Serving on {addr}:{port} (project root: {PROJECT_ROOT})")
	try:
		server.serve_forever()
	except KeyboardInterrupt:
		print("Shutting down")
		server.shutdown()


if __name__ == "__main__":
	# allow optional port arg
	p = 8000
	if len(sys.argv) >= 2:
		try:
			p = int(sys.argv[1])
		except Exception:
			pass
	run(port=p)

