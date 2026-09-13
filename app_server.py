from __future__ import annotations

import argparse
import os
import socket
import sys
import threading
import traceback
import webbrowser
from pathlib import Path

import uvicorn

from app_backend.server import app


def find_available_port(preferred: int) -> int:
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as probe:
        try:
            probe.bind(("127.0.0.1", preferred))
            return preferred
        except OSError:
            probe.bind(("127.0.0.1", 0))
            return int(probe.getsockname()[1])


def main() -> None:
    parser = argparse.ArgumentParser(description="Itupararanga Flood Lab")
    parser.add_argument("--port", type=int, default=8765)
    parser.add_argument("--no-window", action="store_true")
    args = parser.parse_args()
    port = find_available_port(args.port)
    url = f"http://127.0.0.1:{port}"

    if args.no_window:
        uvicorn.run(
            app,
            host="127.0.0.1",
            port=port,
            log_level="warning",
            log_config=None,
            access_log=False,
        )
        return

    config = uvicorn.Config(
        app,
        host="127.0.0.1",
        port=port,
        log_level="warning",
        log_config=None,
        access_log=False,
    )
    server = uvicorn.Server(config)
    server_thread = threading.Thread(target=server.run, daemon=True)
    server_thread.start()
    while not server.started and server_thread.is_alive():
        server_thread.join(0.05)

    try:
        import webview

        webview.create_window(
            "Itupararanga Flood Lab",
            url,
            width=1440,
            height=900,
            min_size=(960, 640),
        )
        webview.start()
    except ImportError:
        webbrowser.open(url)
        server_thread.join()
    finally:
        server.should_exit = True
        server_thread.join(timeout=5)


if __name__ == "__main__":
    try:
        main()
    except Exception:
        log_dir = Path(os.environ.get("LOCALAPPDATA", Path.home())) / "Itupararanga Flood Lab"
        log_dir.mkdir(parents=True, exist_ok=True)
        (log_dir / "startup-error.log").write_text(traceback.format_exc(), encoding="utf-8")
        if not getattr(sys, "frozen", False):
            raise
