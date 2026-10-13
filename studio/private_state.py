"""Private POSIX state directory with a lifetime single-writer lease."""

from __future__ import annotations

import fcntl
import os
from pathlib import Path
import stat


class PrivateState:
    def __init__(self, path: Path):
        path.mkdir(mode=0o700, parents=True, exist_ok=True)
        info = path.lstat()
        if not stat.S_ISDIR(info.st_mode) or info.st_uid != os.getuid():
            raise ValueError("State directory must be an owned, non-symlink directory")
        path.chmod(0o700)
        self.path = path
        self.fd = os.open(path / ".writer.lock", os.O_RDWR | os.O_CREAT | os.O_NOFOLLOW, 0o600)
        try:
            os.fchmod(self.fd, 0o600)
            fcntl.flock(self.fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except OSError as error:
            os.close(self.fd)
            self.fd = -1
            raise ValueError("State directory already has a writer") from error

    def database(self, name: str) -> Path:
        target = self.path / name
        fd = os.open(target, os.O_RDWR | os.O_CREAT | os.O_NOFOLLOW, 0o600)
        try:
            info = os.fstat(fd)
            if not stat.S_ISREG(info.st_mode) or info.st_uid != os.getuid():
                raise ValueError("State database must be an owned regular file")
            os.fchmod(fd, 0o600)
        finally:
            os.close(fd)
        for suffix in ("-wal", "-shm", "-journal"):
            auxiliary = target.with_name(target.name + suffix)
            if auxiliary.exists() or auxiliary.is_symlink():
                info = auxiliary.lstat()
                if not stat.S_ISREG(info.st_mode) or info.st_uid != os.getuid():
                    raise ValueError("State database auxiliary file is unsafe")
                auxiliary.chmod(0o600)
        return target

    def close(self):
        if self.fd >= 0:
            os.close(self.fd)
            self.fd = -1
