from __future__ import annotations

from typing import Protocol

import keyring
from keyring.errors import KeyringError


SERVICE_NAME = "Itupararanga Flood Lab"
ACCOUNT_NAME = "openrouter_api_key"


class CredentialStore(Protocol):
    def get(self) -> str | None: ...

    def set(self, value: str) -> None: ...

    def delete(self) -> None: ...


class SystemCredentialStore:
    """Guarda a chave no cofre de credenciais do sistema operacional."""

    def get(self) -> str | None:
        try:
            return keyring.get_password(SERVICE_NAME, ACCOUNT_NAME)
        except KeyringError as error:
            raise RuntimeError(
                "O cofre de credenciais do Windows não está disponível."
            ) from error

    def set(self, value: str) -> None:
        try:
            keyring.set_password(SERVICE_NAME, ACCOUNT_NAME, value)
        except KeyringError as error:
            raise RuntimeError(
                "Não foi possível salvar a chave no cofre de credenciais do Windows."
            ) from error

    def delete(self) -> None:
        if self.get() is None:
            return
        try:
            keyring.delete_password(SERVICE_NAME, ACCOUNT_NAME)
        except KeyringError as error:
            raise RuntimeError(
                "Não foi possível remover a chave do cofre de credenciais do Windows."
            ) from error


class MemoryCredentialStore:
    """Armazenamento volátil usado somente em testes automatizados."""

    def __init__(self) -> None:
        self.value: str | None = None

    def get(self) -> str | None:
        return self.value

    def set(self, value: str) -> None:
        self.value = value

    def delete(self) -> None:
        self.value = None
