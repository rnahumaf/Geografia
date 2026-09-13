# Segurança

Não publique chaves OpenRouter, credenciais, tokens ou informações pessoais em issues, logs ou arquivos do repositório.

Para relatar uma vulnerabilidade, use uma comunicação privada com o mantenedor ou o recurso de aviso de segurança do GitHub, quando disponível. Informe a versão afetada, o impacto observado e uma forma mínima de reprodução. Não inclua um segredo real: use uma credencial de teste revogada ou um valor fictício.

A chave OpenRouter é armazenada pelo `keyring` no cofre de credenciais do sistema operacional. O projeto não deve gravá-la em arquivos de configuração, no navegador ou em telemetria.
