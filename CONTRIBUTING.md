# Como contribuir

Contribuições de código, documentação, dados de referência e validação hidráulica são bem-vindas. Antes de propor uma mudança ampla, abra uma issue para registrar o problema, as fontes utilizadas e o resultado esperado.

## Ambiente de desenvolvimento

Use Python 3.11 ou posterior e Node.js 20 ou posterior:

```powershell
python -m venv .venv
.\.venv\Scripts\python.exe -m pip install -r requirements-app.txt
npm install
npm run prepare:data
```

Inicie o backend e o frontend em terminais separados:

```powershell
.\.venv\Scripts\python.exe app_server.py --no-window --port 8765
npm run dev
```

## Antes de enviar uma contribuição

Execute:

```powershell
npm test
npm run build
```

Não inclua chaves de API, credenciais, arquivos de `data/raw/`, ambientes virtuais, dependências instaladas ou builds locais. Use dados públicos com licença compatível e registre no README a origem, a resolução, a data de acesso e as limitações relevantes.

Mudanças no solver devem incluir um teste reproduzível e informar o efeito sobre conservação de massa, estabilidade numérica, tempo de execução e resultado espacial. Resultados do modelo não devem ser apresentados como mapas oficiais de inundação sem calibração e validação independente.

Ao enviar uma contribuição, você concorda que ela será distribuída sob a licença MIT deste repositório.
