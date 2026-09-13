# Itupararanga — relevo e cenário de ruptura

O repositório reúne os scripts geográficos antigos em R e uma nova aplicação para explorar o relevo de Sorocaba, a hidrografia regional e um cenário preliminar de ruptura da barragem de Itupararanga.

## Estado atual

A aplicação apresenta:

- relevo derivado do Copernicus DEM GLO-30, com sombreamento das encostas;
- sete faixas de altitude com cores e limites ajustáveis em tempo real;
- rios, córregos e o corpo da represa de Itupararanga sobre o relevo;
- rede de ruas e rodovias do OpenStreetMap;
- limite oficial do município de Sorocaba fornecido pelo IBGE;
- localização cadastrada da barragem;
- intervalo de volume mobilizável entre 247,57 hm³, usado como cenário-base, e 302,087 hm³, usado como limite superior;
- cinco classes experimentais de risco, do vermelho ao amarelo, calculadas a partir de profundidade e velocidade máximas simultâneas;
- alternância entre os contornos hidráulicos originais e a representação cartográfica suavizada;
- propagação 2D com profundidade máxima, velocidade máxima, tempo de chegada e balanço de massa;
- integração local com OpenRouter usando o agente `z-ai/glm-5.3-flash`;
- chave API protegida pelo cofre de credenciais do Windows, sem armazenamento no navegador;
- escolha persistente entre modo automático, GPU CUDA e CPU;
- pesquisa estruturada de coordenadas, características da barragem e fontes geoespaciais;
- exportação do mapa de risco visível em PNG, JPEG ou PDF, com legenda e aviso técnico.

As zonas de risco vêm de um solver raster 2D pela aproximação de inércia local das equações de águas rasas. O cálculo conserva volume, trata células secas e molhadas, aplica atrito de Manning e propaga um hidrograma triangular de ruptura sobre o relevo. Os contornos são suavizados somente na geometria exibida; as métricas permanecem calculadas na grade hidráulica. O modelo ainda não está calibrado. Não use esta versão para evacuação, licenciamento ou decisões operacionais.

## Executar

Requisitos: Python 3.11 ou posterior e Node.js 20 ou posterior. Para aceleração,
esta distribuição usa CUDA 13.x e CuPy 14.2; sem CUDA, o solver recua para NumPy/CPU.

```powershell
python -m venv .venv
.\.venv\Scripts\python.exe -m pip install -r requirements-app.txt
npm install
npm run prepare:data
npm run dev
```

Para habilitar CUDA 13.x, instale também a dependência opcional:

```powershell
.\.venv\Scripts\python.exe -m pip install -r requirements-gpu-cuda13.txt
```

Sem essa dependência ou sem uma GPU compatível, o solver usa NumPy/CPU.

Acesse `http://127.0.0.1:4173`. Em desenvolvimento, mantenha também o backend local aberto:

```powershell
.\.venv\Scripts\python.exe app_server.py --no-window --port 8765
```

Na seção **Agente e processamento**, o usuário pode salvar a chave OpenRouter, testar a
conexão, pesquisar insumos para outra barragem e escolher o modo de processamento. A chave
é gravada pelo `keyring` no Gerenciador de Credenciais do Windows. O app envia a chave ao
OpenRouter apenas no cabeçalho das chamadas solicitadas; ela não aparece em arquivos, logs
ou respostas da API local.

## Aplicativo standalone para Windows

Gere a distribuição com:

```powershell
.\build-standalone.ps1
```

O frontend compilado é criado em `dist-web\`. O executável e suas dependências serão criados em
`dist\Itupararanga-Flood-Lab\Itupararanga-Flood-Lab.exe`. O aplicativo abre uma janela
própria e inicia o servidor somente em `127.0.0.1`. O pacote inclui o runtime Python,
o frontend compilado e a integração CUDA; Node.js e Python não são necessários na máquina
que apenas executa a distribuição.

Os dados brutos ficam em `data/raw/`, fora do Git. Os produtos de visualização são gravados em `public/data/`.

O resultado numérico completo é salvo em `data/processed/hydrodynamic-grid.npz`, com terreno, profundidade máxima, velocidade máxima, tempo de chegada e limites geográficos. O resumo temporal fica em `public/data/hydrodynamic-summary.json`.

## Verificações

```powershell
npm test
npm run build
```

O fluxo de integração contínua executa essas verificações no Windows a cada push e pull request, sem usar uma chave OpenRouter real.

## Dados e hipóteses

O cenário usa os seguintes valores:

| Parâmetro | Valor | Uso atual |
| --- | ---: | --- |
| Capacidade total | 302,087 hm³ | volume armazenado na cota máxima e limite superior liberável |
| Volume útil | 247,57 hm³ | parcela mobilizável do cenário-base |
| Altura da barragem | 35 m | referência estrutural; ainda não alimenta um modelo de brecha |
| Comprimento | 415 m | referência estrutural; não equivale à largura de uma brecha |
| Volume liberado no cenário 2D | 247,57 hm³ | integral do hidrograma experimental |
| Vazão de pico | 15.000 m³/s | hipótese configurável, ainda não calibrada |
| Tempo até o pico | 60 min | hipótese configurável |
| Manning no canal / planície | 0,035 / 0,060 | valores preliminares uniformes |

A diferença entre capacidade total e volume útil é 54,517 hm³. Sem a curva cota-volume e sem a cota final da brecha, não é possível afirmar quanto desse volume residual sairia. Por isso, a interface mostra 247,57 hm³ como estimativa-base e 302,087 hm³ como limite superior conservador, não como previsão pontual.

Fontes principais:

- [Cadastro de barragens do Estado de São Paulo, 2019](https://arquivo.ambiente.sp.gov.br/publicacoes/2019/07/barragens-noestadodespaulo2019.pdf): coordenadas, altura, comprimento, tipo construtivo e capacidade total.
- [ANEEL, Resolução Homologatória nº 341/2006](https://www2.aneel.gov.br/cedoc/reh2006341.pdf): volume útil de 247,57 hm³.
- [ANA, aplicação da metodologia simplificada para áreas de inundação](https://www.ana.gov.br/arquivos/imprensa/eventosprojetos/20130717_AreasDeInundacaoAplicacaoDaMetodologia_JoseMelo.pdf): referência para a etapa hidráulica seguinte.
- [Copernicus DEM GLO-30](https://dataspace.copernicus.eu/explore-data/data-collections/copernicus-contributing-missions/collections-description/COP-DEM): relevo com amostragem aproximada de 30 m.
- [IBGE — Sorocaba, código 3552205](https://www.ibge.gov.br/cidades-e-estados/sp/sorocaba.html): limite municipal.
- [OpenStreetMap](https://www.openstreetmap.org/copyright): rede viária, rios e geometria visível da represa.

Aviso obrigatório do produto derivado do relevo: produced using Copernicus WorldDEM-30 © DLR e.V. 2010–2014 and © Airbus Defence and Space GmbH 2014–2018 provided under COPERNICUS by the European Union and ESA; all rights reserved.

O código deste repositório é distribuído sob a [licença MIT](LICENSE). Dados, mapas-base e produtos derivados de terceiros continuam sujeitos às condições de suas respectivas fontes. Consulte as atribuições acima antes de redistribuí-los.

## Modelo hidráulico atual

O módulo `scripts/hydrodynamic.py` implementa:

1. grade hidráulica detalhada de aproximadamente 92 × 100 m, recortada para Sorocaba e o corredor da barragem;
2. continuidade e momento 2D pela formulação de inércia local;
3. passo de tempo adaptativo pela condição CFL;
4. limitadores de vazão e número de Froude para impedir profundidades negativas e velocidades espúrias;
5. atrito de Manning distinto entre canal e planície;
6. hidrograma triangular com integral igual ao volume liberado;
7. profundidade e velocidade máximas simultâneas, tempo de chegada e balanço de massa.
8. execução vetorizada em CUDA/CuPy quando disponível, com fallback e opção explícita de CPU.

O resumo numérico registra `computeBackend` e `computeDevice`, permitindo auditar onde cada
resultado foi calculado. Nesta máquina, a simulação detalhada foi validada na
`NVIDIA GeForce RTX 5050 Laptop GPU` com CUDA 13.x.

Na configuração atual, a simulação experimental dura cerca de 12,17 horas. O risco visual usa o índice `profundidade × (velocidade + 0,5)`. Os parâmetros estão reunidos em `SimulationConfig` para permitir análises de sensibilidade.

## Próxima etapa de calibração

Uma mancha operacionalmente defensável ainda exige:

1. curva cota-área-volume e nível inicial do reservatório;
2. geometria, cota final e tempo de formação da brecha;
3. modelo digital do terreno condicionado ao leito e às estruturas urbanas;
4. rugosidade do vale, pontes, aterros e demais controles hidráulicos;
5. condições de contorno abertas, caso a onda alcance a borda do domínio;
6. calibração, análise de sensibilidade e comparação com um solver de referência.

O caminho previsto é manter esta interface como visualizador, calibrar a formulação local e comparar os resultados com HEC-RAS 2D ou LISFLOOD-FP quando os dados de entrada estiverem disponíveis.

## Contribuições

Consulte [CONTRIBUTING.md](CONTRIBUTING.md) para preparar o ambiente, executar as verificações e propor mudanças. Falhas de segurança devem seguir [SECURITY.md](SECURITY.md), sem exposição de credenciais ou dados privados.
