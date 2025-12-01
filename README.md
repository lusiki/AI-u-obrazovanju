


![](ai.jpg)




# AI in Croatian Education: Media Frame Analysis

<div align="center">

![Analysis Period](https://img.shields.io/badge/Period-2023--2025-blue)
![Articles Analyzed](https://img.shields.io/badge/Articles-4%2C424-green)
![Language](https://img.shields.io/badge/Language-Croatian-red)
![R](https://img.shields.io/badge/R-4.2%2B-276DC3?logo=r&logoColor=white)

**A computational analysis of how Croatian media framed generative AI in education**

</div>

---

## Overview

This repository contains a media frame analysis examining how Croatian web media covered the emergence of generative AI (ChatGPT, etc.) in education from 2023 to 2025. Using computational text analysis, we track the evolution of media narratives from initial panic through debate to integration.
The full report is available as a Quarto document [here](https://raw.githack.com/lusiki/AI-u-obrazovanju/main/AI_Education_Media_Analysis.html).


## Research Questions

1. **Volume & Timing**: How much coverage exists, and when did it peak?
2. **Framing**: Which interpretive frames dominate, and how do they shift?
3. **Actors**: Who is represented in coverage?
4. **Sources**: Do different media types frame AI differently?

## Key Findings

- **Pragmatic coverage**: Contrary to moral panic expectations, OPPORTUNITY framing outweighs THREAT framing across most of the study period
- **Clear narrative arc**: Coverage evolved from panic (early 2023) → debate → policy focus → normalization
- **Source variation**: Tabloids show higher threat framing than quality press
- **Actor focus**: Tech companies are the most frequently mentioned actors

### Narrative Phases

| Phase | Period | Character |
|-------|--------|-----------|
| Emergence | Jan–May 2023 | Panic & confusion |
| Debate | Jun–Dec 2023 | Weighing pros and cons |
| Integration | Jan–Aug 2024 | Policy and implementation focus |
| Normalization | Sep 2024+ | Routine coverage |

## Methodology

Analysis draws on framing theory (Entman, 1993), moral panic theory (Cohen, 1972), and diffusion of innovations (Rogers, 1962).

We developed Croatian-language dictionaries for 8 interpretive frames:

| Frame | Example Terms |
|-------|---------------|
| THREAT | prijetnja, opasnost, varanje, plagijat |
| OPPORTUNITY | alat, prilika, napredak, inovacija |
| REGULATION | pravilnik, zakon, ministarstvo, mjera |
| DISRUPTION | promjena, transformacija, revolucija |
| REPLACEMENT | zamjena, automatizacija, gubitak posla |
| QUALITY | halucinacija, greška, pouzdanost |
| EQUITY | nejednakost, pristup, digitalni jaz |
| COMPETENCE | vještine, pismenost, kritičko mišljenje |

## Quick Start

Render the Quarto report:

```r
quarto::quarto_render("AI_Education_Media_Analysis.qmd")
```

Or run the full R script:

```r
source("AI_Education_Media_Analysis_Complete.R")
```

## Citation

```bibtex
@misc{ai_croatian_education_2025,
  author       = {Siic, Trbusic, Males},
  title        = {Generative AI in Croatian Education: A Media Frame Analysis (2023-2025)},
  year         = {2025},
  publisher    = {GitHub},
  url          = {https://github.com/username/AI-u-obrazovanju}
}
```

## License

MIT
