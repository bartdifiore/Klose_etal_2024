# Prior Specifications for Hurdle Model (wintercept_v2)

## Model: `stream_quality_full_hurdle_wintercept_v2.stan`

This document summarizes all prior distributions specified in the Bayesian latent variable model with hurdle component for stream quality analysis.

---

## Prior Distributions Table

| Parameter Category | Parameter | Prior Distribution | Notes |
|:-------------------|:----------|:-------------------|:------|
| **Latent Variable** | | | |
| | $\nu_{\text{raw}}$ | $\mathcal{N}(0, 1)$ | Non-centered parameterization |
| **Stream Quality Predictors** | | | |
| | $\alpha_{\text{sq}}$ | $\mathcal{N}(0, 10)$ | Intercept for stream quality |
| | $\beta_{\text{burned}}$ | $\mathcal{N}(0, 0.5)$ | Effect of burn on stream quality |
| | $\beta_{\text{wet}}$ | $\mathcal{N}(0, 0.5)$ | Effect of wet vs dry on stream quality |
| **Factor Loadings** | | | |
| | $\beta_{\text{conduct}}$ | $\mathcal{N}(0, 2)$ | Conductivity loading |
| | $\beta_{\text{depth}}$ | $\text{Exp}(1.8)$ | Depth loading (constrained positive) |
| | $\beta_{\text{DO}}$ | $\mathcal{N}(0, 2)$ | Dissolved oxygen loading |
| | $\beta_{\text{thermal}}$ | $\mathcal{N}(0, 2)$ | Thermal index loading |
| | $\beta_{\text{canopy}}$ | $\mathcal{N}(0, 2)$ | Canopy cover loading |
| | $\beta_{\text{Q}}$ | $\mathcal{N}(0, 2)$ | Discharge loading |
| **Indicator Intercepts** | | | |
| | $\alpha_{\text{conduct}}$ | $\mathcal{N}(0, 10)$ | Conductivity intercept |
| | $\alpha_{\text{depth}}$ | $\mathcal{N}(0, 10)$ | Depth intercept |
| | $\alpha_{\text{DO}}$ | $\mathcal{N}(0, 10)$ | Dissolved oxygen intercept |
| | $\alpha_{\text{thermal}}$ | $\mathcal{N}(0, 10)$ | Thermal index intercept |
| | $\alpha_{\text{canopy}}$ | $\mathcal{N}(0, 10)$ | Canopy cover intercept |
| | $\alpha_{\text{Q}}$ | $\mathcal{N}(0, 10)$ | Discharge intercept |
| **Residual Standard Deviations** | | | |
| | $\sigma_{\text{conduct}}$ | $\text{Exp}(2)$ | Mean = 0.5 |
| | $\sigma_{\text{depth}}$ | $\text{Exp}(2)$ | Mean = 0.5 |
| | $\sigma_{\text{DO}}$ | $\text{Exp}(2)$ | Mean = 0.5 |
| | $\sigma_{\text{thermal}}$ | $\text{Exp}(2)$ | Mean = 0.5 |
| | $\sigma_{\text{canopy}}$ | $\text{Exp}(2)$ | Mean = 0.5 |
| | $\sigma_{\text{Q}}$ | $\text{Exp}(2)$ | Mean = 0.5 |
| **Hurdle Part 1: Presence** | | | |
| | $\alpha_{\text{presence}}$ | $\mathcal{N}(0, 2)$ | Logistic regression intercept |
| | $\beta_{\text{presence}}$ | $\mathcal{N}(0, 2)$ | Effect of stream quality on presence |
| **Hurdle Part 2: Abundance** | | | |
| | $\alpha_{\text{abundance}}$ | $\mathcal{N}(0, 2)$ | Log-linear intercept |
| | $\beta_{\text{abundance}}$ | $\text{Exp}(1)$ | Effect of stream quality on abundance (constrained positive) |

---

## Key Model Features

### Changes from Original Model

1. **Dissolved Oxygen ($\beta_{\text{DO}}$)**: Changed from $\text{Exp}(1.8)$ (constrained positive) to $\mathcal{N}(0, 2)$ (unconstrained)
2. **Thermal Index ($\beta_{\text{thermal}}$)**: Changed from $\mathcal{N}(-1, 1)$ to $\mathcal{N}(0, 2)$ (centered at 0 instead of -1)
3. **Conductivity ($\beta_{\text{conduct}}$)**: Changed from $\mathcal{N}(-1, 1)$ to $\mathcal{N}(0, 2)$ (centered at 0 instead of -1)
4. **Trout Presence ($\beta_{\text{presence}}$)**: Changed from $\mathcal{N}(1, 1)$ to $\mathcal{N}(0, 2)$ (centered at 0 instead of +1)

### Regularization Strategy

- **Stream quality predictors** ($\beta_{\text{burned}}$, $\beta_{\text{wet}}$): Tighter priors ($\text{SD} = 0.5$) for stronger regularization given small sample size
- **Factor loadings**: Moderate priors ($\text{SD} = 2$) to allow data to inform the relationships
- **Residual SDs**: Strong priors ($\text{Exp}(2)$) to prevent overfitting with small N
- **Intercepts**: Weakly informative priors ($\text{SD} = 10$) centered at 0

### Identification Constraints

1. **$\beta_{\text{depth}}$**: Constrained to be positive (`real<lower=0>`) for model identification
2. **$\beta_{\text{abundance}}$**: Constrained to be positive (`real<lower=0>`) for model identification
3. **Intercept ($\alpha_{\text{sq}}$)**: Included in this version (unlike models without intercept)

---

## Model Structure

### Latent Variable Model

$$\nu_i = \alpha_{\text{sq}} + \beta_{\text{burned}} \cdot \text{burned}_i + \beta_{\text{wet}} \cdot \text{wet}_i + \epsilon_i$$

where $\epsilon_i \sim \mathcal{N}(0, 1)$

### Indicator Model

$$y_{ij} = \alpha_j + \beta_j \cdot \nu_i + \delta_{ij}$$

where $\delta_{ij} \sim \mathcal{N}(0, \sigma_j)$ for indicator $j \in \{\text{conduct, depth, DO, thermal, canopy, Q}\}$

### Hurdle Model for Trout

**Part 1 (Presence/Absence):**
$$\text{logit}(P(\text{present}_i)) = \alpha_{\text{presence}} + \beta_{\text{presence}} \cdot \nu_i$$

**Part 2 (Abundance | Present):**
$$\log(\lambda_i) = \alpha_{\text{abundance}} + \beta_{\text{abundance}} \cdot \nu_i$$
$$\text{count}_i \mid \text{present}_i \sim \text{Poisson}(\lambda_i) \text{ truncated at } [1, \infty)$$

---

**Generated:** `r Sys.Date()`
