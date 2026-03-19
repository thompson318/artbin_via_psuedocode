# artbin Pseudocode

Translated from artbin.ado v2.1.2 and art2bin.ado v1.01.

---

## Main Function: artbin(pr, margin, alpha, power, n, aratios, ltfu, onesided, favourable, condit, local, trend, doses, nvmethod, wald, ccorrect, noround, force)

### 1. Input parsing and validation

```
npr = length(pr)
niss = (margin != NULL && margin != 0)   // NI or substantial-sup flag

// Error checks:
IF n > 0 AND power given → ERROR
IF n == 0 AND power == NULL → power = 0.8 (default)
IF npr < 2 → ERROR
IF margin != NULL AND npr > 2 → ERROR ("Can not have margin with >2 groups")
IF npr == 2 AND min(pr) == max(pr) AND margin == NULL → ERROR ("Event probabilities can not be equal")
IF ngroups specified and != npr → WARNING (ngroups is ignored, use npr)

// Old syntax traps:
IF ni2 == 0 → ERROR "use new syntax"
IF ni specified → ERROR "use margin() syntax"
IF distant specified → ERROR "distant is default"

// Onesided handling:
IF onesided → onesided = 1; sided = "one"
ELSE → onesided = 0; sided = "two"

// ccorrect handling:
IF ccorrect → ccorrect = 1
ELSE → ccorrect = 0

// NI + nchi warning (nchi is undocumented, dropped in R)

// Error checks for option combinations:
IF local AND wald → ERROR
IF condit AND wald → ERROR
IF wald AND nvmethod not in {NULL, 1} → ERROR
IF wald AND nvmethod == NULL → nvmethod = 1
IF nvmethod == NULL → nvmethod = 3
IF local AND nvmethod != 3 → ERROR
IF niss AND condit → ERROR ("Can not select conditional for NI/substantial-sup")
IF npr == 2 AND trend → ERROR
IF npr == 2 AND doses → ERROR
IF condit AND NOT local → WARNING "As conditional has been selected local will be used"; local = TRUE
IF npr == 2 AND margin == 0 AND condit AND ccorrect → ERROR

// LTFU handling:
IF ltfu given → obsfrac = 1 - ltfu
ELSE → obsfrac = 1
IF n > 0 → noround = TRUE; ntotal = n; IF ltfu given: n = round(ntotal * obsfrac)
```

### 2. 2-arm favourable/unfavourable inference (when npr == 2)

```
IF margin == NULL → margin = 0
threshold = pr[1] + margin

IF favourable == NULL AND unfavourable == NULL:
    IF pr[2] < threshold → trialoutcome = "unfavourable"
    IF pr[2] > threshold → trialoutcome = "favourable"
    IF pr[2] == threshold → ERROR "p2 can not equal p1 + margin"
ELSE:
    trialoutcome = user-specified

// Consistency check:
IF trialoutcome == "unfavourable" AND threshold < pr[2] AND NOT force → ERROR
IF trialoutcome == "favourable" AND threshold > pr[2] AND NOT force → ERROR

// Determine trial type:
IF trialoutcome == "unfavourable" AND margin > 0 OR trialoutcome == "favourable" AND margin < 0:
    trialtype = "non-inferiority"
ELSE IF trialoutcome == "unfavourable" AND margin < 0 OR trialoutcome == "favourable" AND margin > 0:
    trialtype = "substantial-superiority"
ELSE:
    trialtype = "superiority"
```

### 3. Calculation routing

```
IF npr == 2 AND condit == FALSE:
    // 2-arm path via art2bin
    result = art2bin(pr[1], pr[2], margin=margin, power/n, ar=aratios, alpha=alpha,
                     nvmethod=nvmethod, onesided=onesided, ccorrect=ccorrect,
                     local=local, wald=wald, noround=TRUE)
    n_unrounded = result$n   // total unrounded

ELSE:
    // K-group path
    result = artbin_kgroup(pr, aratios, alpha, power/n, onesided, local, condit,
                           wald, trend, doses, nvmethod, noround, ...)
    n_unrounded = result$n   // total unrounded
```

### 4. Rounding (always done here, after calculation)

```
// Normalize allocation ratios
IF aratios == NULL → allr = rep(1, npr)
ELSE allr = aratios
// If only 1 ratio given for 2-arm: allr = c(1, allr[1])
// Rescale so allr[1] == 1
IF allr[1] != 1: allr = allr / allr[1]
totalallr = sum(allr)

nbygroup = n_unrounded / totalallr

FOR each arm a in 1..npr:
    IF calculating SS:
        IF noround:
            n[a] = nbygroup * allr[a] / obsfrac
        ELSE:
            n[a] = ceil(nbygroup * allr[a] / obsfrac)
    ELSE (power calc):
        n[a] = ntotal * allr[a] / totalallr

    D[a] = n[a] * pr[a] * obsfrac

ntotal = sum(n)
D_total = sum(D)
```

### 5. Return values

```
return list(
    n = ntotal,
    n1 = n[1], n2 = n[2], ...,   // per-arm
    power = power_calculated_or_designed,
    D = D_total,
    D1 = D[1], D2 = D[2], ...    // per-arm events
)
```

---

## Internal Function: art2bin(p0, p1, margin, n, n0, n1, ar, alpha, power, nvmethod, onesided, ccorrect, local, wald, noround)

### 1. Allocation ratio

```
IF ar == NULL or ar == 1 or ar == c(1,1): ar10 = 1, allocr = "equal group sizes"
ELSE IF length(ar) == 1: ar10 = ar, allocr = "1:ar"
ELSE IF length(ar) == 2: ar10 = ar[2]/ar[1], allocr = "ar[1]:ar[2]"
```

### 2. Null hypothesis event probabilities

```
IF nvmethod == 1: p0null = p0; p1null = p1
IF nvmethod == 2 (fixed marginal totals):
    p0null = (p0 + ar10*p1 - ar10*mrg) / (1 + ar10)
    p1null = (p0 + ar10*p1 + mrg) / (1 + ar10)
    check p0null, p1null in (0,1)
IF nvmethod == 3 (constrained ML, default):
    // Solve cubic equation
    a = 1 + ar10
    b = mrg*(ar10+2) - 1 - ar10 - p0 - ar10*p1
    c = (mrg - 1 - ar10 - 2*p0)*mrg + p0 + ar10*p1
    d = p0*mrg*(1-mrg)
    v = (b/(3a))^3 - (bc)/(6a^2) + d/(2a)
    u = sign(v) * sqrt((b/(3a))^2 - c/(3a))
    cos_val = v/u^3  (or 0 if both v and u^3 are near 0)
    w = (pi + acos(cos_val)) / 3
    p0null = 2*u*cos(w) - b/(3a)
    p1null = p0null + mrg
```

### 3. Key quantities

```
D_diff = |p1 - p0 - mrg|
za = qnorm(1 - alpha/2)   [two-sided; qnorm(1-alpha) if onesided]
zb = qnorm(power)
snull = sqrt(p0null*(1-p0null) + p1null*(1-p1null)/ar10)
salt  = sqrt(p0*(1-p0) + p1*(1-p1)/ar10)
```

### 4. Sample size calculation

```
Default (score test, distant):
    m = ((za*snull + zb*salt) / D_diff)^2

Local alternative:
    m = ((za*snull + zb*snull) / D_diff)^2

Wald test:
    m = ((za*salt + zb*salt) / D_diff)^2

Continuity correction:
    a_cc = (ar10 + 1) / (D_diff * ar10)
    cf = ((1 + sqrt(1 + 2*a_cc/m))^2) / 4
    m = m * cf

n0 = m (or ceil(m) if rounding), n1 = ar10*m (or ceil(ar10*m))
n = n0 + n1
Dart = n0*p0 + n1*p1
```

### 5. Power calculation

```
Continuity correction deflation:
    a_cc = (ar10 + 1) / (D_diff * ar10)
    n0_adj = ((2*n0 - a_cc)^2) / (4*n0)

Default (score, distant):
    Power = pnorm((D_diff*sqrt(n0_adj) - za*snull) / salt)

Local:
    Power = pnorm((D_diff*sqrt(n0_adj) - za*snull) / snull)

Wald:
    Power = pnorm((D_diff*sqrt(n0_adj) - za*salt) / salt)
```

---

## Internal Function: artbin_kgroup(pr, aratios, ...)

### Setup

```
K = length(pr) - 1
PI = pr  (vector)
AR = normalize(aratios) so sum(AR) = 1
pibar = sum(PI * AR)   (weighted mean probability)
s = pibar*(1-pibar)
MU = PI - pibar        (deviations from weighted mean)
S = PI*(1-PI)          (arm-level variances)
sbar = sum(S * AR)     (weighted mean variance)
```

### Unconditional test, chi-square, local OR wald (simple NCP formula)

```
q0 = sum(MU^2 * AR) / s

critical_a = qchisq(1 - alpha, K)
beta = 1 - power

IF calculating SS:
    lambda = npnchi2(K, critical_a, beta)  // find ncp such that pchisq(critical_a, K, ncp) = beta
    n = lambda / q0

IF calculating power:
    beta = pchisq(critical_a, K, ncp = n * q0)
    power = 1 - beta
```

### Unconditional test, chi-square, distant (iterative, default)

```
// Additional quantities:
W = 1 - 2*AR
a0 = (sum(S) - sbar) / s                 // unweighted sum of S minus weighted mean
q1 = sum(MU^2 * S * AR) / s^2
a1 = (sum(S^2 * W) + sbar^2) / s^2      // where W = 1 - 2*AR

_pe2(a0, q0, a1, q1, K, n, critical_a):
    b0 = a0 + n*q0
    b1 = a1 + 2*n*q1
    l = b0^2 - K*b1
    f = sqrt(l*(l + K*b1))
    l = (l + f) / b1
    f = critical_a * (K + l) / b0
    beta = pchisq(f, K, ncp = l)

IF calculating SS (iterative bisection):
    n0 = npnchi2(K, critical_a, beta) / q0   // starting point
    solve _pe2(n) = beta iteratively with bisection until |b - beta| < convcrit

IF calculating power:
    beta = _pe2(a0, q0, a1, q1, K, n, critical_a)
```

### Trend test (linear)

```
// Doses: default = 0, 1, ..., K (centered by weighted mean)
DOSE = doses - sum(doses * AR)   // centered doses

tr  = sum(MU * DOSE * AR)        // _sp MU DOSE AR
q0  = sum(DOSE^2 * AR) * s      // _sp DOSE DOSE AR * s

IF local:
    q1 = q0
ELSE:
    q1 = sum(DOSE^2 * S * AR)   // _sp DOSE DOSE S AR

// Critical value adjustment for trend test:
IF wald: a_crit = sqrt(q1) * qnorm(1 - alpha/2)
ELSE:    a_crit = sqrt(q0) * qnorm(1 - alpha/2)

IF SS:
    a_crit = a_crit + sqrt(q1) * qnorm(power)
    n = (a_crit / tr)^2

IF power:
    z = |tr| * sqrt(n) - a_crit
    beta = 1 - pnorm(z / sqrt(q1))
```

### Conditional test (Peto's log-odds ratio approximation)

```
v = pibar * (1 - pibar)
LOR = log(PI/(1-PI)) - log(PI[1]/(1-PI[1]))   // log odds ratio relative to group 1
LOR[1] = 0
LOR = LOR - sum(LOR * AR)                      // center

// Chi-square test:
q0 = sum(LOR^2 * AR)
critical_a = qchisq(1 - alpha, K)

IF SS:
    lambda = npnchi2(K, critical_a, beta)
    d = lambda
    l = sqrt(d * (d - 4*q0*v))
    d = (d + l) / (2*q0*(1-pibar))
    n = d / pibar

IF power:
    d = n * pibar
    l = d*(n-d)*q0 / (n-1)
    beta = pchisq(critical_a, K, ncp = l)
```

---

## Utility: npnchi2(df, x, p)

Find ncp (non-centrality parameter) such that pchisq(x, df, ncp) = p.

```
Solve via uniroot:
    f(ncp) = pchisq(x, df, ncp) - p = 0
    Search interval: [0, upper] where upper grows until f(upper) < 0
```
