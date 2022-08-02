# Gadget2to3: Translate gadget2 model files into a gadget3 script

## Quick Start

Compile a g3 model based on a gadget2 directory:

```
actions <- eval(g2to3_mainfile('06-ling'))
model_fn <- g3_to_r(actions)
eval(g2to3_params_r('06-ling', 'params.in'))
result <- model_fn(params.in)
```

To generate a standalone script containing a g3 model based on a gadget2 directory:

```
g2to3_script('06-ling', 'g3-ling')
source('g3-ling/run.R', chdir = T, echo = T)
```
