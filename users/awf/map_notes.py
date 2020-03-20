from math import sin,cos

# Turn function f: S->T into function f':(S, dT) -> T
# e.g.
#  sin1 = mul(sin)
# is as if we had written
#  def sin1(x, dx):
#     return sin(x) * dx
def mul(f):
    return lambda x, d: f(x)*d

global_func_table = {
    sin: mul(cos), # Or namedTuple("rev", lmScale(cos), "fwd", ..., "grad", lmScale(cos), ...)
    cos: mul(sin)
    }

# Define a new function
def my_f(x):
    return x*sin(x)

# And its derivative 
def my_f_grad(x):
    return x*cos(x) + sin(x)

# And register the derivative...
global_func_table[my_f] = mul(my_f_grad)

print(global_func_table)
# map :: FPtr{S->T} -> (Env, Vec S) -> Vec T
# rev$map :: FPtr{S->T} -> (Env, Vec S, Vec dT) -> (dEnv, Vec dS)
# fwd$map :: FPtr{S->T} -> (Env, Vec S, dEnv, Vec dS) -> Vec dT
# grad$map :: FPtr{S->T} -> LM (dEnv, Vec dS) (Vec dT)

# map: (f, (Env, [S])) -> [T]
def map(f, ess):
    (e,ss) = ess
    return [f(e,s) for s in ss]

# rev$map : (f, (Env, [S]), dT) -> (dEnv, [dS]) 
def rev_map(f, ess, dts):
    (e,ss) = ess
    # f : (Env, S) -> T 
    rev_f = global_func_table[f]
    # rev_f : ((Env, S), dT) -> (dEnv, dS)
    out_dss = []
    out_de = 0
    for (s,dt) in zip(ss,dts):
        de, ds = rev_f((e,s), dt)
        out_dss += [ds] # list concatenation
        out_de += de # addition

    return (out_de, out_dss)

drop1st = lambda f: lambda v,x: f(x)
def rev_drop1st(f, )
xs = [1.1*x for x in range(5)]
map(drop1st(sin), ((), xs))
rev_map(drop1st(sin), ((), xs))
