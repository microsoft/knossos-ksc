import time
import torch

from ts2ks import ts2mod

torch.set_default_dtype(torch.float64)

class time_sampler:
    def __init__(self, minimizing = False):
        self.minimizing = minimizing
        if self.minimizing:
            self.time = 1e10
        else:
            self.time = 0       
            self.ncalls = 0

    def duration(self):
        if self.minimizing:
            return self.time
        else:
            return self.time / self.ncalls

    @property
    def us(self):
        return self.duration() * 1e6

    @staticmethod
    def get_time():
        return time.time_ns() * 1e-9

    def mark(self):
        self.start = time_sampler.get_time()

    def record(self):
        delta = time_sampler.get_time() - self.start
        if self.minimizing:
            self.time = min(delta, self.time)
        else:
            self.time += delta
            self.ncalls += 1

def timeit(msg, fn, config):
    print('Timing: ', msg, fn, config.shape)
    inference_timer = time_sampler()
    forward_timer = time_sampler()
    backward_timer = time_sampler()
    nruns = 50
    for _ in range(nruns):
        config.requires_grad = False
        inference_timer.mark()
        loss = config.sum()
        inference_timer.record()

        config.requires_grad = True
        forward_timer.mark()
        loss = config.sum()
        forward_timer.record()

        backward_timer.mark()
        loss.backward()
        csum = config.grad.sum()
        backward_timer.record()

    print(f'{msg:20} {csum:.6e} Inference: {inference_timer.us:10.3f} us | Forward: {forward_timer.us:10.3f} us | Backward {backward_timer.us:10.3f} us | {config.shape}')

def bench(module_name, bench_name):
  import inspect
  import importlib

  mod = importlib.import_module(module_name)
  for fn in inspect.getmembers(mod, inspect.isfunction):
    fn_name, fn_obj = fn
    if fn_name == bench_name + '_bench_configs':
      configs = list(fn_obj())
    elif fn_name == bench_name + '_pt':
      pt_fun = fn_obj
    elif fn_name == bench_name:
      ks_fun = fn_obj
    else:
      print(f"Ignoring {fn_name}")

  # TODO: vmap  
  # ks_compiled = ts2mod(ks_fun, configs[0])

  for config in configs:
    print("--", config.shape)
    timeit(bench_name + ' PT', pt_fun, config)
    timeit(bench_name + ' KS Un-opt', ks_fun, config)

if __name__ == "__main__":
  bench("relu3", "vrelu3")
