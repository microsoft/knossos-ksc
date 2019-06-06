
#include "knossos.h"

namespace ks {
#ifdef BUMPY
	allocator g_alloc{ 1'000'000'000 };
#endif

	int log_indent = 8;
	bool do_log = false;
	std::set<void*> objects;

	struct timer_t {
		typedef std::chrono::high_resolution_clock clock_t;
		typedef std::chrono::time_point<clock_t> time_t;
		time_t t0;

		timer_t() { reset(); }

		void reset()
		{
			t0 = clock_t::now();
		}

		double operator()() {
			auto t1 = clock_t::now();
			std::chrono::duration<double> elapsed_seconds = t1 - t0;
			return elapsed_seconds.count();
		}
	};

	double benchmark(std::function<void(int)> f)
	{
		std::cerr << "Start timing -- calibration ... ";
		timer_t timer;
		int n_repeats = 1;
		double t;
		while (n_repeats < (1 << 30)) {
			timer.reset();
			f(n_repeats);
			t = timer();
			if (t > 0.5) {
				std::cerr << ", " << t << " sec at nruns = " << n_repeats << std::endl;
				break;
			}
			std::cerr << ".";
			n_repeats *= 2;
		}
		// if n_repeats > 1, we took at most 1 sec to do that number.
		// Now spend 4 more seconds at most in sampling.
		double time_to_sample = 4.0;
		int n_samples = int(time_to_sample / t);

		// Report minimum cost.  
		// Most timing noise today is positive, except for clock resolution.
		// I don't believe there's a machine with std::chrono and a resolution worse than 1ms.
		// Worst possible case is old Windows machines at 15ms, ie. 1.5% at 1 second.
		double per_call_min = t / n_repeats;

		// Do a few more runs up to time_to_sample sec
		timer_t total;
		for (int i = 0; i < n_samples; ++i) {
			std::cerr << "Start timing sample " << i << "/" << n_samples << " or " << total() << "/" << time_to_sample << " sec... ";

			timer.reset();
			f(n_repeats);
			double per_call = timer() / n_repeats;
			per_call_min = std::min(per_call, per_call_min);
			std::cerr << "Each call: " << per_call * 1000 << " msec.\n";
			if (total() > time_to_sample) {
				break;
			}
		}
		
		std::cerr << "Per-call min: " << per_call_min*1e6 << " usec.\n";
		return per_call_min * 1e6;
	};

};
