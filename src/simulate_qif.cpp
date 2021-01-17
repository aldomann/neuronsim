using namespace std;

#include <vector>
#include <cmath>
#include <cstdlib> // rand()
#include <ctime>   // time()

#include <Rcpp.h>
using namespace Rcpp;

//' Simulate QIF Neurons
//'
//' @param params Named vector of parameters (delta, etabar, J) to pass to the FREs.
//' @param init_state Named vector of the initial state (r, v) for the ODE system.
//' @param times Time sequence for which output is wanted; the first value of times must be the initial time.
//' @param input Input current or stimulus function of the system.
//' @export
// [[Rcpp::export]]
List simulate_qif(NumericVector params, NumericVector init_state, NumericVector times, NumericVector input) {
  // Physical parameters
  int neurons = 10000;
  int sel_neurons = 300;

  // Parse params
  double delta = params[0];
  double etabar = params[1];
  double J = params[2];

  // Parse initial state
  double r0 = init_state[0];
  double v0 = init_state[1];
  double vp = 100.0;

  // Time parameters
  double t_init = times[0];
  int n = times.size();
  double t_final = times[n - 1];

  // double h = 0.0001;
  double h = 0.01;
  int steps = int((t_final - t_init) / h);
  int refract_steps = int(1 / (vp * h));
  double tau = 0.001; // For the synaptic activation

  // Initialize eta vector
  vector<double> eta(neurons);
  srand(time(0));
  for (int n = 0; n < neurons; n++) {
    eta[n] = tan(M_PI * (((double) rand() / (RAND_MAX)) - 0.5 )) + etabar;
  }

  // Initialize mean membrane potential vector
  vector<double> v_avg(steps + 1);
  v_avg[0] = v0;

  // Initialize membrane potential matrix
  vector< vector<double> > v;
  v.resize(2, vector<double>(neurons));
  for (int n = 0; n < neurons; n++) {
    // Initial membrane potential state
    v[0][n] = v0;
  }

  // Initialize spike and synaptic activation vectors
  vector<int> spike_times(neurons, 0);
  vector<double> fire_rate(steps + 1, 0);
  vector<double> syn_act(steps + 1, 0.0);

  // Loop
  for (int i = 1; i < steps + 1; i++) {
    for (int n = 0; n < neurons; n++) {
      if(spike_times[n] == 0 && v[0][n] >= vp) {
        // Save time i and v(t = i) value
        spike_times[n] = i;
      } else if (spike_times[n] == 0) {
        // Normal evolution
        v[1][n] = v[0][n] + h * (pow(v[0][n], 2) + input[i - 1] + eta[n] + J * syn_act[i - 1]);
      } else if (i < spike_times[n] + 2 * refract_steps) {
        // Spikes
        if (i >= spike_times[n] + refract_steps) {
          // Post spike
          v[1][n] = -vp;
          fire_rate[i] += 0.01;
          // if (n < sel_neurons && i == spike_times[n] + refract_steps + 1){ // Only consider selected neurons
          //   // raster_file << double(i) * h << " " << n + 1 << endl ;
          // }
          if (i < spike_times[n] + refract_steps + int(tau / h)) {
            syn_act[i] += 1 / (tau * neurons);
          }
        } else {
          // Pre spike
          v[1][n] = vp;
        }
      } else if (i > spike_times[n] + 2 * refract_steps) {
        // Reset refractory time
        spike_times[n] = 0;
      }


      // Reset matrix
      v[0][n] = v[1][n];
      v_avg[i] += v[1][n] / neurons;
    }
  }

  // Return object
  List Results = List::create(v_avg, fire_rate, times, input);

  return Results;
}
