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
  double v0 = -2.0, vp = 100.0;
  double I0 = 3.0;
  double J = 15;

  // Time parameters
  double t_final = 5, t_init = 0.0;
  double h = 0.0001;
  int steps = int((t_final - t_init)/h);
  double refract_steps = int(1/(vp * h));
  double tau = 0.001; // For the synaptic activation

  // Test
  NumericVector v1 = times * 2;
  NumericVector v2 = times * 3;

  List L = List::create(v1, v2);

  return L;
}
