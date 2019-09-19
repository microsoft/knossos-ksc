#include <vector>
#include <iostream>
extern bool ctrl_c();
using std::vector;
struct Vec : vector<double> {};
struct Mat : vector<Vec>{ 
  static Mat rand(int,int); 
};
Vec conv(Mat,Vec);
Vec operator*(Mat,Vec);
vector<Mat> operator*(double, vector<Mat> const&){}
vector<Mat> operator-=(vector<Mat>&, vector<Mat> const&){}

struct reverse : vector<Mat> {
    reverse(vector<Mat> v); // : vector<Mat> { .. reverse_iterator ... }
};

double sqnorm(Vec a) { 
    return dot(a,a);
}
double sqdiff(Vec a, Vec b) { 
    return sqnorm(a-b);
}
double relu(double x) {
    return x > 0.0 ? x : 0.0;
}
Vec relu(Vec x) {
    return transform(x, relu);
}

// conv(M,x) is convolution

// Vec and Mat are vector and matrix from some numerical library

// Fully connected Deep Neural Network, input as 1D Vector
Vec dnn(vector<Mat> weights, Vec x) {
    Vec out = x;
    for(auto W: weights)
        out = relu(W * out);                   // relu(x) = max(x,0)
    return out;
}

// Loss function over training set "examples" for DNN given by "weights"
double loss(vector<Vec> examples, vector<Mat> weights)
{
    double tot = 0.0;
    for(auto x: examples)
        tot += sqnorm(dnn(weights, x) - x);    // sqnorm(x) = dot(x,x)
    return tot;
}

// Gradient of loss function wrt "weights"
vector<Mat> grad_loss(vector<Vec> examples, vector<Mat> weights)
{
  // code for d_loss/d_weights

}

// Training code: return weights given examples
vector<Mat> train(vector<Vec> examples)
{
    vector<Mat> weights = 
        { rand(784,400), rand(400,30), rand(30,400), rand(400,784) };
    while (!ctrl_c()) {
        weights -= 0.001 * grad2<loss>(examples, weights);
        std::cout << "Loss = " << loss(examples, weights) << std::endl;
    }
}

// More correct training code: return weights given examples
vector<Mat> train(vector<Vec> examples)
{
    vector<Mat> weights = reverse // Reverse the list
        { rand(784,400), rand(400,30), rand(30,400), rand(400,784) };
    while (!ctrl_c()) {
        weights -= 0.001 * grad<loss>(examples, weights)[1]; // Get second entry of tuple
        std::cout << "Loss = " << loss(examples, weights) << std::endl;
    }
}



template<typename T>
void cross(
	const T* const a,
	const T* const b,
	T* out)
{
	out[0] = a[1] * b[2] - a[2] * b[1];
	out[1] = a[2] * b[0] - a[0] * b[2];
	out[2] = a[0] * b[1] - a[1] * b[0];
}

template<typename T>
T arr_max(int n, const T* const x)
{
  T m = x[0];
  for (int i = 1; i < n; i++)
  {
    if (m < x[i])
      m = x[i];
  }
  return m;
}

double sqnorm(int n, const double* x)
{
  double res = x[0] * x[0];
  for (int i = 1; i < n; i++)
    res = res + x[i] * x[i];
  return res;
}

// Forward-mode autodiff of sqnorm [manually constructed]
// Supply dx (e.g. {1,0,0,..,0}) and return d_res
double fwd_diff_sqnorm(int n, double const* x, double const* dx)
{
  double res = x[0] * x[0];
  double d_res = 2 * x[0] * dx[0];
  for (int i = 1; i < n; i++) {
    res = res + x[i] * x[i];
    d_res = d_res + 2 * x[i] * dx[i];
  }
  return d_res;
}

// Reverse-mode autodiff of sqnorm [manually constructed]
// Supply d_res, compute dx
void rev_diff_sqnorm(int n, double const* x, double d_res, double* dx_out)
{
  double res = x[0] * x[0];
  double d_res = 2 * x[0] * dx[0];
  for (int i = 1; i < n; i++) {
    res = res + x[i] * x[i];
    d_res = d_res + 2 * x[i] * dx[i];
  }
  return d_res;
}

