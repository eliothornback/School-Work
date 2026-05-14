//Name:Eliot Hornback
//Newton-Raphson method of finding roots
//Initial guess: -3, -1.9, 0, 1.9, 3
//Found 5 roots:
//-2.61298
//-1.9175
//0
//1.9175
//2.61298

#include <iostream>
#include <cmath>
#include <limits>

int main () {

  double x_i ;                  //x old 
  double x_n ;                  //x new
  double f_i ;                  //f old 
  double fp_i ;                 //fprime old
  double epsilon ;             

  int niter = 1 ;               //number of iterations
  const int NITER_MAX = 300 ;   //max number of iterations
  bool converged ;

  
  std::cout << "Enter your guess at a root:" ;
  std::cin >> x_i ;
  std::cout << "Enter a value for epsilon:"  ;
  std::cin >> epsilon ; 

  while (niter < NITER_MAX) {
    //evaluate f estimate 
    f_i = x_i + 3.0 * sin(2.0 * x_i) ; 

    //evaluate fp estimate
    fp_i = 1.0 + 6.0 * cos(2.0 * x_i) ;

    if (abs(fp_i) < 1.0) {
      if(abs(f_i) > abs(fp_i) * std::numeric_limits<double>::max()) {
	std::cout << "f' is approx.  0 so method overflows" ;
	return 1 ;
      }
    }
    
    x_n = x_i - f_i / fp_i ;

    if (x_i != 0.0) {
      converged = abs(x_n-x_i) < epsilon * abs(x_i) ;
    } else {
      converged = abs(x_n-x_i) < epsilon ;  
    }

    if (converged) {
      std::cout << "Root is:" << x_n << std::endl ; 
      return 2 ; 
    }
    x_i = x_n ; 
    niter = niter + 1 ;
  }  
  std::cout << "Method failed due to too many iterations" << std::endl ; 
  return 0 ; 
}
